/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2022 The Stockfish developers (see AUTHORS file)

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <sstream>

#include "misc.h"
#include "movegen.h"
#include "position.h"
#include "search.h"
#include "thread.h"
#include "timeman.h"
#include "uci.h"
#include "syzygy/tbprobe.h"

namespace Search {

  LimitsType Limits;
}

namespace Tablebases {

  int Cardinality;
  bool RootInTB;
  bool UseRule50;
  Depth ProbeDepth;
}

namespace TB = Tablebases;

using std::string;
using namespace Search;

namespace {

  bool onlyChecks;
  int allMoves, kingMoves;

  Value search(Position& pos, Stack* ss, Value alpha, Value beta, Depth depth);

  // perft() is our utility to verify move generation. All the leaf nodes up
  // to the given depth are generated and counted, and the sum is returned.
  template<bool Root>
  uint64_t perft(Position& pos, Depth depth) {

    StateInfo st;
    uint64_t cnt, nodes = 0;
    const bool leaf = (depth == 2);

    for (const auto& m : MoveList<LEGAL>(pos))
    {
        if (Root && depth <= 1)
            cnt = 1, nodes++;
        else
        {
            pos.do_move(m, st);
            cnt = leaf ? MoveList<LEGAL>(pos).size() : perft<false>(pos, depth - 1);
            nodes += cnt;
            pos.undo_move(m);
        }
        if (Root)
            sync_cout << UCI::move(m, pos.is_chess960()) << ": " << cnt << sync_endl;
    }

    return nodes;
  }

} // namespace


/// Search::clear() resets search state to its initial value

void Search::clear() {

  Threads.main()->wait_for_search_finished();

  Threads.clear();
  Tablebases::init(Options["SyzygyPath"]); // Free mapped files
}


/// MainThread::search() is started when the program receives the UCI 'go'
/// command. It searches from the root position and outputs the "bestmove".

void MainThread::search() {

  // Special case 1: 'go perft x' 
  if (Limits.perft)
  {
      nodes = perft<true>(rootPos, Limits.perft);
      sync_cout << "\nNodes searched: " << nodes << "\n" << sync_endl;

      return;
  }

  // Special case 2: no move(s) to search
  if (rootMoves.empty())
  {
      // Must be mate or stalemate
      sync_cout << "info depth 0 score "
                << UCI::value(rootPos.checkers() ? -VALUE_MATE : VALUE_DRAW)
                << std::endl;
      std::cout << "bestmove " << UCI::move(MOVE_NULL, rootPos.is_chess960()) << sync_endl;

      return;
  }

  Time.init(Limits, rootPos.side_to_move(), rootPos.game_ply());

  // Read UCI options
  onlyChecks = Options["ChecksOnly"];
  kingMoves  = Options["KingMoves"];
  allMoves   = Options["AllMoves"];

  for (Thread* th : Threads)
      if (th != this)
          th->start_searching();

  Thread::search(); // Let's start searching!

  while (!Threads.stop && Limits.infinite)
  {} // Busy wait for a stop

  // Wait until all threads have finished
  for (Thread* th : Threads)
      if (th != this)
          th->wait_for_search_finished();

  // Stop the threads if not already stopped
  Threads.stop = true;

  sync_cout << "info string All threads finished!" << sync_endl;

  Thread* bestThread = this;

  for (Thread* th : Threads)
      if (  !th->rootMoves.empty()
          && th->rootMoves[0].score > bestThread->rootMoves[0].score)
          bestThread = th;

  // Send PV info
//  sync_cout << UCI::pv(bestThread->rootPos, bestThread->completedDepth, -VALUE_INFINITE, VALUE_INFINITE) << sync_endl;

  // Send best move and ponder move (if available)
  sync_cout << "bestmove " << UCI::move(bestThread->rootMoves[0].pv[0], bestThread->rootPos.is_chess960());

  if (bestThread->rootMoves[0].pv.size() > 1)
      std::cout << " ponder " << UCI::move(bestThread->rootMoves[0].pv[1], bestThread->rootPos.is_chess960());

  std::cout << sync_endl;
}


/// Thread::search() is the main iterative deepening loop. It calls search()
/// repeatedly with increasing depth until the allocated thinking time has been
/// consumed, the user stops the search, or the maximum search depth is reached.

void Thread::search() {

  Stack stack[MAX_PLY+1], *ss = stack;
  StateInfo rootSt;
  Value alpha, beta, bestValue, value;

  for (int i = 0; i <= MAX_PLY; ++i)
      (ss+i)->ply = i;
  ss->pv.clear();

  Color us = rootPos.side_to_move();
  Depth targetDepth = Limits.mate ? 2 * Limits.mate - 1 : MAX_PLY;
  size_t multiPV = rootMoves.size();
  rootDepth = 1;

  // 
  alpha = VALUE_MATE - 2 * Limits.mate;
  beta  = VALUE_INFINITE;

  const auto king = rootPos.square<KING>(~us);

  // Now we rank the root moves for the mate search.
  for (auto& rm : rootMoves)
  {
      // R-Mobility (kind of ?)
      rootPos.do_move(rm.pv[0], rootSt);
      rm.tbRank -= (rootPos.checkers() ? 20 : 16) * int(MoveList<LEGAL>(rootPos).size());
      rootPos.undo_move(rm.pv[0]);

      if (rootPos.gives_check(rm.pv[0]))
          rm.tbRank += 2000 - 10 * distance(king, to_sq(rm.pv[0])); // Top priority!

      else if (rootPos.capture(rm.pv[0]))
          rm.tbRank += 200; // TODO MVV/LVA

      else
          rm.tbRank += 20 * relative_rank(us, to_sq(rm.pv[0]));
  }

  std::stable_sort(rootMoves.begin(), rootMoves.end(),
     [](const RootMove &rm1, const RootMove &rm2) { return rm1.tbRank > rm2.tbRank; });


  while (rootDepth <= targetDepth)
  {
      // Save the last iteration's scores before first PV line is searched and
      // all the move scores except the (new) PV are set to -VALUE_INFINITE.
      for (RootMove& rm : rootMoves)
      {
          rm.previousScore = rm.score;
          rm.score = -VALUE_INFINITE;
      }

      bestValue = -VALUE_INFINITE;

      for (pvIdx = 0; pvIdx < multiPV; ++pvIdx)
      {
          if (Time.elapsed() > 2000)
              sync_cout << "info depth " << rootDepth
                        << " currmove "  << UCI::move(rootMoves[pvIdx].pv[0], rootPos.is_chess960())
                        << " currmovenumber " << pvIdx + 1 << sync_endl;

          if (   onlyChecks
              && rootMoves[pvIdx].tbRank < 1000)
              continue;

          // Make, search and undo the root move
          rootPos.do_move(rootMoves[pvIdx].pv[0], rootSt);

          value = -::search(rootPos, ss+1, -beta, -alpha, rootDepth-1);

          rootPos.undo_move(rootMoves[pvIdx].pv[0]);

          if (value > bestValue)
              bestValue = value;

          // Assign the search value, selective search depth
          // and the pv to this root move.
          rootMoves[pvIdx].score = value;
          rootMoves[pvIdx].selDepth = rootDepth;
          rootMoves[pvIdx].pv.resize(1);

          // Append child pv
          for (auto& m : (ss+1)->pv)
              rootMoves[pvIdx].pv.push_back(m);

          // Sort the PV lines searched so far
          std::stable_sort(rootMoves.begin(), rootMoves.begin() + pvIdx + 1);

          // Have we found a "mate in x"?
          if (bestValue >= alpha)
          {
              Threads.stop = true;
              sync_cout << "info string Thread " << id() + 1 << ": Success! Mate in " << (rootDepth + 1) / 2 << " found!" << sync_endl;
              sync_cout << UCI::pv(rootPos, rootDepth, alpha, beta) << sync_endl;
          }

          if (Threads.stop.load())
              break;
      }

      completedDepth = rootDepth;

      if (Threads.stop.load())
          break;

      // Inform the user that we haven't found a mate
      sync_cout << "info string Thread " << id() + 1 << ": No mate in " << (completedDepth + 1) / 2 << " found" << sync_endl;

      rootDepth += 2;
  }

}


namespace {

  // search<>() is the main search function

  Value search(Position& pos, Stack* ss, Value alpha, Value beta, Depth depth) {

    StateInfo st;
    Value bestValue, value;
    bool inCheck = !!pos.checkers();
    int moveCount;
    Color us = pos.side_to_move();

    assert(alpha < beta);

    // Start with a fresh pv
    ss->pv.clear();

    // Check for aborted search or maximum ply reached
    if (   Threads.stop.load()
        || ss->ply == MAX_PLY)
        return VALUE_ZERO;

    // At the leafs, we simply either return a mate score
    // or zero. No evaluation needed!
    if (depth == 0)
    {
        if (inCheck && !MoveList<LEGAL>(pos).size())
            return mated_in(ss->ply);
        else
            return VALUE_DRAW;
    }

    if (ss->ply & 1)
    {
        if (   kingMoves < 8
            && int(MoveList<LEGAL, KING>(pos).size()) > kingMoves)
            return VALUE_DRAW;

        if (   allMoves < 250
            && int(MoveList<LEGAL>(pos).size()) > allMoves)
            return VALUE_DRAW;
    }

    // Check for draw by repetition and 50-move rule
    if (pos.is_draw(ss->ply))
        return VALUE_DRAW;

    // TODO: Tablebase probe
    // For the root color we can immediately return on
    // TB draws or losses

    bestValue = -VALUE_INFINITE;
    moveCount = 0;
    auto rankThisMove = 0;

    std::vector<RankedMove> legalMoves;
    legalMoves.reserve(64); // avoid reallocations

    const auto king = pos.square<KING>(~us); // Square of the opponent's king

    for (const auto& m : MoveList<LEGAL>(pos))
    {
        if (pos.gives_check(m))
            rankThisMove += 2000 - 100 * distance(king, to_sq(m)); // Top priority!

        else if (pos.capture(m))
            rankThisMove += 200; // TODO MVV/LVA

        else
            rankThisMove += 20 * relative_rank(us, to_sq(m));

        // Add this ranked move
        legalMoves.emplace_back(RankedMove(m, rankThisMove));

        rankThisMove = 0; // Reset for the next move
    }

    // Search all legal moves
    while (!legalMoves.empty())
    {
        moveCount++;

        // Pick the next best move
        auto rm = std::max_element(legalMoves.begin(), legalMoves.end(),
                                [](const RankedMove& a, const RankedMove& b) { return b.rank > a.rank; });

        // At frontier nodes we can skip all non-checking moves
        if (   depth == 1
            && (*rm).rank < 1000)
            break;

        if (   onlyChecks
            && !(ss->ply & 1)
            && (*rm).rank < 1000)
            break;

        pos.do_move((*rm).move, st);
        value = -search(pos, ss+1, -beta, -alpha, depth-1);
        pos.undo_move((*rm).move);

        // Do we have a new best value?
        if (value > bestValue)
        {
            // Beta-cutoff?
            if (value >= beta)
                return value;

            bestValue = value;

            if (value > alpha)
            {
                // Update alpha
                alpha = value;

                // Reset PV and insert current best move
                ss->pv.clear();
                ss->pv.push_back((*rm).move);

                // Append child pv
                for (auto& m : (ss+1)->pv)
                    ss->pv.push_back(m);
            }
        }

        // Delete this move from the list
        legalMoves.erase(rm);
    }

    // No moves? Must be Mate or Stalemate!
    if (!moveCount)
        bestValue = inCheck ? mated_in(ss->ply) // Checkmate!
                            : VALUE_DRAW;       // Stalemate!

    assert(-VALUE_INFINITE <= bestValue && bestValue < VALUE_INFINITE);

    return bestValue;
  }

} // namespace


/// MainThread::check_time() is used to print debug info and, more importantly,
/// to detect when we are out of available time and thus stop the search.

void MainThread::check_time() {

  if (--callsCnt > 0)
      return;

  // When using nodes, ensure checking rate is not lower than 0.1% of nodes
  callsCnt = Limits.nodes ? std::min(1024, int(Limits.nodes / 1024)) : 1024;

  static TimePoint lastInfoTime = now();

  TimePoint elapsed = Time.elapsed();
  TimePoint tick = Limits.startTime + elapsed;

  if (tick - lastInfoTime >= 1000)
  {
      lastInfoTime = tick;
      dbg_print();
  }

  if (   (Limits.use_time_management() && elapsed > Time.maximum() - 10)
      || (Limits.movetime && elapsed >= Limits.movetime)
      || (Limits.nodes && Threads.nodes_searched() >= (uint64_t)Limits.nodes))
      Threads.stop = true;
}


/// UCI::pv() formats PV information according to the UCI protocol. UCI requires
/// that all (if any) unsearched PV lines are sent using a previous search score.

string UCI::pv(const Position& pos, Depth depth, Value alpha, Value beta) {

  std::stringstream ss;
  TimePoint elapsed = Time.elapsed() + 1;
  const RootMoves& rootMoves = pos.this_thread()->rootMoves;
  size_t pvIdx = pos.this_thread()->pvIdx;
  size_t multiPV = std::min((size_t)Options["MultiPV"], rootMoves.size());
  uint64_t nodesSearched = Threads.nodes_searched();
  uint64_t tbHits = Threads.tb_hits() + (TB::RootInTB ? rootMoves.size() : 0);

  for (size_t i = 0; i < multiPV; ++i)
  {
      bool updated = rootMoves[i].score != -VALUE_INFINITE;

      if (depth == 1 && !updated)
          continue;

      Depth d = updated ? depth : depth - 1;
      Value v = updated ? rootMoves[i].score : rootMoves[i].previousScore;

      bool tb = TB::RootInTB && abs(v) < VALUE_MATE - MAX_PLY;
      v = tb ? rootMoves[i].tbScore : v;

      if (ss.rdbuf()->in_avail()) // Not at first line
          ss << "\n";

      ss << "info"
         << " depth "    << d
         << " seldepth " << rootMoves[i].selDepth
         << " multipv "  << i + 1
         << " score "    << UCI::value(v);

      if (!tb && i == pvIdx)
          ss << (v >= beta ? " lowerbound" : v <= alpha ? " upperbound" : "");

      ss << " nodes "    << nodesSearched
         << " nps "      << nodesSearched * 1000 / elapsed;

      ss << " tbhits "   << tbHits
         << " time "     << elapsed
         << " pv";

      for (Move m : rootMoves[i].pv)
          ss << " " << UCI::move(m, pos.is_chess960());
  }

  return ss.str();
}

/*
void Tablebases::rank_root_moves(Position& pos, Search::RootMoves& rootMoves) {

    RootInTB = false;
    UseRule50 = Options["Syzygy50MoveRule"];
    ProbeDepth = Options["SyzygyProbeDepth"];
    Cardinality = Options["SyzygyProbeLimit"];
    bool dtz_available = true;

    // Tables with fewer pieces than SyzygyProbeLimit are searched with
    // ProbeDepth == DEPTH_ZERO
    if (Cardinality > MaxCardinality)
    {
        Cardinality = MaxCardinality;
        ProbeDepth = 0;
    }

    if (Cardinality >= popcount(pos.pieces()) && !pos.can_castle(ANY_CASTLING))
    {
        // Rank moves using DTZ tables
        RootInTB = root_probe(pos, rootMoves);

        if (!RootInTB)
        {
            // DTZ tables are missing; try to rank moves using WDL tables
            dtz_available = false;
            RootInTB = root_probe_wdl(pos, rootMoves);
        }
    }

    if (RootInTB)
    {
        // Sort moves according to TB rank
        std::sort(rootMoves.begin(), rootMoves.end(),
                  [](const RootMove &a, const RootMove &b) { return a.tbRank > b.tbRank; } );

        // Probe during search only if DTZ is not available and we are winning
        if (dtz_available || rootMoves[0].tbScore <= VALUE_DRAW)
            Cardinality = 0;
    }
    else
    {
        // Clean up if root_probe() and root_probe_wdl() have failed
        for (auto& m : rootMoves)
            m.tbRank = 0;
    }
}*/
