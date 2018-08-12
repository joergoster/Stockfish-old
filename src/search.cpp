/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2018 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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
#include <cstring>   // For std::memset
#include <iostream>
#include <sstream>

#include "evaluate.h"
#include "misc.h"
#include "movegen.h"
#include "movepick.h"
#include "position.h"
#include "search.h"
#include "thread.h"
#include "timeman.h"
#include "tt.h"
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
using Eval::evaluate;
using namespace Search;

namespace {

  // Different node types, used as a template parameter
  enum NodeType { NonPV, PV };

  // Sizes and phases of the skip-blocks, used for distributing search depths across the threads
  constexpr int SkipSize[]  = { 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4 };
  constexpr int SkipPhase[] = { 0, 1, 0, 1, 2, 3, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 6, 7 };

  // Razor and futility margins
  constexpr int RazorMargin = 600;

  Value futility_margin(Depth d, bool improving) {
    return Value((175 - 50 * improving) * d / ONE_PLY);
  }

  // Futility and reductions lookup tables, initialized at startup
  int FutilityMoveCounts[2][16]; // [improving][depth]
  int Reductions[2][2][64][64];  // [pv][improving][depth][moveNumber]

  template <bool PvNode> Depth reduction(bool i, Depth d, int mn) {
    return Reductions[PvNode][i][std::min(d / ONE_PLY, 63)][std::min(mn, 63)] * ONE_PLY;
  }

  // History and stats update bonus, based on depth
  int stat_bonus(Depth depth) {
    int d = depth / ONE_PLY;
    return d > 17 ? 0 : 29 * d * d + 138 * d - 134;
  }

  // Global switch for Null-move search
  bool doNull;

  template <NodeType NT>
  Value search(Position& pos, Stack* ss, Value alpha, Value beta, Depth depth, bool cutNode, bool skipEarlyPruning);

  template <NodeType NT>
  Value qsearch(Position& pos, Stack* ss, Value alpha, Value beta, Depth depth = DEPTH_ZERO);

  Value value_to_tt(Value v, int ply);
  Value value_from_tt(Value v, int ply);
  void update_pv(Move* pv, Move move, Move* childPv);
  void update_continuation_histories(Stack* ss, Piece pc, Square to, int bonus);
  void update_quiet_stats(const Position& pos, Stack* ss, Move move, Move* quiets, int quietsCnt, int bonus);
  void update_capture_stats(const Position& pos, Move move, Move* captures, int captureCnt, int bonus);

  inline bool gives_check(const Position& pos, Move move) {
    Color us = pos.side_to_move();
    return  type_of(move) == NORMAL && !(pos.blockers_for_king(~us) & pos.pieces(us))
          ? pos.check_squares(type_of(pos.moved_piece(move))) & to_sq(move)
          : pos.gives_check(move);
  }

  // perft() is our utility to verify move generation. All the leaf nodes up
  // to the given depth are generated and counted, and the sum is returned.
  template<bool Root>
  uint64_t perft(Position& pos, Depth depth) {

    StateInfo st;
    uint64_t cnt, nodes = 0;
    const bool leaf = (depth == 2 * ONE_PLY);

    for (const auto& m : MoveList<LEGAL>(pos))
    {
        if (Root && depth <= ONE_PLY)
            cnt = 1, nodes++;
        else
        {
            pos.do_move(m, st);
            cnt = leaf ? MoveList<LEGAL>(pos).size() : perft<false>(pos, depth - ONE_PLY);
            nodes += cnt;
            pos.undo_move(m);
        }
        if (Root)
            sync_cout << UCI::move(m, pos.is_chess960()) << ": " << cnt << sync_endl;
    }
    return nodes;
  }

} // namespace


/// Search::init() is called at startup to initialize various lookup tables

void Search::init() {

  for (int imp = 0; imp <= 1; ++imp)
      for (int d = 1; d < 64; ++d)
          for (int mc = 1; mc < 64; ++mc)
          {
              double r = log(d) * log(mc) / 1.95;

              Reductions[NonPV][imp][d][mc] = int(std::round(r));
              Reductions[PV][imp][d][mc] = std::max(Reductions[NonPV][imp][d][mc] - 1, 0);

              // Increase reduction for non-PV nodes when eval is not improving
              if (!imp && r > 1.0)
                Reductions[NonPV][imp][d][mc]++;
          }

  for (int d = 0; d < 16; ++d)
  {
      FutilityMoveCounts[0][d] = int(2.4 + 0.74 * pow(d, 1.78));
      FutilityMoveCounts[1][d] = int(5.0 + 1.00 * pow(d, 2.00));
  }
}


/// Search::clear() resets search state to its initial value

void Search::clear() {

  Threads.main()->wait_for_search_finished();

  Time.availableNodes = 0;
  TT.clear();
  Threads.clear();
}


/// MainThread::search() is called by the main thread when the program receives
/// the UCI 'go' command. It searches from the root position and outputs the "bestmove".

void MainThread::search() {

  if (Limits.perft)
  {
      nodes = perft<true>(rootPos, Limits.perft * ONE_PLY);
      sync_cout << "\nNodes searched: " << nodes << "\n" << sync_endl;
      return;
  }

  Color us = rootPos.side_to_move();
  Time.init(Limits, us, rootPos.game_ply());
  TT.new_search();

  if (rootMoves.empty())
  {
      rootMoves.emplace_back(MOVE_NONE);
      sync_cout << "info depth 0 score "
                << UCI::value(rootPos.checkers() && (rootPos.rule50_count() <= 100 || !Options["Syzygy50MoveRule"]) ? -VALUE_MATE : VALUE_DRAW)
                << sync_endl;
  }
  else if (TB::UseRule50 && rootPos.rule50_count() >= 100)
  {
      sync_cout << "info depth 0 score "
                << UCI::value(VALUE_DRAW)
                << sync_endl;
  }
  else
  {
      for (Thread* th : Threads)
          if (th != this)
              th->start_searching();

      Thread::search(); // Let's start searching!
  }

  // When we reach the maximum depth, we can arrive here without a raise of
  // Threads.stop. However, if we are pondering or in an infinite search,
  // the UCI protocol states that we shouldn't print the best move before the
  // GUI sends a "stop" or "ponderhit" command. We therefore simply wait here
  // until the GUI sends one of those commands (which also raises Threads.stop).
  Threads.stopOnPonderhit = true;

  while (!Threads.stop && (Threads.ponder || Limits.infinite))
  {} // Busy wait for a stop or a ponder reset

  // Stop the threads if not already stopped (also raise the stop if
  // "ponderhit" just reset Threads.ponder).
  Threads.stop = true;

  // Wait until all threads have finished
  for (Thread* th : Threads)
      if (th != this)
          th->wait_for_search_finished();

  // When playing in 'nodes as time' mode, subtract the searched nodes from
  // the available ones before exiting.
  if (Limits.npmsec)
      Time.availableNodes += Limits.inc[us] - Threads.nodes_searched();

  previousScore = rootMoves[0].score;

  sync_cout << "bestmove " << UCI::move(rootMoves[0].pv[0], rootPos.is_chess960());

  if (rootMoves[0].pv.size() > 1 || rootMoves[0].extract_ponder_from_tt(rootPos))
      std::cout << " ponder " << UCI::move(rootMoves[0].pv[1], rootPos.is_chess960());

  std::cout << sync_endl;
}


/// Thread::search() is the main iterative deepening loop. It calls search()
/// repeatedly with increasing depth until the allocated thinking time has been
/// consumed, the user stops the search, or the maximum search depth is reached.

void Thread::search() {

  Stack stack[MAX_PLY+7], *ss = stack+4; // To reference from (ss-4) to (ss+2)
  Value lastBestScore, bestValue, alpha, beta, delta;
  Move currentBestMove, lastBestMove;
  Depth lastBestMoveDepth = DEPTH_ZERO;
  MainThread* mainThread = (this == Threads.main() ? Threads.main() : nullptr);
  double timeReduction = 1.0;
  Color us = rootPos.side_to_move();
  Material::Entry* me;

  std::memset(ss-4, 0, 7 * sizeof(Stack));
  for (int i = 4; i > 0; i--)
     (ss-i)->continuationHistory = &this->continuationHistory[NO_PIECE][0]; // Use as sentinel

  bestValue = alpha = -VALUE_INFINITE;
  beta = VALUE_INFINITE;
  delta = VALUE_ZERO;

  currentBestMove = lastBestMove = rootMoves[0].pv[0];

  if (mainThread)
      mainThread->bestMoveChanges = 0, mainThread->failedLow = false;

  doNull = Options["NullMove"];
  multiPV = std::min((size_t)Options["MultiPV"], rootMoves.size());

  // Probe the material hash table
  me = Material::probe(rootPos);
  double mf = 8 - 7.0 * (PHASE_MIDGAME - me->game_phase()) / PHASE_MIDGAME;

  int baseCt = Options["Contempt"] * PawnValueEg / 100; // From centipawns
  int ct = 1; // Used for changing sign of contempt when analyzing

  // In analysis mode, adjust contempt in accordance with user preference
  if (!Limits.use_time_management())
      ct =  Options["Analysis Contempt"] == "Off" ?  0
          : Options["Analysis Contempt"] == "White" && us == BLACK ? -ct
          : Options["Analysis Contempt"] == "Black" && us == WHITE ? -ct
          : ct; // contempt remains with the side to move

  // In evaluate.cpp the evaluation is from White's point of view
  baseCt *= ct;
  contempt = (us == WHITE ?  make_score(baseCt, baseCt / 2)
                          : -make_score(baseCt, baseCt / 2));

  // Iterative deepening loop until requested to stop or the target depth is reached
  while (   (rootDepth += ONE_PLY) < DEPTH_MAX
         && !Threads.stop
         && !(Limits.depth && mainThread && rootDepth / ONE_PLY > Limits.depth))
  {
      // Distribute search depths across the helper threads
      if (idx > 0)
      {
          int i = (idx - 1) % 20;
          if (((rootDepth / ONE_PLY + SkipPhase[i]) / SkipSize[i]) % 2)
              continue;  // Retry with an incremented rootDepth
      }

      // Age out PV variability metric
      if (mainThread)
          mainThread->bestMoveChanges *= 0.517, mainThread->failedLow = false;

      // Save the last iteration's scores before first PV line is searched
      // and reset all move scores to -VALUE_INFINITE.
      for (RootMove& rm : rootMoves)
          if (rm.score != -VALUE_INFINITE)
              rm.previousScore = rm.score, rm.score = -VALUE_INFINITE;

      lastBestScore = rootMoves[0].previousScore;

      if (rootDepth < 9 * ONE_PLY)
          PVLines = rootMoves.size();
      else
      {
          PVLines = 1;

          for (size_t i = 1; i < rootMoves.size(); i++)
              if (rootMoves[i].previousScore >= lastBestScore - int(int(PawnValueEg) / mf))
                  PVLines++;

          PVLines = std::max(multiPV, PVLines);
      }

      // MultiPV loop. We perform a full root search for each PV line
      for (PVIdx = 0; PVIdx < PVLines && !Threads.stop; ++PVIdx)
      {
          // Reset UCI info selDepth and bestValue for each depth and each PV line
          selDepth = 1;
          bestValue = rootMoves[PVIdx].previousScore;

          // Reset aspiration window starting size
          if (rootDepth >= 5 * ONE_PLY && abs(bestValue) < VALUE_KNOWN_WIN)
          {
              delta = Value(18);
              alpha = bestValue - delta;
              beta  = bestValue + delta;

              // Adjust contempt based on best score of the previous iteration for each PV line (dynamic contempt)
              int dynCt = baseCt + ct * 88 * bestValue / (abs(bestValue) + 200);

              contempt = (us == WHITE ?  make_score(dynCt, dynCt / 2)
                                      : -make_score(dynCt, dynCt / 2));
          }
          else
          {
              alpha = -VALUE_INFINITE;
              beta  =  VALUE_INFINITE;
          }

          // Start with a small aspiration window and, in the case of a fail
          // high/low, re-search with a bigger window until we don't fail
          // high/low anymore.
          while (true)
          {
              bestValue = ::search<PV>(rootPos, ss, alpha, beta, rootDepth, false, true);

              // Bring the best move to the front. It is critical that sorting
              // is done with a stable algorithm because all the values but the
              // first and eventually the new best one are set to -VALUE_INFINITE
              // and we want to keep the same order for all the moves except the
              // new PV that goes to the front. Note that in case of MultiPV
              // search the already searched PV lines are preserved.
              if (PVIdx + 1 == PVLines)
                  std::stable_sort(rootMoves.begin() + PVIdx, rootMoves.end());

              // If search has been stopped, we break immediately
              if (Threads.stop)
                  break;

              // When failing high/low give some update (without cluttering
              // the UI) before a re-search.
              if (   mainThread
                  && multiPV == 1
                  && PVIdx == 0
                  && (bestValue <= alpha || bestValue >= beta)
                  && Time.elapsed() > 3000)
                  sync_cout << UCI::pv(rootPos, rootDepth, alpha, beta) << sync_endl;

              // In case of failing low/high increase aspiration window and
              // re-search, otherwise exit the loop.
              if (bestValue <= alpha)
              {
                  beta = (alpha + beta) / 2;
                  alpha = bestValue - delta;

                  if (mainThread)
                  {
                      mainThread->failedLow = true;
                      Threads.stopOnPonderhit = false;
                  }
              }
              else if (bestValue >= beta)
                  beta = bestValue + delta;
              else
                  break;

              delta += delta / 4 + 5;

              // Search with full window in case we have a win/mate score
              if (abs(bestValue) >= VALUE_KNOWN_WIN)
              {
                  alpha = -VALUE_INFINITE;
                  beta  =  VALUE_INFINITE;
              }

              assert(delta >= VALUE_ZERO);
              assert(alpha >= -VALUE_INFINITE && beta <= VALUE_INFINITE);
          }

          // Best move before sorting
          if (mainThread)
              currentBestMove = rootMoves[0].pv[0];

          // Let each thread sort the PV lines searched so far. In case of stop,
          // sorting is safe because RootMoves is still valid, although it refers
          // to the previous iteration.
          std::stable_sort(rootMoves.begin(), rootMoves.begin() + PVIdx + 1);

          // Update the GUI with the new best move or the new PV line
          if (    mainThread
              && (Threads.stop || PVIdx + 1 == multiPV || rootMoves[0].pv[0] != currentBestMove))
              sync_cout << UCI::pv(rootPos, rootDepth, alpha, beta) << sync_endl;
      }

      if (!Threads.stop)
          completedDepth = rootDepth;

      if (rootMoves[0].pv[0] != lastBestMove)
         lastBestMove = rootMoves[0].pv[0], lastBestMoveDepth = rootDepth;

      if (!mainThread)
          continue;

      // Have we found a "mate in x"?
      if (   Limits.mate
          && bestValue >= VALUE_MATE_IN_MAX_PLY
          && VALUE_MATE - bestValue <= 2 * Limits.mate)
          Threads.stop = true;

      // Do we have time for the next iteration? Can we stop searching now?
      if (    Limits.use_time_management()
          && !Threads.stop
          && !Threads.stopOnPonderhit)
      {
          const int F[] = { mainThread->failedLow,
                            bestValue - mainThread->previousScore };

          int improvingFactor = std::max(246, std::min(832, 306 + 119 * F[0] - 6 * F[1]));

          // If the bestMove is stable over several iterations, reduce time accordingly
          timeReduction = 1.0;
          for (int i : {3, 4, 5})
              if (lastBestMoveDepth * i < completedDepth)
                  timeReduction *= 1.25;

          // Use part of the gained time from a previous stable move for the current move
          double bestMoveInstability = 1.0 + mainThread->bestMoveChanges;
          bestMoveInstability *= std::pow(mainThread->previousTimeReduction, 0.528) / timeReduction;

          // Stop the search if we have only one legal move, or if available time elapsed
          if (   rootMoves.size() == 1
              || Time.elapsed() > Time.optimum() * bestMoveInstability * improvingFactor / 581)
          {
              // If we are allowed to ponder do not stop the search now but
              // keep pondering until the GUI sends "ponderhit" or "stop".
              if (Threads.ponder)
                  Threads.stopOnPonderhit = true;
              else
                  Threads.stop = true;
          }
      }
  }

  if (!mainThread)
      return;

  mainThread->previousTimeReduction = timeReduction;
}


namespace {

  // search<>() is the main search function for both PV and non-PV nodes

  template <NodeType NT>
  Value search(Position& pos, Stack* ss, Value alpha, Value beta, Depth depth, bool cutNode, bool skipEarlyPruning) {

    constexpr bool PvNode = NT == PV;
    const bool rootNode = ss->ply == 0;

    assert(-VALUE_INFINITE <= alpha && alpha < beta && beta <= VALUE_INFINITE);
    assert(PvNode || (alpha == beta - 1));
    assert(DEPTH_ZERO < depth && depth < DEPTH_MAX);
    assert(!(PvNode && cutNode));
    assert(depth / ONE_PLY * ONE_PLY == depth);

    Move pv[MAX_PLY+1], capturesSearched[32], quietsSearched[64];
    StateInfo st;
    TTEntry* tte;
    Key posKey;
    Move ttMove, move, excludedMove, bestMove;
    Color us;
    Depth extension, newDepth;
    Value bestValue, value, ttValue, eval, maxValue;
    bool ttHit, inCheck, givesCheck, improving;
    bool captureOrPromotion, doFullDepthSearch, moveCountPruning, skipQuiets, ttCapture, pvExact;
    Piece movedPiece;
    int moveCount, captureCount, quietCount;

    // Step 1. Initialize node
    Thread* thisThread = pos.this_thread();
    inCheck = bool(pos.checkers());
    us = pos.side_to_move();
    moveCount = captureCount = quietCount = ss->moveCount = 0;
    bestValue = -VALUE_INFINITE;
    maxValue = VALUE_INFINITE;

    // Step 2. Check for the available remaining time
    if (thisThread == Threads.main())
        static_cast<MainThread*>(thisThread)->check_time();

    // Used to send selDepth info to GUI
    if (PvNode)
        thisThread->selDepth = std::max(ss->ply, thisThread->selDepth);

    // Step 3. Check for immediate return conditions
    if (!rootNode)
    {
        // Step 3a. Check for aborted search
        if (Threads.stop.load(std::memory_order_relaxed))
            return VALUE_ZERO;

        // Step 3b. Check for draw by 50-move rule
        if (    TB::UseRule50
            &&  pos.rule50_count() == 100
            && (!inCheck || MoveList<LEGAL>(pos).size()))
            return VALUE_DRAW;

        // Step 3c. Check for draw by repetition
        if (pos.has_repeated(false, ss->ply))
            return VALUE_REP_DRAW;

        // Step 3d. Check for maximum ply reached
        if (ss->ply >= MAX_PLY)
            return !inCheck ? evaluate(pos) : beta;

        // Step 3e. Mate distance pruning. Even if we mate at the next move our score
        // would be at best mate_in(ss->ply+1), but if alpha is already bigger because
        // a shorter mate was found upward in the tree then there is no need to search
        // because we will never beat the current alpha. Same logic but with reversed
        // signs applies also in the opposite condition of being mated instead of giving
        // mate. In this case return a fail-high score.
        alpha = std::max(mated_in(ss->ply), alpha);
        beta  = std::min(mate_in(ss->ply+1), beta);

        if (alpha >= beta)
            return alpha;
    }

    assert(0 <= ss->ply && ss->ply < MAX_PLY);

    (ss+1)->ply = ss->ply + 1;
    ss->currentMove = (ss+1)->excludedMove = bestMove = MOVE_NONE;
    ss->continuationHistory = &thisThread->continuationHistory[NO_PIECE][0];
    (ss+2)->killers[0] = (ss+2)->killers[1] = MOVE_NONE;
    Square prevSq = to_sq((ss-1)->currentMove);

    // Initialize statScore to zero for the grandchildren of the current position.
    // So statScore is shared between all grandchildren and only the first grandchild
    // starts with statScore = 0. Later grandchildren start with the last calculated
    // statScore of the previous grandchild. This influences the reduction rules in
    // LMR which are based on the statScore of parent position.
    (ss+2)->statScore = 0;

    // Step 4. Transposition table lookup. We don't want the score of a partial
    // search to overwrite a previous full search TT value, so we use a different
    // position key in case of an excluded move.
    excludedMove = ss->excludedMove;
    posKey = pos.key() ^ Key(excludedMove << 16); // Isn't a very good hash
    tte = TT.probe(posKey, ttHit);
    ttValue = ttHit ? value_from_tt(tte->value(), ss->ply) : VALUE_NONE;
    ttMove =  rootNode ? thisThread->rootMoves[thisThread->PVIdx].pv[0]
            : ttHit    ? tte->move() : MOVE_NONE;

    // At non-PV nodes we check for an early TT cutoff
    if (  !PvNode
        && ttHit
        && tte->depth() >= depth
        && ttValue != VALUE_NONE // Possible in case of TT access race
        && (ttValue >= beta ? (tte->bound() & BOUND_LOWER)
                            : (tte->bound() & BOUND_UPPER)))
    {
        // If ttMove is quiet, update move sorting heuristics on TT hit
        if (ttMove)
        {
            if (ttValue >= beta)
            {
                if (!pos.capture_or_promotion(ttMove))
                    update_quiet_stats(pos, ss, ttMove, nullptr, 0, stat_bonus(depth));

                // Extra penalty for a quiet TT move in previous ply when it gets refuted
                if ((ss-1)->moveCount == 1 && !pos.captured_piece())
                    update_continuation_histories(ss-1, pos.piece_on(prevSq), prevSq, -stat_bonus(depth + ONE_PLY));
            }
            // Penalty for a quiet ttMove that fails low
            else if (!pos.capture_or_promotion(ttMove))
            {
                int penalty = -stat_bonus(depth);
                thisThread->mainHistory[us][from_to(ttMove)] << penalty;
                update_continuation_histories(ss, pos.moved_piece(ttMove), to_sq(ttMove), penalty);
            }
        }
        return ttValue;
    }

    // Step 5. Tablebases probe
    if (!rootNode && TB::Cardinality)
    {
        int piecesCount = pos.count<ALL_PIECES>();

        if (    piecesCount <= TB::Cardinality
            &&  piecesCount > 3
            && (piecesCount <  TB::Cardinality || depth >= TB::ProbeDepth)
            &&  pos.rule50_count() == 0
            && !pos.can_castle(ANY_CASTLING))
        {
            TB::ProbeState err;
            TB::WDLScore wdl = Tablebases::probe_wdl(pos, &err);

            if (err != TB::ProbeState::FAIL)
            {
                thisThread->tbHits.fetch_add(1, std::memory_order_relaxed);

                int drawScore = TB::UseRule50 ? 1 : 0;

                value =  wdl < -drawScore ? -VALUE_KNOWN_WIN
                       : wdl >  drawScore ?  VALUE_KNOWN_WIN
                                          :  VALUE_DRAW + 2 * wdl * drawScore;

                Bound b =  wdl < -drawScore ? BOUND_UPPER
                         : wdl >  drawScore ? BOUND_LOWER : BOUND_EXACT;

                // Always return draw scores and values outside the search window.
                // Otherwise, continue searching.
                if (    b == BOUND_EXACT
                    || (b == BOUND_LOWER ? value >= beta : value <= alpha))
                    return value;

                if (PvNode)
                {
                    if (b == BOUND_LOWER)
                        bestValue = value, alpha = std::max(alpha, bestValue);
                    else
                        maxValue = value;
                }
            }
        }
    }

    // Step 6. Evaluate the position statically
    if (inCheck)
    {
        ss->staticEval = eval = VALUE_NONE;
        improving = false;
        goto moves_loop;
    }
    else if (ttHit)
    {
        // Never assume anything on values stored in TT
        if ((ss->staticEval = eval = tte->eval()) == VALUE_NONE)
            eval = ss->staticEval = evaluate(pos);

        // Can ttValue be used as a better position evaluation?
        if (    ttValue != VALUE_NONE
            && (tte->bound() & (ttValue > eval ? BOUND_LOWER : BOUND_UPPER)))
            eval = ttValue;
    }
    else
    {
        ss->staticEval = eval =
        (ss-1)->currentMove != MOVE_NULL ? evaluate(pos)
                                         : -(ss-1)->staticEval + 2 * Eval::Tempo;

        tte->save(posKey, VALUE_NONE, BOUND_NONE, DEPTH_NONE, MOVE_NONE,
                  ss->staticEval, TT.generation());
    }

    improving =   ss->staticEval >= (ss-2)->staticEval
               ||(ss-2)->staticEval == VALUE_NONE;

    if (    skipEarlyPruning
        ||  thisThread->rootDepth < 7 * ONE_PLY
        || !pos.non_pawn_material(us))
        goto moves_loop;

    // Step 7. Razoring (skipped when in check)
    if (   depth < 2 * ONE_PLY
        && eval <= alpha - RazorMargin)
        return qsearch<NT>(pos, ss, alpha, beta);

    // Step 8. Futility pruning: child node (skipped when in check)
    if (   !rootNode
        &&  depth < 7 * ONE_PLY
        &&  eval >= beta + futility_margin(depth, improving)
        &&  eval < VALUE_KNOWN_WIN) // Do not return unproven wins
        return eval;

    // Step 9. Null move search with verification search
    if (    doNull
        && !PvNode
        &&  eval >= beta
        && (ss-1)->statScore < 23200
        &&  ss->staticEval >= beta - 36 * depth / ONE_PLY + 225
        &&  thisThread->selDepth + 5 > thisThread->rootDepth / ONE_PLY
        && !(depth > 12 * ONE_PLY && MoveList<LEGAL>(pos).size() < 4))
    {
        assert(eval - beta >= 0);

        // Null move dynamic reduction based on depth and value
        Depth R = ((823 + 67 * depth / ONE_PLY) / 256 + std::min((eval - beta) / PawnValueMg, 3)) * ONE_PLY;

        ss->currentMove = MOVE_NULL;
        ss->continuationHistory = &thisThread->continuationHistory[NO_PIECE][0];

        pos.do_null_move(st);
        Value nullValue = depth-R < ONE_PLY ? -qsearch<NonPV>(pos, ss+1, -beta, -beta+1)
                                            : - search<NonPV>(pos, ss+1, -beta, -beta+1, depth-R, !cutNode, true);
        pos.undo_null_move();

        if (nullValue >= beta)
        {
            // Do not return unproven mate scores
            if (nullValue >= VALUE_MATE_IN_MAX_PLY)
                nullValue = beta;

            if (depth < 12 * ONE_PLY && abs(beta) < VALUE_KNOWN_WIN)
                return nullValue;

            // Do verification search at high depths
            R = std::min(R, 5 * ONE_PLY);
            Value v = depth-R < ONE_PLY ? qsearch<NonPV>(pos, ss, beta-1, beta)
                                        :  search<NonPV>(pos, ss, beta-1, beta, depth-R, false, true);

            if (v >= beta)
                return nullValue;
        }
    }

    // Step 10. ProbCut (skipped when in check)
    // If we have a good enough capture and a reduced search returns a value
    // much above beta, we can (almost) safely prune the previous move.
    if (   !PvNode
        &&  depth >= 5 * ONE_PLY
        &&  abs(beta) < VALUE_MATE_IN_MAX_PLY)
    {
        assert(is_ok((ss-1)->currentMove));

        int probCutCount = 0;
        Value rbeta = std::min(beta + 216 - 48 * improving, VALUE_INFINITE);

        // Initialize a MovePicker object for the current position
        MovePicker mp(pos, ttMove, rbeta - ss->staticEval, &thisThread->captureHistory);

        while ((move = mp.next_move()) != MOVE_NONE && probCutCount < 3) // try at most 3 moves
        {
            assert(is_ok(move));

            if (!pos.legal(move))
                continue;

            ++probCutCount;
            ss->currentMove = move;
            ss->continuationHistory = &thisThread->continuationHistory[pos.moved_piece(move)][to_sq(move)];

            pos.do_move(move, st);

            // Perform a preliminary qsearch to verify that the move holds
            value = -qsearch<NonPV>(pos, ss+1, -rbeta, -rbeta+1);

            // If the qsearch held perform the regular search
            if (value >= rbeta)
                value = -search<NonPV>(pos, ss+1, -rbeta, -rbeta+1, depth - 4 * ONE_PLY, !cutNode, false);

            pos.undo_move(move);

            if (value >= rbeta)
                return value;
        }
    }

    // Step 11. Internal iterative deepening (skipped when in check)
    if (    depth >= 6 * ONE_PLY
        && !ttMove
        && (PvNode || ss->staticEval + 128 >= beta))
    {
        Depth d = (3 * depth / (4 * ONE_PLY) - 2) * ONE_PLY;
        search<NT>(pos, ss, alpha, beta, d, cutNode, true);

        tte = TT.probe(posKey, ttHit);
        ttValue = ttHit ? value_from_tt(tte->value(), ss->ply) : VALUE_NONE;
        ttMove = ttHit ? tte->move() : MOVE_NONE;
    }

moves_loop: // When in check, search starts from here

    const PieceToHistory* contHist[] = { (ss-1)->continuationHistory, (ss-2)->continuationHistory, nullptr, (ss-4)->continuationHistory };
    Move countermove = thisThread->counterMoves[pos.piece_on(prevSq)][prevSq];

    MovePicker mp(pos, ttMove, depth, &thisThread->mainHistory, &thisThread->captureHistory,
                  contHist, countermove, ss->killers);

    value = bestValue; // Workaround a bogus 'uninitialized' warning under gcc
    skipQuiets = false;
    ttCapture = false;
    pvExact = PvNode && ttHit && tte->bound() == BOUND_EXACT;

    // Step 12. Loop through all pseudo-legal moves until no moves remain
    // or a beta cutoff occurs.
    while ((move = mp.next_move(skipQuiets)) != MOVE_NONE)
    {
      assert(is_ok(move));

      if (move == excludedMove)
          continue;

      if (rootNode)
      {
          // At root obey the "searchmoves" option and skip moves not listed in
          // Root Move List. As a consequence, any illegal move is also skipped.
          if (!std::count(thisThread->rootMoves.begin() + thisThread->PVIdx,
                          thisThread->rootMoves.end(), move))
              continue;

          // Skip all other moves until we have reached the last PV line
          if (   thisThread->PVIdx + 1 < thisThread->PVLines
              && move != ttMove)
              continue;
      }

      ss->moveCount = ++moveCount;

      if (rootNode && thisThread == Threads.main() && Time.elapsed() > 3000)
          sync_cout << "info depth " << depth / ONE_PLY
                    << " currmove " << UCI::move(move, pos.is_chess960())
                    << " currmovenumber " << moveCount + thisThread->PVIdx << sync_endl;
      if (PvNode)
          (ss+1)->pv = nullptr;

      extension = DEPTH_ZERO;
      captureOrPromotion = pos.capture_or_promotion(move);
      movedPiece = pos.moved_piece(move);
      givesCheck = gives_check(pos, move);

      moveCountPruning =   depth < 16 * ONE_PLY
                        && moveCount >= FutilityMoveCounts[improving][depth / ONE_PLY];

      // Step 13. Extensions

      // Singular extension search. If all moves but one fail low on a search
      // of (alpha-s, beta-s), and just one fails high on (alpha, beta), then
      // that move is singular and should be extended. To verify this we do a
      // reduced search on on all the other moves but the ttMove and if the
      // result is lower than ttValue minus a margin then we will extend the ttMove.
      if (   !rootNode
          && !excludedMove // Recursive singular search is not allowed
          &&  depth >= 8 * ONE_PLY
          &&  move == ttMove
          &&  ttValue != VALUE_NONE
          && (tte->bound() & BOUND_LOWER)
          &&  tte->depth() >= depth - 3 * ONE_PLY
          &&  pos.legal(move))
      {
          Value rBeta = std::max(ttValue - 2 * depth / ONE_PLY, -VALUE_MATE);
          Depth d = (depth / (2 * ONE_PLY)) * ONE_PLY;

          ss->excludedMove = move;
          value = search<NonPV>(pos, ss, rBeta - 1, rBeta, d, cutNode, true);
          ss->excludedMove = MOVE_NONE;

          if (value < rBeta)
              extension = ONE_PLY;
      }
      else if (    givesCheck // Check extension
               && !moveCountPruning
               &&  pos.see_ge(move))
          extension = ONE_PLY;

      // Calculate new depth for this move
      newDepth = depth - ONE_PLY + extension;

      // Step 14. Pruning at shallow depth
      if (  !rootNode
          && thisThread->rootDepth > 6 * ONE_PLY
          && pos.non_pawn_material(us)
          && bestValue > VALUE_MATED_IN_MAX_PLY
          && beta < VALUE_MATE_IN_MAX_PLY)
      {
          if (   !captureOrPromotion
              && !givesCheck
              && (!pos.advanced_pawn_push(move) || pos.non_pawn_material() >= Value(5000)))
          {
              // Move count based pruning
              if (moveCountPruning)
              {
                  skipQuiets = true;
                  continue;
              }

              // Reduced depth of the next LMR search
              int lmrDepth = std::max(newDepth - reduction<PvNode>(improving, depth, moveCount), DEPTH_ZERO) / ONE_PLY;

              // Countermoves based pruning
              if (    lmrDepth < 3 + ((ss-1)->statScore > 0)
                  && (*contHist[0])[movedPiece][to_sq(move)] < CounterMovePruneThreshold
                  && (*contHist[1])[movedPiece][to_sq(move)] < CounterMovePruneThreshold)
                  continue;

              // Futility pruning: parent node
              if (    lmrDepth < 7
                  && !inCheck
                  &&  ss->staticEval + 256 + 200 * lmrDepth <= alpha)
                  continue;

              // Prune moves with negative SEE
              if (!pos.see_ge(move, Value(-29 * lmrDepth * lmrDepth)))
                  continue;
          }
          else if (   !extension
                   && !pos.see_ge(move, -PawnValueEg * (depth / ONE_PLY)))
                  continue;
      }

      // Speculative prefetch as early as possible
      prefetch(TT.first_entry(pos.key_after(move)));

      // Check for legality just before making the move
      if (!rootNode && !pos.legal(move))
      {
          ss->moveCount = --moveCount;
          continue;
      }

      if (move == ttMove && captureOrPromotion)
          ttCapture = true;

      // Update the current move (this must be done after singular extension search)
      ss->currentMove = move;
      ss->continuationHistory = &thisThread->continuationHistory[movedPiece][to_sq(move)];

      // Step 15. Make the move
      pos.do_move(move, st, givesCheck);

      // Step 16. Reduced depth search (LMR). If the move fails high it will be
      // re-searched at full depth.
      if (    depth >= 3 * ONE_PLY
          &&  moveCount > 1
          && (!captureOrPromotion || moveCountPruning))
      {
          Depth r = reduction<PvNode>(improving, depth, moveCount);

          // Decrease reduction if opponent's move count is high
          if ((ss-1)->moveCount > 15)
              r -= ONE_PLY;

          if (!captureOrPromotion)
          {
              // Decrease reduction for exact PV nodes
              if (pvExact)
                  r -= ONE_PLY;

              // Increase reduction if ttMove is a capture
              if (ttCapture)
                  r += ONE_PLY;

              // Increase reduction for cut nodes
              if (cutNode)
                  r += 2 * ONE_PLY;

              // Decrease reduction for moves that escape a capture. Filter out
              // castling moves, because they are coded as "king captures rook" and
              // hence break make_move().
              else if (    type_of(move) == NORMAL
                       && !pos.see_ge(make_move(to_sq(move), from_sq(move))))
                  r -= 2 * ONE_PLY;

              ss->statScore =  thisThread->mainHistory[us][from_to(move)]
                             + (*contHist[0])[movedPiece][to_sq(move)]
                             + (*contHist[1])[movedPiece][to_sq(move)]
                             + (*contHist[3])[movedPiece][to_sq(move)]
                             - 4000;

              // Decrease/increase reduction by comparing our and opponent's stat scores
              r -= ((ss->statScore >= 0) - ((ss-1)->statScore >= 0)) * ONE_PLY;

              // Decrease/increase reduction for moves with a good/bad history
              r -= ss->statScore / 20000 * ONE_PLY;
          }

          if (r > DEPTH_ZERO)
          {
              Depth d = std::max(newDepth - r, ONE_PLY);
              value = -search<NonPV>(pos, ss+1, -(alpha+1), -alpha, d, true, false);

              doFullDepthSearch = value > alpha;
          }
          else
              doFullDepthSearch = true;
      }
      else
          doFullDepthSearch = !PvNode || moveCount > 1;

      // Step 17. Full depth search when LMR is skipped or fails high
      if (doFullDepthSearch)
          value = newDepth < ONE_PLY ? -qsearch<NonPV>(pos, ss+1, -(alpha+1), -alpha)
                                     : - search<NonPV>(pos, ss+1, -(alpha+1), -alpha, newDepth, !cutNode, false);

      // For PV nodes only, do a full PV search on the first move or after a fail
      // high (in the latter case search only if value < beta), otherwise let the
      // parent node fail low with value <= alpha and try another move.
      if (PvNode && (moveCount == 1 || (value > alpha && (rootNode || value < beta))))
      {
          (ss+1)->pv = pv;
          (ss+1)->pv[0] = MOVE_NONE;

          value = newDepth < ONE_PLY ? -qsearch<PV>(pos, ss+1, -beta, -alpha)
                                     : - search<PV>(pos, ss+1, -beta, -alpha, newDepth, false, false);
      }

      // Step 18. Undo move
      pos.undo_move(move);

      assert(value > -VALUE_INFINITE && value < VALUE_INFINITE);

      // Step 19. Check for a new best move
      // Finished searching the move. If a stop occurred, the return value of
      // the search cannot be trusted, and we return immediately without
      // updating best move, PV and TT.
      if (Threads.stop.load(std::memory_order_relaxed))
          return VALUE_ZERO;

      if (rootNode)
      {
          RootMove& rm = *std::find(thisThread->rootMoves.begin(),
                                    thisThread->rootMoves.end(), move);

          // PV move or new best move?
          if (moveCount == 1 || value > alpha)
          {
              rm.score = value;
              rm.selDepth = thisThread->selDepth;
              rm.pv.resize(1);

              assert((ss+1)->pv);

              for (Move* m = (ss+1)->pv; *m != MOVE_NONE; ++m)
                  rm.pv.push_back(*m);

              // We record how often the best move has been changed in each
              // iteration. This information is used for time management: When
              // the best move changes frequently, we allocate some more time.
              if (moveCount > 1 && thisThread == Threads.main())
                  ++static_cast<MainThread*>(thisThread)->bestMoveChanges;
          }
          else
              // All other moves but the PV are set to the lowest value: this
              // is not a problem when sorting because the sort is stable and the
              // move position in the list is preserved - just the PV is pushed up.
              rm.score = -VALUE_INFINITE;
      }

      if (value > bestValue)
      {
          bestValue = value;

          if (value > alpha)
          {
              bestMove = move;

              if (PvNode && !rootNode) // Update pv even in fail-high case
                  update_pv(ss->pv, move, (ss+1)->pv);

              if (PvNode && value < beta) // Update alpha! Always alpha < beta
                  alpha = value;
              else
              {
                  assert(value >= beta); // Fail high
                  ss->statScore = 0;
                  break;
              }
          }
      }

      if (move != bestMove)
      {
          if (captureOrPromotion && captureCount < 32)
              capturesSearched[captureCount++] = move;

          else if (!captureOrPromotion && quietCount < 64)
              quietsSearched[quietCount++] = move;
      }
    }

    // The following condition would detect a stop only after move loop has been
    // completed. But in this case bestValue is valid because we have fully
    // searched our subtree, and we can anyhow save the result in TT.
    /*
       if (Threads.stop)
        return VALUE_DRAW;
    */

    // Step 20. Check for mate and stalemate
    // All legal moves have been searched and if there are no legal moves, it
    // must be a mate or a stalemate. If we are in a singular extension search then
    // return a fail low score.

    assert(moveCount || !inCheck || excludedMove || !MoveList<LEGAL>(pos).size());

    if (!moveCount)
        bestValue = excludedMove ? alpha
                   :     inCheck ? mated_in(ss->ply) : VALUE_DRAW;
    else if (bestMove)
    {
        // Quiet best move: update move sorting heuristics
        if (!pos.capture_or_promotion(bestMove))
            update_quiet_stats(pos, ss, bestMove, quietsSearched, quietCount, 
							   stat_bonus(depth + (bestValue > beta + PawnValueMg ? ONE_PLY : DEPTH_ZERO)));
        // Update capture stats in any case
        update_capture_stats(pos, bestMove, capturesSearched, captureCount, stat_bonus(depth + ONE_PLY));

        // Extra penalty for a quiet TT move in previous ply when it gets refuted
        if ((ss-1)->moveCount == 1 && !pos.captured_piece())
            update_continuation_histories(ss-1, pos.piece_on(prevSq), prevSq, -stat_bonus(depth + ONE_PLY));
    }
    // Bonus for prior countermove that caused the fail low
    else if (   (PvNode || depth >= 3 * ONE_PLY)
             && !pos.captured_piece()
             && is_ok((ss-1)->currentMove))
        update_continuation_histories(ss-1, pos.piece_on(prevSq), prevSq, stat_bonus(depth));

    if (PvNode)
        bestValue = std::min(bestValue, maxValue);

    if (!excludedMove && bestValue != VALUE_REP_DRAW)
        tte->save(posKey, value_to_tt(bestValue, ss->ply),
                  bestValue >= beta ? BOUND_LOWER :
                  PvNode && bestMove ? BOUND_EXACT : BOUND_UPPER,
                  depth, bestMove, ss->staticEval, TT.generation());

    assert(bestValue > -VALUE_INFINITE && bestValue < VALUE_INFINITE);

    return bestValue;
  }


  // qsearch() is the quiescence search function, which is called by the main
  // search function with depth zero, or recursively with depth less than ONE_PLY.
  template <NodeType NT>
  Value qsearch(Position& pos, Stack* ss, Value alpha, Value beta, Depth depth) {

    constexpr bool PvNode = NT == PV;

    assert(alpha >= -VALUE_INFINITE && alpha < beta && beta <= VALUE_INFINITE);
    assert(PvNode || (alpha == beta - 1));
    assert(depth <= DEPTH_ZERO);
    assert(depth / ONE_PLY * ONE_PLY == depth);

    Move pv[MAX_PLY+1];
    StateInfo st;
    TTEntry* tte;
    Key posKey;
    Move ttMove, move, bestMove;
    Depth ttDepth;
    Value bestValue, value, ttValue, futilityValue, futilityBase, oldAlpha;
    bool ttHit, inCheck, givesCheck, evasionPrunable;
    int moveCount;

    // Initialize node
    Thread* thisThread = pos.this_thread();
    inCheck = bool(pos.checkers());

    if (PvNode)
    {
        oldAlpha = alpha; // To flag BOUND_EXACT when eval above alpha and no available moves
        (ss+1)->pv = pv;
        ss->pv[0] = MOVE_NONE;
        thisThread->selDepth = std::max(ss->ply, thisThread->selDepth);
    }

    // Check for draw by 50-move rule
    if (    TB::UseRule50
        &&  pos.rule50_count() == 100
        && (!inCheck || MoveList<LEGAL>(pos).size()))
        return VALUE_DRAW;

    // Check for draw by repetition
    if (pos.has_repeated(false, ss->ply))
        return VALUE_REP_DRAW;

    // Check for maximum ply reached
    if (ss->ply >= MAX_PLY)
        return !inCheck ? evaluate(pos) : beta;

    assert(0 <= ss->ply && ss->ply < MAX_PLY);

    (ss+1)->ply = ss->ply + 1;
    ss->currentMove = bestMove = MOVE_NONE;
    ss->continuationHistory = &thisThread->continuationHistory[NO_PIECE][0];
    moveCount = 0;

    // Decide whether or not to include checks: this fixes also the type of
    // TT entry depth that we are going to use. Note that in qsearch we use
    // only two types of depth in TT: DEPTH_QS_CHECKS or DEPTH_QS_NO_CHECKS.
    ttDepth = inCheck || depth >= DEPTH_QS_CHECKS ? DEPTH_QS_CHECKS
                                                  : DEPTH_QS_NO_CHECKS;
    // Transposition table lookup
    posKey = pos.key();
    tte = TT.probe(posKey, ttHit);
    ttValue = ttHit ? value_from_tt(tte->value(), ss->ply) : VALUE_NONE;
    ttMove = ttHit ? tte->move() : MOVE_NONE;

    if (  !PvNode
        && ttHit
        && tte->depth() >= ttDepth
        && ttValue != VALUE_NONE // Only in case of TT access race
        && (ttValue >= beta ? (tte->bound() & BOUND_LOWER)
                            : (tte->bound() & BOUND_UPPER)))
        return ttValue;

    // Evaluate the position statically
    if (inCheck)
    {
        ss->staticEval = VALUE_NONE;
        bestValue = futilityBase = -VALUE_INFINITE;
    }
    else
    {
        if (ttHit)
        {
            // Never assume anything on values stored in TT
            if ((ss->staticEval = bestValue = tte->eval()) == VALUE_NONE)
                ss->staticEval = bestValue = evaluate(pos);

            // Can ttValue be used as a better position evaluation?
            if (   !PvNode
                &&  ttValue != VALUE_NONE
                && (tte->bound() & (ttValue > bestValue ? BOUND_LOWER : BOUND_UPPER)))
                bestValue = ttValue;
        }
        else
            ss->staticEval = bestValue =
            (ss-1)->currentMove != MOVE_NULL ? evaluate(pos)
                                             : -(ss-1)->staticEval + 2 * Eval::Tempo;

        // Stand pat. Return immediately if static value is at least beta
        if (bestValue >= beta)
        {
            if (!ttHit)
                tte->save(posKey, value_to_tt(bestValue, ss->ply), BOUND_LOWER,
                          DEPTH_NONE, MOVE_NONE, ss->staticEval, TT.generation());

            return bestValue;
        }

        if (PvNode && bestValue > alpha)
            alpha = bestValue;

        futilityBase = bestValue + 128;
    }

    const PieceToHistory* contHist[] = { (ss-1)->continuationHistory, (ss-2)->continuationHistory, nullptr, (ss-4)->continuationHistory };

    // Initialize a MovePicker object for the current position, and prepare
    // to search the moves. Because the depth is <= 0 here, only captures,
    // queen promotions and checks (only if depth >= DEPTH_QS_CHECKS) will
    // be generated.
    MovePicker mp(pos, ttMove, depth, &thisThread->mainHistory,
                  &thisThread->captureHistory, contHist, to_sq((ss-1)->currentMove));

    // Loop through the moves until no moves remain or a beta cutoff occurs
    while ((move = mp.next_move()) != MOVE_NONE)
    {
      assert(is_ok(move));

      givesCheck = gives_check(pos, move);

      moveCount++;

      // Futility pruning
      if (   !inCheck
          && !givesCheck
          &&  futilityBase > -VALUE_KNOWN_WIN
          &&  pos.non_pawn_material(pos.side_to_move()) > BishopValueMg
          && !pos.advanced_pawn_push(move))
      {
          assert(type_of(move) != ENPASSANT); // Due to !pos.advanced_pawn_push

          futilityValue = futilityBase + PieceValue[EG][pos.piece_on(to_sq(move))];

          if (futilityValue <= alpha)
          {
              bestValue = std::max(bestValue, futilityValue);
              continue;
          }

          if (futilityBase <= alpha && !pos.see_ge(move, VALUE_ZERO + 1))
          {
              bestValue = std::max(bestValue, futilityBase);
              continue;
          }
      }

      // Detect non-capture evasions that are candidates to be pruned
      evasionPrunable =    inCheck
                       &&  (depth != DEPTH_ZERO || moveCount > 2)
                       &&  bestValue > VALUE_MATED_IN_MAX_PLY
                       && !pos.capture(move);

      // Don't search moves with negative SEE values
      if (  (!inCheck || evasionPrunable)
          && !pos.see_ge(move))
          continue;

      // Speculative prefetch as early as possible
      prefetch(TT.first_entry(pos.key_after(move)));

      // Check for legality just before making the move
      if (!pos.legal(move))
      {
          moveCount--;
          continue;
      }

      ss->currentMove = move;
      ss->continuationHistory = &thisThread->continuationHistory[pos.moved_piece(move)][to_sq(move)];

      // Make and search the move
      pos.do_move(move, st, givesCheck);
      value = -qsearch<NT>(pos, ss+1, -beta, -alpha, depth - ONE_PLY);
      pos.undo_move(move);

      assert(value > -VALUE_INFINITE && value < VALUE_INFINITE);

      // Check for a new best move
      if (value > bestValue)
      {
          bestValue = value;

          if (value > alpha)
          {
              if (PvNode) // Update pv even in fail-high case
                  update_pv(ss->pv, move, (ss+1)->pv);

              if (PvNode && value < beta) // Update alpha here!
              {
                  alpha = value;
                  bestMove = move;
              }
              else // Fail high
              {
                  tte->save(posKey, value_to_tt(value, ss->ply), BOUND_LOWER,
                            ttDepth, move, ss->staticEval, TT.generation());

                  return value;
              }
          }
       }
    }

    // All legal moves have been searched. A special case: If we're in check
    // and no legal moves were found, it is checkmate.
    if (inCheck && bestValue == -VALUE_INFINITE)
        return mated_in(ss->ply); // Plies to mate from the root

    if (bestValue != VALUE_REP_DRAW)
        tte->save(posKey, value_to_tt(bestValue, ss->ply),
                  PvNode && bestValue > oldAlpha ? BOUND_EXACT : BOUND_UPPER,
                  ttDepth, bestMove, ss->staticEval, TT.generation());

    assert(bestValue > -VALUE_INFINITE && bestValue < VALUE_INFINITE);

    return bestValue;
  }


  // value_to_tt() adjusts a mate score from "plies to mate from the root" to
  // "plies to mate from the current position". Non-mate scores are unchanged.
  // The function is called before storing a value in the transposition table.

  Value value_to_tt(Value v, int ply) {

    assert(v != VALUE_NONE);

    return  v >= VALUE_MATE_IN_MAX_PLY  ? v + ply
          : v <= VALUE_MATED_IN_MAX_PLY ? v - ply : v;
  }


  // value_from_tt() is the inverse of value_to_tt(): It adjusts a mate score
  // from the transposition table (which refers to the plies to mate/be mated
  // from current position) to "plies to mate/be mated from the root".

  Value value_from_tt(Value v, int ply) {

    return  v == VALUE_NONE             ? VALUE_NONE
          : v >= VALUE_MATE_IN_MAX_PLY  ? v - ply
          : v <= VALUE_MATED_IN_MAX_PLY ? v + ply : v;
  }


  // update_pv() adds current move and appends child pv[]

  void update_pv(Move* pv, Move move, Move* childPv) {

    for (*pv++ = move; childPv && *childPv != MOVE_NONE; )
        *pv++ = *childPv++;
    *pv = MOVE_NONE;
  }


  // update_continuation_histories() updates histories of the move pairs formed
  // by moves at ply -1, -2, and -4 with current move.

  void update_continuation_histories(Stack* ss, Piece pc, Square to, int bonus) {

    for (int i : {1, 2, 4})
        if (is_ok((ss-i)->currentMove))
            (*(ss-i)->continuationHistory)[pc][to] << bonus;
  }


  // update_capture_stats() updates move sorting heuristics when a new capture best move is found

  void update_capture_stats(const Position& pos, Move move,
                            Move* captures, int captureCnt, int bonus) {

      CapturePieceToHistory& captureHistory =  pos.this_thread()->captureHistory;
      Piece moved_piece = pos.moved_piece(move);
      PieceType captured = type_of(pos.piece_on(to_sq(move)));
      captureHistory[moved_piece][to_sq(move)][captured] << bonus;

      if (pos.capture_or_promotion(move))
          captureHistory[moved_piece][to_sq(move)][captured] << bonus;

      // Decrease all the other played capture moves
      for (int i = 0; i < captureCnt; ++i)
      {
          moved_piece = pos.moved_piece(captures[i]);
          captured = type_of(pos.piece_on(to_sq(captures[i])));
          captureHistory[moved_piece][to_sq(captures[i])][captured] << -bonus;
      }
  }


  // update_quiet_stats() updates move sorting heuristics when a new quiet best move is found

  void update_quiet_stats(const Position& pos, Stack* ss, Move move,
                          Move* quiets, int quietsCnt, int bonus) {

    if (ss->killers[0] != move)
    {
        ss->killers[1] = ss->killers[0];
        ss->killers[0] = move;
    }

    Color us = pos.side_to_move();
    Thread* thisThread = pos.this_thread();
    thisThread->mainHistory[us][from_to(move)] << bonus;
    update_continuation_histories(ss, pos.moved_piece(move), to_sq(move), bonus);

    if (is_ok((ss-1)->currentMove))
    {
        Square prevSq = to_sq((ss-1)->currentMove);
        thisThread->counterMoves[pos.piece_on(prevSq)][prevSq] = move;
    }

    // Decrease all the other played quiet moves
    for (int i = 0; i < quietsCnt; ++i)
    {
        thisThread->mainHistory[us][from_to(quiets[i])] << -bonus;
        update_continuation_histories(ss, pos.moved_piece(quiets[i]), to_sq(quiets[i]), -bonus);
    }
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

  // We should not stop pondering until told so by the GUI
  if (Threads.ponder)
      return;

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
  size_t PVIdx = pos.this_thread()->PVIdx;
  size_t multiPV = std::min((size_t)Options["MultiPV"], rootMoves.size());
  uint64_t nodesSearched = Threads.nodes_searched();
  uint64_t tbHits = Threads.tb_hits() + (TB::RootInTB ? rootMoves.size() : 0);

  for (size_t i = 0; i < multiPV; ++i)
  {
      bool updated = (i <= PVIdx && rootMoves[i].score != -VALUE_INFINITE);

      if (depth == ONE_PLY && !updated)
          continue;

      Depth d = updated ? depth : depth - ONE_PLY;
      Value v = updated ? rootMoves[i].score : rootMoves[i].previousScore;

      bool tb = TB::RootInTB && abs(v) < VALUE_MATE - MAX_PLY;
      v = tb ? rootMoves[i].TBScore : v;

      if (ss.rdbuf()->in_avail()) // Not at first line
          ss << "\n";

      ss << "info"
         << " depth "    << d / ONE_PLY
         << " seldepth " << rootMoves[i].selDepth
         << " multipv "  << i + 1
         << " score "    << UCI::value(v);

      if (!tb && i == PVIdx)
          ss << (v >= beta ? " lowerbound" : v <= alpha ? " upperbound" : "");

      ss << " nodes "    << nodesSearched
         << " nps "      << nodesSearched * 1000 / elapsed;

      if (elapsed > 1000) // Earlier makes little sense
          ss << " hashfull " << TT.hashfull();

      ss << " tbhits "   << tbHits
         << " time "     << elapsed
         << " pv";

      for (Move m : rootMoves[i].pv)
          ss << " " << UCI::move(m, pos.is_chess960());
  }

  return ss.str();
}


/// RootMove::extract_ponder_from_tt() is called in case we have no ponder move
/// before exiting the search, for instance, in case we stop the search during a
/// fail high at root. We try hard to have a ponder move to return to the GUI,
/// otherwise in case of 'ponder on' we have nothing to think on.

bool RootMove::extract_ponder_from_tt(Position& pos) {

    StateInfo st;
    bool ttHit;

    assert(pv.size() == 1);

    if (!pv[0])
        return false;

    pos.do_move(pv[0], st);
    TTEntry* tte = TT.probe(pos.key(), ttHit);

    if (ttHit)
    {
        Move m = tte->move(); // Local copy to be SMP safe
        if (MoveList<LEGAL>(pos).contains(m))
            pv.push_back(m);
    }

    pos.undo_move(pv[0]);
    return pv.size() > 1;
}


void Tablebases::rank_root_moves(Position& pos, Search::RootMoves& rootMoves) {

    RootInTB = false;
    UseRule50 = bool(Options["Syzygy50MoveRule"]);
    ProbeDepth = int(Options["SyzygyProbeDepth"]) * ONE_PLY;
    Cardinality = int(Options["SyzygyProbeLimit"]);
    bool dtz_available = true;
    int piecesCount = pos.count<ALL_PIECES>();

    // Tables with fewer pieces than SyzygyProbeLimit are searched with
    // ProbeDepth == DEPTH_ZERO
    if (Cardinality > MaxCardinality)
    {
        Cardinality = MaxCardinality;
        ProbeDepth = DEPTH_ZERO;
    }

    // Don't rank basic 3-men endgames
    if (    Cardinality >= piecesCount
        &&  piecesCount > 3
        && !pos.can_castle(ANY_CASTLING))
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
        std::stable_sort(rootMoves.begin(), rootMoves.end());

        // Probe during search only if DTZ is not available and we are winning
        if (dtz_available || rootMoves[0].TBScore <= VALUE_DRAW)
            Cardinality = 0;
    }
}
