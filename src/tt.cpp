/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2020 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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

#include <cstring>   // For std::memset
#include <iostream>
#include <thread>

#include "bitboard.h"
#include "misc.h"
#include "thread.h"
#include "tt.h"
#include "uci.h"

TranspositionTable TT; // Our global transposition table


/// TranspositionTable::resize() sets the size of the transposition table,
/// measured in megabytes. Transposition table consists of a power of 2 number
/// of clusters and each cluster consists of ClusterSize number of TTEntry.

void TranspositionTable::resize(size_t mbSize) {

  Threads.main()->wait_for_search_finished();

  aligned_ttmem_free(mem);

  clusterCount = mbSize * 1024 * 1024 / sizeof(Cluster);
  table = static_cast<Cluster*>(aligned_ttmem_alloc(clusterCount * sizeof(Cluster), mem));

  if (!mem)
  {
      std::cerr << "Failed to allocate " << mbSize
                << "MB for transposition table." << std::endl;
      exit(EXIT_FAILURE);
  }

  clear();
}


/// TranspositionTable::clear() initializes the entire transposition table to zero,
//  in a multi-threaded way.

void TranspositionTable::clear() {

  std::vector<std::thread> threads;

  for (size_t idx = 0; idx < Options["Threads"]; ++idx)
  {
      threads.emplace_back([this, idx]() {

          // Thread binding gives faster search on systems with a first-touch policy
          if (Options["Threads"] > 8)
              WinProcGroup::bindThisThread(idx);

          // Each thread will zero its part of the hash table
          const size_t stride = size_t(clusterCount / Options["Threads"]),
                       start  = size_t(stride * idx),
                       len    = idx != Options["Threads"] - 1 ?
                                stride : clusterCount - start;

          std::memset(&table[start], 0, len * sizeof(Cluster));
      });
  }

  for (std::thread& th : threads)
      th.join();

  generation16 = 0;
}

/// TranspositionTable::probe() looks up the current position in the
/// transposition table. It returns true if the position is found.

bool TranspositionTable::probe(const Key key, Value& ttValue, Value& ttEval, Move& ttMove,
                                              Depth& ttDepth, Bound& ttBound, bool& ttPv) const {

  TTEntry* const tte = first_entry(key);

  for (int i = 0; i < ClusterSize; ++i)
  {
      if (!tte[i].key32)
          return false;

      if (tte[i].key32 == uint32_t(key >> 32))
      {
          // Refresh the existing entry (makes it a bit harder to replace).
          // However, we don't know if this entry is useful or not ...
          tte[i].age16 = generation16;

          // Copy over values
          ttValue = Value(tte[i].value16);
          ttEval  = Value(tte[i].eval16);
          ttMove  = Move(tte[i].move16);
          ttDepth = Depth(tte[i].depth16);
          ttBound = Bound(tte[i].pvbound16 & 0x3);
          ttPv    = bool(tte[i].pvbound16 & 0x4);

          return true;
      }
  }

  return false;
}


/// TranspositionTable::save() populates the hash with a new node's data, possibly
/// overwriting an old position. Update is not atomic and can be racy.
/// Currently, we have two passes. First, we're looking for a matching entry
/// or an empty slot. Otherwise, we're looking for the least valuable
/// entry which will be replaced by the new entry. The replace value of an entry
/// is calculated as its depth minus 4 times its relative age. TTEntry t1 is considered
/// more valuable than TTEntry t2 if its replace value is greater than that of t2.

void TranspositionTable::save(Key k, Value v, bool pv, Bound b, Depth d, Move m, Value ev) {

  TTEntry* tte = first_entry(k);
  TTEntry* replace;
  const uint32_t key32 = k >> 32;
  bool success = false;

  // First, look for a slot with a matching key. As soon
  // as we find an empty slot, we can break immediately.
  // (There will be no entry after an empty one!)
  for (int i = 0; i < ClusterSize; ++i)
      if (!tte[i].key32 || tte[i].key32 == key32)
      {
          replace = &tte[i];
          success = true;

          break;
      }

  if (!success)
  {
      // The first pass failed, find the least valuable entry to be replaced
      replace = &tte[0];

      for (int i = 1; i < ClusterSize; ++i)
          if (  replace->depth16 - 4 * (generation16 - replace->age16)
              >   tte[i].depth16 - 4 * (generation16 -   tte[i].age16))
              replace = &tte[i];
  }

  // Preserve any existing move for the same position
  if (m || replace->key32 != key32)
      replace->move16 = uint16_t(m);

  // Always save to an empty slot, overwrite
  // non-matching and less valuable entries.
  if (   replace->key32 != key32
      || b == BOUND_EXACT
      || d > replace->depth16 - 4)
  {
      replace->key32     = key32;
      replace->age16     = generation16;
      replace->value16   = int16_t(v);
      replace->eval16    = int16_t(ev);
      replace->depth16   = int16_t(d);
      replace->pvbound16 = int16_t(int16_t(pv) << 2 | b);
  }
}


/// TranspositionTable::hashfull() returns an approximation of the hashtable occupation
/// during a search. The hash is x permill full, as per UCI protocol. We are checking
/// the first 1,000 clusters for entries with current age.

int TranspositionTable::hashfull() const {

  int cnt = 0;
  for (int i = 0; i < 1000; ++i)
      for (int j = 0; j < ClusterSize; ++j)
          cnt += table[i].entry[j].age16 == generation16;

  return cnt / ClusterSize;
}
