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
}

/// TranspositionTable::probe() looks up the current position in the transposition
/// table. It returns true and a pointer to the TTEntry if the position is found.
/// Otherwise, it returns false and a null pointer.

TTEntry* TranspositionTable::probe(const Key key, bool& found) const {

  TTEntry* const tte = first_entry(key);
  const uint16_t key16 = key >> 48;  // Use the high 16 bits as key inside the cluster

  for (int i = 0; i < ClusterSize; ++i)
      if (tte[i].key16 == key16)
      {
          // Refresh the existing entry (makes it a bit harder to replac).
          // However, we don't know if this entry is useful or not ...
          tte[i].genBound8 = uint8_t(generation8 | (tte[i].genBound8 & 0x7));

          return found = true, &tte[i];
      }

  return found = false, nullptr;
}


/// TranspositionTable::save() populates the hash with a new node's data, possibly
/// overwriting an old position. Update is not atomic and can be racy.
/// Currently, we have two passes. First, we look for an empty slot. If there is one,
/// we store the entry and we're done. Otherwise, we're looking for the least valuable
/// entry which will be replaced by the new entry. The replace value of an entry
/// is calculated as its depth minus 8 times its relative age. TTEntry t1 is considered
/// more valuable than TTEntry t2 if its replace value is greater than that of t2.

void TranspositionTable::save(Key k, Value v, bool pv, Bound b, Depth d, Move m, Value ev) {

  TTEntry* replace;
  TTEntry* tte = first_entry(k);
  const uint16_t key16 = k >> 48;
  bool success = false;

  // First, look for a slot with a matching key
  for (int i = 0; i < ClusterSize; ++i)
      if (tte[i].key16 == key16)
      {
          replace = &tte[i];
          success = true;
          break;
      }

  // Second, look for an empty slot
  if (!success)
  {
      for (int i = 0; i < ClusterSize; ++i)
          if (!tte[i].key16)
          {
              replace = &tte[i];
              success = true;
              break;
          }
  }

  // Last, find an entry to be replaced
  if (!success)
  {
      replace = tte;
      for (int i = 1; i < ClusterSize; ++i)
          // Due to our packed storage format for generation and its cyclic
          // nature we add 263 (256 is the modulus plus 7 to keep the unrelated
          // lowest three bits from affecting the result) to calculate the entry
          // age correctly even after generation8 overflows into the next cycle.
          if (  replace->depth8 - ((263 + generation8 - replace->genBound8) & 0xF8)
              >   tte[i].depth8 - ((263 + generation8 -   tte[i].genBound8) & 0xF8))
              replace = &tte[i];
  }

  // Preserve any existing move for the same position
  if (m || replace->key16 != key16)
      replace->move16 = uint16_t(m);

  replace->key16     = key16;
  replace->value16   = int16_t(v);
  replace->eval16    = int16_t(ev);
  replace->genBound8 = uint8_t(generation8 | uint8_t(pv) << 2 | b);
  replace->depth8    = uint8_t(d - DEPTH_OFFSET);
}


/// TranspositionTable::hashfull() returns an approximation of the hashtable occupation
/// during a search. The hash is x permill full, as per UCI protocol. We are checking
/// the first 1,000 clusters for entries with current age and valid bounds.

int TranspositionTable::hashfull() const {

  int cnt = 0;
  for (int i = 0; i < 1000; ++i)
      for (int j = 0; j < ClusterSize; ++j)
          cnt +=   (table[i].entry[j].genBound8 & 0xF8) == generation8
                && (table[i].entry[j].genBound8 & 0x3) != BOUND_NONE;

  return cnt / ClusterSize;
}
