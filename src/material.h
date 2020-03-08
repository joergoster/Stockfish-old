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

#ifndef MATERIAL_H_INCLUDED
#define MATERIAL_H_INCLUDED

#include "misc.h"
#include "position.h"
#include "types.h"

namespace Material {

/// Material::Entry contains various information about a material configuration.
/// It contains a material imbalance evaluation, the game phase, and scale factors.

struct Entry {

  Score imbalance() const { return make_score(value, value); }
  Phase game_phase() const { return gamePhase; }

  Key key;
  int16_t value;
  Phase gamePhase;
};

typedef HashTable<Entry, 8192> Table;

Entry* probe(const Position& pos);

} // namespace Material

#endif // #ifndef MATERIAL_H_INCLUDED
