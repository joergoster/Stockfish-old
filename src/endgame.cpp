/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2017 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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

#include "bitboard.h"
#include "endgame.h"
#include "movegen.h"

using std::string;

namespace {

#ifndef NDEBUG
  bool verify_material(const Position& pos, Color c, Value npm, int pawnsCnt) {
    return pos.non_pawn_material(c) == npm && pos.count<PAWN>(c) == pawnsCnt;
  }
#endif

  // Map the square as if strongSide is white and strongSide's only pawn
  // is on the left half of the board.
  Square normalize(const Position& pos, Color strongSide, Square sq) {

    assert(pos.count<PAWN>(strongSide) == 1);

    if (file_of(pos.square<PAWN>(strongSide)) >= FILE_E)
        sq = Square(sq ^ 7); // Mirror SQ_H1 -> SQ_A1

    if (strongSide == BLACK)
        sq = ~sq;

    return sq;
  }

} // namespace


/// Endgames members definitions

Endgames::Endgames() {

  add<KNNK>("KNNK");

  add<KNPK>("KNPK");
}


template<EndgameType E, typename T>
void Endgames::add(const string& code) {
  StateInfo st;
  map<T>()[Position().set(code, WHITE, &st).material_key()] = std::unique_ptr<EndgameBase<T>>(new Endgame<E>(WHITE));
  map<T>()[Position().set(code, BLACK, &st).material_key()] = std::unique_ptr<EndgameBase<T>>(new Endgame<E>(BLACK));
}


/// Some cases of trivial draws
template<> Value Endgame<KNNK>::operator()(const Position&) const { return VALUE_DRAW; }


/// KNP vs K. There is a single rule: if the pawn is a rook pawn on the 7th rank
/// and the defending king prevents the pawn from advancing, the position is drawn.
template<>
ScaleFactor Endgame<KNPK>::operator()(const Position& pos) const {

  assert(verify_material(pos, strongSide, KnightValueMg, 1));
  assert(verify_material(pos, weakSide, VALUE_ZERO, 0));

  // Assume strongSide is white and the pawn is on files A-D
  Square pawnSq     = normalize(pos, strongSide, pos.square<PAWN>(strongSide));
  Square weakKingSq = normalize(pos, strongSide, pos.square<KING>(weakSide));

  if (pawnSq == SQ_A7 && distance(SQ_A8, weakKingSq) <= 1)
      return SCALE_FACTOR_DRAW;

  return SCALE_FACTOR_NONE;
}
