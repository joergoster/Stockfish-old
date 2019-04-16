/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2019 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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

#include "types.h"

Value PieceValue[PHASE_NB][PIECE_NB] = {
  { VALUE_ZERO, PawnValueMg, KnightValueMg, BishopValueMg, RookValueMg, QueenValueMg },
  { VALUE_ZERO, PawnValueEg, KnightValueEg, BishopValueEg, RookValueEg, QueenValueEg }
};

namespace PSQT {

#define S(mg, eg) make_score(mg, eg)

// FileBonus[PieceType][Square] and RankBonus[PieceType][Square] contain Piece-Square scores.
// For each piece type on a given square a (middlegame, endgame) score pair is assigned for the file
// and the rank of the square. Tables are defined for files A..H, ranks 1..8 and white side: it is
// symmetric for black side.

constexpr Score FileBonus[PIECE_NB][FILE_NB] = {
  {},
  { S( 3, -17), S(-16,  4), S( 0,  2), S( 4,  0), S(-6, -4), S(-3,  4), S(10,  0), S( 6,  6), }, // PAWN
  { S(-2, -3), S(10, -3), S( 3,  1), S(-1,  9), S( 3, -7), S( 3,  3), S(-12,  1), S(11,  5), }, // KNIGHT
  { S(-3,  0), S( 5,  6), S(-4, 11), S( 1, -4), S( 4, -15), S(-3, -3), S( 2,  3), S(-13, -5), }, // BISHOP
  { S( 7, 13), S(14, -9), S( 4,  6), S(-14, -1), S( 0,  4), S( 3, -2), S(-2, 10), S(-10,  7), }, // ROOK
  { S( 0, -5), S(-2,  2), S( 0, 13), S(-2,  4), S( 3,  3), S( 4, -7), S(-7, -5), S(-2,  1), }, // QUEEN
  { S( 5, 11), S( 7,  5), S(14, -2), S(22,  -5), S( 3,  3), S( 4,  3), S(-4, -4), S(-1,  7), }, // KING
};

constexpr Score RankBonus[PIECE_NB][RANK_NB] = {
  {},
  { S( 0,  0), S( 9, -2), S( 7, -9), S( 8, -2), S(-1, -1), S(-3, -1), S(10, -5), S( 0,  0), }, // PAWN
  { S( 3,  9), S(-10, 2), S( 5, -3), S(-11,-15), S(13,  7), S(17, -8), S(-3,  6), S( 2,  8), }, // KNIGHT
  { S( 0, -1), S(-6,  8), S(-11,-10), S( 5,  3), S( 9,  3), S( 1, -14), S(10, -3), S(-9, -2), }, // BISHOP
  { S( 0,  2), S(-8,  3), S( 6, -3), S(-6,  0), S( 9, -4), S( 0, -9), S(-7,  6), S( 3, -13), }, // ROOK
  { S(-2,  4), S(-1, 10), S( 4, -3), S( 9, -4), S(-1, -14), S(-7,  4), S(-1,  4), S(-3, -6), }, // QUEEN
  { S( 8, -2), S(-8, -6), S( 6, -2), S(10, -8), S(13, -9), S( 5, -2), S(20,  4), S(-12, -3), }, // KING
};

#undef S

Score psq[PIECE_NB][SQUARE_NB];

// init() initializes piece-square tables: the white tables are lookuped
// from FileBonus[] and RankBonus[] and added to the piece value, then the
// black tables are initialized by flipping piece and square and changing
// the sign of the white scores.
void init() {

  for (Piece pc = W_PAWN; pc <= W_KING; ++pc)
  {
      PieceValue[MG][~pc] = PieceValue[MG][pc];
      PieceValue[EG][~pc] = PieceValue[EG][pc];

      Score score = make_score(PieceValue[MG][pc], PieceValue[EG][pc]);

      for (Square s = SQ_A1; s <= SQ_H8; ++s)
      {
          psq[ pc][ s] = score + FileBonus[pc][file_of(s)]
                               + RankBonus[pc][rank_of(s)];
          psq[~pc][~s] = -psq[pc][s];
      }
  }
}

} // namespace PSQT
