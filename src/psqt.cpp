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
  { S(12, -12), S(-22, -4), S(-9,  9), S(-11, 15), S(-23, 0), S( 7, -20), S(20, 16), S(-19, -10), }, // PAWN
  { S(-3, -27), S( 7, -63), S(-5, 72), S(55, 42), S(26, -85), S(22, -26), S(70, 7), S(16, 13), }, // KNIGHT
  { S(-62, -49), S(-25, -52), S(12, -10), S(18, -45), S(19, -7), S(11, 83), S(36, -78), S(-21, -61), }, // BISHOP
  { S(-3, 8), S(-3, -9), S(-22, -6), S(-30, 20), S(27, 9), S(-3, -28), S(14, 1), S(-5, -6), }, // ROOK
  { S( 6, 33), S(32, -19), S(32, -16), S(-12, -39), S(-23, -36), S(-3, 45), S( 4, -12), S( 2, -30), }, // QUEEN
  { S(-55, -34), S(14, 32), S(-106, -7), S(-60, 20), S(-103, -38), S(-2, 133), S(68, -14), S(-109, -61), }, // KING
};

constexpr Score RankBonus[PIECE_NB][RANK_NB] = {
  {},
  { S( 0,  0), S(11, 0), S( 6, -2), S(13, -24), S(-12, 35), S(-19, -20), S( 3, -24), S( 0,  0), }, // PAWN
  { S(75, -8), S(40, 22), S(-34, 37), S(-21, 27), S(-18, 33), S(-39, 0), S( 0, 10), S( 2, 4), }, // KNIGHT
  { S(-49, 21), S(-12, 5), S( 5,  2), S(43, -74), S(12, -20), S(-46, 5), S(-38, 19), S(21, -44), }, // BISHOP
  { S(-8,  7), S( 6,  1), S(-9, 45), S( 5, -9), S( 5, -18), S(-5, -4), S(-9, 15), S( 0, 10), }, // ROOK
  { S(-38, 24), S( 9, -19), S(26, 4), S(-35, -16), S(15, -46), S(25, 51), S(-2, -10), S( 5, -3), }, // QUEEN
  { S(-49, 96), S(25, -149), S(-129, 110), S(26, 26), S( 7, 0), S(70, 29), S(30, -56), S(-110, 103), }, // KING
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
