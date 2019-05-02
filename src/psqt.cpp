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

// FileBonus[PieceType][File] and RankBonus[PieceType][Rank] contain Piece-Square scores.
// For each piece type on a given square a (middlegame, endgame) score pair is assigned for the file
// and the rank of the square. Tables are defined for files A..H, ranks 1..8 and white side: it is
// symmetric for black side.

constexpr Score FileBonus[PIECE_NB][FILE_NB] = {
  {},
  { S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0) }, // PAWN
  { S(-99, -80), S(-26, -60), S(-10, -35), S(-9, -8), S(-9, -8), S(-10, -35), S(-26, -60), S(-99, -80) }, // KNIGHT
  { S(-20, -30), S(20, 20), S(10, 10), S( 0, 10), S( 0, 10), S(10, 10), S(20, 20), S(-20, -30) }, // BISHOP
  { S(-18,  0), S(-12,  0), S(-8,  0), S( 0,  0), S( 0,  0), S(-8,  0), S(-12,  0), S(-18,  0) }, // ROOK
  { S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0) }, // QUEEN
  { S(220, 60), S(310, 100), S(220, 130), S(180, 140), S(160, 140), S(200, 130), S( 290, 100), S(200, 60) } // KING
};

constexpr Score RankBonus[PIECE_NB][RANK_NB] = {
  {},
  { S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0) }, // PAWN
  { S(-70, -10), S(-10, 15), S(20, 35), S(40, 44), S(50, 50), S(55, 40), S(30, 28), S(-50, -20) }, // KNIGHT
  { S(-20, -20), S( 0, -10), S(15, 5), S(35, 10), S(30, 10), S(10, 5), S(-10, -10), S(-30, -20) }, // BISHOP
  { S(-2,  0), S( 0,  0), S( 2,  0), S( 4,  0), S( 6,  0), S( 8,  0), S(16,  0), S( 8,  0) }, // ROOK
  { S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0), S( 0,  0) }, // QUEEN
  { S(10, -60), S( 0, 0), S(-60, 35), S(-80, 50), S(-110, 65), S(-150, 50), S(-165, 5), S(-190, -40) } // KING
};

#undef S

Score psq[PIECE_NB][SQUARE_NB];

// init() initializes piece-square tables: the white tables are looked up
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
