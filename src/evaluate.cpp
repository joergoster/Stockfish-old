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

#include <algorithm>
#include <cassert>
#include <iomanip>
#include <sstream>

#include "evaluate.h"
#include "neuralnet.h"
#include "thread.h"

namespace Trace {

  double to_cp(Value v) { return double(v) / PawnValueEg; }
}

using namespace Trace;

namespace {

  // Evaluation class
  class Evaluation {
  public:
    Evaluation() = delete;
    explicit Evaluation(const Position& p) : pos(p) {}
    Evaluation& operator=(const Evaluation&) = delete;
    Value value();

  private:
    const Position& pos;
  };


  // Evaluation::value() is the main function of the class. It computes the various
  // parts of the evaluation and returns the value of the position from the point
  // of view of the side to move.

  Value Evaluation::value() {

    assert(!pos.checkers());

    Value v = pos.nnue_output();
    v = std::min(v, Value(30000));

    return (pos.side_to_move() == WHITE ? v : -v) + Tempo;
  }

} // namespace


/// evaluate() is the evaluator for the outer world. It returns a static
/// evaluation of the position from the point of view of the side to move.

Value Eval::evaluate(const Position& pos) {
  return Evaluation(pos).value();
}


/// trace() is like evaluate(), but instead of returning a value, it returns
/// a string (suitable for outputting to stdout) that contains the detailed
/// descriptions and values of each evaluation term. Useful for debugging.

std::string Eval::trace(const Position& pos) {

  if (pos.checkers())
      return "Total evaluation: none (in check)";

  Value v = Evaluation(pos).value();
  v = pos.side_to_move() == WHITE ? v : -v; // Trace scores are from white's point of view

  std::stringstream ss;
  ss << std::setprecision(2) << "\nNNUE evaluation: " << to_cp(v) << " (white side)\n";

  return ss.str();
}
