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

#include <fstream>
#include <iostream>
#include <istream>
#include <vector>

#include "position.h"

using namespace std;

namespace {

const vector<string> Defaults = {
  // Mate in 2
  "3R3K/4ppp1/2p5/1nN1kpQN/8/r7/b4rP1/3R2n1 w - -",
  "2R5/8/3k4/3N2p1/4K1Q1/8/8/8 w - -",
  "5N2/2n1n3/2p5/4p3/1N1k4/1KR2B2/1Pp5/2r1B1b1 w - -",
  "1BK5/8/2p5/3k4/4bR2/4R3/8/2Q5 w - -",

  // Mate in 3
  "7B/3K2N1/5k2/4b2B/8/5P1N/8/8 w - -",
  "7k/3NPn2/8/4N2K/8/8/q7/8 w - -",
  "4N2k/1n2N2p/5Kpp/5p2/8/1b6/1B4n1/8 w - -",
  "8/1PPp1P2/2pPp2P/2p1Pkp1/2K3p1/3P2P1/8/8 w - -",

  // Mate in 4
  "8/1bp4B/2kbK3/Pp1R1N2/3P4/2n5/4p3/2r5 w - -",
  "k1r2q2/ppp5/6Qp/2P5/B7/5n2/5P2/1R5K w - -",
  "8/8/8/8/2Np4/3N4/k1K5/8 w - -",
  "7K/8/8/5B2/2p2B2/1pp4n/1p6/3k2rr b - -",
  "4brkn/4bp1p/3q2pP/8/2B3N1/1P4N1/2PP3P/1K2Q3 w - -",

  // Mate in 5
  "n1rb4/1p3p1p/1p6/1R5K/8/p3p1PN/1PP1R3/N6k w - - 0 1",
  "1r3n2/2pB1pq1/1bR3p1/N2NR2b/KP1kpp1r/1p2p3/1PP2Q1n/8 w - - 0 1",

  // Mate and stalemate positions
  "7k/7P/6K1/8/3B4/8/8/8 b - -",
  "8/8/8/8/8/6k1/6p1/6K1 w - -"
};

} // namespace

/// setup_bench() builds a list of UCI commands to be run by bench. There
/// are five parameters: TT size in MB, number of search threads that
/// should be used, the limit value spent for each position, a file name
/// where to look for positions in FEN format and the type of the limit:
/// depth, perft, nodes and movetime (in millisecs).
///
/// bench -> search default positions up to depth 13
/// bench 64 1 15 -> search default positions up to depth 15 (TT = 64MB)
/// bench 64 4 5000 current movetime -> search current position with 4 threads for 5 sec
/// bench 64 1 100000 default nodes -> search default positions for 100K nodes each
/// bench 16 1 5 default perft -> run a perft 5 on default positions

vector<string> setup_bench(const Position& current, istream& is) {

  vector<string> fens, list;
  string go, token;

  // Assign default values to missing arguments
  string ttSize    = (is >> token) ? token : "16";
  string threads   = (is >> token) ? token : "1";
  string limit     = (is >> token) ? token : "5";
  string fenFile   = (is >> token) ? token : "default";
  string limitType = (is >> token) ? token : "mate";

  go = "go " + limitType + " " + limit;

  if (fenFile == "default")
      fens = Defaults;

  else if (fenFile == "current")
      fens.push_back(current.fen());

  else
  {
      string fen;
      ifstream file(fenFile);

      if (!file.is_open())
      {
          cerr << "Unable to open file " << fenFile << endl;
          exit(EXIT_FAILURE);
      }

      while (getline(file, fen))
          if (!fen.empty())
              fens.push_back(fen);

      file.close();
  }

  list.emplace_back("setoption name Threads value " + threads);
  list.emplace_back("setoption name Hash value " + ttSize);
  list.emplace_back("ucinewgame");

  for (const string& fen : fens)
      if (fen.find("setoption") != string::npos)
          list.emplace_back(fen);
      else
      {
          list.emplace_back("position fen " + fen);
          list.emplace_back(go);
      }

  return list;
}
