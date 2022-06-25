/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2022 The Stockfish developers (see AUTHORS file)

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
#include <cstdio>
#include <iostream>

#include "neuralnet.h"


void NeuralNet::init(const char* filename) {

  for (int i = 0; i < HIDDEN_BIAS; i++)
      accumulator.push_back(0);

  FILE* f = fopen(filename, "rb");
  if (f != NULL)
  {    
      fread(InputWeights  , sizeof(int16_t), INPUT_WEIGHTS * HIDDEN_WEIGHTS, f);
      fread(HiddenBias    , sizeof(int16_t), HIDDEN_BIAS, f);
      fread(HiddenWeights , sizeof(int16_t), HIDDEN_WEIGHTS, f);
      fread(OutputBias    , sizeof(int32_t), OUTPUT_BIAS, f);

      fclose(f);
  }
  else
  {
      std::cout << "Network file could not be loaded!" << std::endl;
      std::exit(EXIT_FAILURE);
  }
}

void NeuralNet::reset_accumulator() {

  for (int i = 0; i < HIDDEN_BIAS; i++)
      accumulator[i] = HiddenBias[i];
}

void NeuralNet::activate(int inputSq) {

  for (int i = 0; i < HIDDEN_BIAS; i++)
      accumulator[i] += InputWeights[inputSq * HIDDEN_BIAS + i];
//      accumulator[i] = HiddenBias[i] + InputWeights[inputSq * HIDDEN_BIAS + i];
}

void NeuralNet::deactivate(int inputSq) {

  for (int i = 0; i < HIDDEN_BIAS; i++)
      accumulator[i] -= InputWeights[inputSq * HIDDEN_BIAS + i];
//      accumulator[i] = 0;
}

int NeuralNet::relu(int x) {
  return std::max(x, 0);
}

int32_t NeuralNet::output() {

  int32_t output = OutputBias[0];

  for (int i = 0; i < HIDDEN_BIAS; i++)
      output += relu(accumulator[i]) * HiddenWeights[i];

  return output / (64 * 256);
}

