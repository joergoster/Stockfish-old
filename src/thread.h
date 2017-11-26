/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
<<<<<<< HEAD
  Copyright (C) 2015-2016 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad
=======
  Copyright (C) 2015-2017 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad
>>>>>>> always_imb

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

#ifndef THREAD_H_INCLUDED
#define THREAD_H_INCLUDED

#include <atomic>
#include <condition_variable>
#include <mutex>
#include <thread>
#include <vector>

#include "material.h"
#include "movepick.h"
#include "pawns.h"
#include "position.h"
#include "search.h"
#include "thread_win32.h"


/// Thread class keeps together all the thread-related stuff. We use
/// per-thread pawn and material hash tables so that once we get a
/// pointer to an entry its life time is unlimited and we don't have
/// to care about someone changing the entry under our feet.

<<<<<<< HEAD
class Spinlock {

  std::atomic_int lock;

public:
  Spinlock() { lock = 1; } // Init here to workaround a bug with MSVC 2013
  void acquire() {
      while (lock.fetch_sub(1, std::memory_order_acquire) != 1)
          while (lock.load(std::memory_order_relaxed) <= 0)
              std::this_thread::yield(); // Be nice to hyperthreading
  }
  void release() { lock.store(1, std::memory_order_release); }
};


/// SplitPoint struct stores information shared by the threads searching in
/// parallel below the same split point. It is populated at splitting time.

struct SplitPoint {

  // Const data after split point has been setup
  const Position* pos;
  Search::Stack* ss;
  Thread* master;
  Depth depth;
  Value beta;
  int nodeType;
  bool cutNode;

  // Const pointers to shared data
  MovePicker* movePicker;
  SplitPoint* parentSplitPoint;

  // Shared variable data
  Spinlock spinlock;
  std::bitset<MAX_THREADS> slavesMask;
  volatile bool allSlavesSearching;
  volatile uint64_t nodes;
  volatile Value alpha;
  volatile Value bestValue;
  volatile Move bestMove;
  volatile int moveCount;
  volatile bool cutoff;
};


/// ThreadBase struct is the base of the hierarchy from where we derive all the
/// specialized thread classes.

struct ThreadBase : public std::thread {

  ThreadBase() { exit = false; }
  virtual ~ThreadBase() = default;
  virtual void idle_loop() = 0;
  void notify_one();
  void wait_for(std::atomic_bool& b);

  Mutex mutex;
  Spinlock spinlock;
  ConditionVariable sleepCondition;
  std::atomic_bool exit;
};


/// Thread struct keeps together all the thread related stuff like locks, state
/// and especially split points. We also use per-thread pawn and material hash
/// tables so that once we get a pointer to an entry its life time is unlimited
/// and we don't have to care about someone changing the entry under our feet.

struct Thread : public ThreadBase {

  Thread();
  virtual void idle_loop();
  bool cutoff_occurred() const;
  bool can_join(const SplitPoint* sp) const;
=======
class Thread {

  Mutex mutex;
  ConditionVariable cv;
  size_t idx;
  bool exit = false, searching = true; // Set before starting std::thread
  std::thread stdThread;
>>>>>>> always_imb

public:
  explicit Thread(size_t);
  virtual ~Thread();
  virtual void search();
  void clear();
  void idle_loop();
  void start_searching();
  void wait_for_search_finished();

  Pawns::Table pawnsTable;
  Material::Table materialTable;
  Endgames endgames;
<<<<<<< HEAD
  Position* activePosition;
  size_t idx;
  int maxPly, callsCnt;
  SplitPoint* volatile activeSplitPoint;
  volatile size_t splitPointsSize;
  std::atomic_bool searching, resetCallsCnt;
};


/// MainThread is a derived class used to characterize the main one

struct MainThread : public Thread {
  MainThread() { thinking = true; } // Avoid a race with start_thinking()
  virtual void idle_loop();
  void join();
  std::atomic_bool thinking;
=======
  size_t PVIdx;
  int selDepth;
  std::atomic<uint64_t> nodes, tbHits;

  Position rootPos;
  Search::RootMoves rootMoves;
  Depth rootDepth, completedDepth;
  CounterMoveHistory counterMoves;
  ButterflyHistory mainHistory;
  CapturePieceToHistory captureHistory;
  ContinuationHistory contHistory;
};


/// MainThread is a derived class specific for main thread

struct MainThread : public Thread {

  using Thread::Thread;

  void search() override;
  void check_time();

  bool failedLow;
  double bestMoveChanges, previousTimeReduction;
  Value previousScore;
  int callsCnt;
>>>>>>> always_imb
};


/// ThreadPool struct handles all the threads-related stuff like init, starting,
/// parking and, most importantly, launching a thread. All the access to threads
/// is done through this class.

struct ThreadPool : public std::vector<Thread*> {

<<<<<<< HEAD
  void init(); // No constructor and destructor, threads rely on globals that should
  void exit(); // be initialized and are valid during the whole thread lifetime.
=======
  void init(size_t); // No constructor and destructor, threads rely on globals that should
  void exit();       // be initialized and valid during the whole thread lifetime.
  void start_thinking(Position&, StateListPtr&, const Search::LimitsType&, bool = false);
  void set(size_t);

  MainThread* main()        const { return static_cast<MainThread*>(front()); }
  uint64_t nodes_searched() const { return accumulate(&Thread::nodes); }
  uint64_t tb_hits()        const { return accumulate(&Thread::tbHits); }
>>>>>>> always_imb

  std::atomic_bool stop, ponder, stopOnPonderhit;

<<<<<<< HEAD
  Depth minimumSplitDepth;
=======
private:
  StateListPtr setupStates;

  uint64_t accumulate(std::atomic<uint64_t> Thread::* member) const {

    uint64_t sum = 0;
    for (Thread* th : *this)
        sum += (th->*member).load(std::memory_order_relaxed);
    return sum;
  }
>>>>>>> always_imb
};

extern ThreadPool Threads;

#endif // #ifndef THREAD_H_INCLUDED
