/* Asymptote, a UCI chess engine
   Copyright (C) 2018-2022  Maximilian Lupke

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

use std::cmp;
use std::sync;
use std::time;

use crate::eval::Score;
use crate::position::Position;
use crate::search::{Ply, MAX_PLY};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum TimeControl {
    Infinite,
    FixedMillis(u64),
    FixedDepth(Ply),
    FixedNodes(u64),
    Variable {
        wtime: u64,
        btime: u64,
        winc: Option<u64>,
        binc: Option<u64>,
        movestogo: Option<u64>,
    },
}

#[derive(Clone)]
pub struct TimeManager {
    started_at: time::Instant,
    control: TimeControl,
    searching_for_white: bool,
    pub abort: sync::Arc<sync::atomic::AtomicBool>,
    force_stop: bool,
    move_overhead: u64,

    dynamic: DynamicTimeManagement,

    times_checked: u64,
}

#[derive(Copy, Clone, Default)]
struct DynamicTimeManagement {
    maximum: u64,
    initial: u64,
    target: u64,
}

impl TimeManager {
    pub fn new(
        position: &Position,
        control: TimeControl,
        move_overhead: u64,
        abort: sync::Arc<sync::atomic::AtomicBool>,
    ) -> TimeManager {
        let mut tm = TimeManager {
            started_at: time::Instant::now(),
            control,
            searching_for_white: position.white_to_move,
            abort,
            force_stop: false,
            move_overhead,
            dynamic: DynamicTimeManagement::default(),
            times_checked: 0,
        };

        tm.update(position, control);
        tm
    }

    pub fn update(&mut self, position: &Position, control: TimeControl) {
        self.force_stop = false;
        self.started_at = time::Instant::now();
        self.control = control;
        self.searching_for_white = position.white_to_move;
        self.times_checked = 0;
        self.abort.store(false, sync::atomic::Ordering::SeqCst);

        if let TimeControl::Variable {
            wtime,
            btime,
            winc,
            binc,
            movestogo,
        } = control
        {
            let time = if self.searching_for_white {
                wtime
            } else {
                btime
            };
            let inc = if self.searching_for_white { winc } else { binc }.unwrap_or(0);

            let initial = cmp::min(time, time / movestogo.unwrap_or(40) + inc);
            let target = initial;
            let maximum = target + (time - target) / 4;

            self.dynamic.initial = initial;
            self.dynamic.target = target;
            self.dynamic.maximum = maximum;
        }
    }

    pub fn elapsed_millis(&self) -> u64 {
        let duration = time::Instant::now() - self.started_at;
        duration.as_millis() as u64
    }

    pub fn check_for_stop(&mut self) {
        if self.abort.load(sync::atomic::Ordering::Relaxed) {
            self.force_stop = true;
        }
    }

    pub fn start_another_iteration(&mut self, ply: Ply) -> bool {
        self.check_for_stop();
        if ply == MAX_PLY || self.force_stop {
            return false;
        }

        let start_another = match self.control {
            TimeControl::Infinite => true,
            TimeControl::FixedMillis(millis) => {
                let elapsed = self.elapsed_millis();
                elapsed + self.move_overhead <= millis
            }
            TimeControl::FixedDepth(stop_depth) => ply <= stop_depth,
            TimeControl::FixedNodes(_) => true, // handled by should_stop
            TimeControl::Variable { .. } => {
                let elapsed = self.elapsed_millis();
                elapsed + self.move_overhead <= self.dynamic.target / 2
            }
        };

        if !start_another {
            self.abort.store(true, sync::atomic::Ordering::Relaxed);
        }
        start_another
    }

    pub fn should_stop(&mut self) -> bool {
        self.times_checked += 1;
        if self.times_checked & 0x7F == 0 {
            self.check_for_stop();
        }

        if self.force_stop {
            return true;
        }

        let stop = match self.control {
            TimeControl::Infinite => false,
            TimeControl::FixedMillis(millis) => {
                if self.times_checked & 0x7F == 0 {
                    let elapsed = self.elapsed_millis();
                    elapsed + self.move_overhead > millis
                } else {
                    false
                }
            }
            TimeControl::FixedDepth(_) => false, // handled by start_another_iteration
            TimeControl::FixedNodes(nodes) => self.times_checked >= nodes,
            TimeControl::Variable { .. } => {
                if self.times_checked & 0x7F == 0 {
                    let elapsed = self.elapsed_millis();
                    elapsed + self.move_overhead >= self.dynamic.maximum
                } else {
                    false
                }
            }
        };

        if stop {
            self.abort.store(true, sync::atomic::Ordering::Relaxed);
        }
        stop
    }

    pub fn fail_low(&mut self, diff: Score) {
        if diff > -25 {
            return;
        }

        if diff > -75 {
            self.dynamic.target = cmp::min(self.dynamic.maximum, self.dynamic.target * 5 / 4);
        }

        self.dynamic.target = cmp::min(self.dynamic.maximum, self.dynamic.target * 3 / 2);
    }
}
