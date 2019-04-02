/* Asymptote, a UCI chess engine
   Copyright (C) 2018-2019  Maximilian Lupke

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

pub struct TimeManager {
    started_at: time::Instant,
    control: TimeControl,
    searching_for_white: bool,
    pub abort: sync::Arc<sync::atomic::AtomicBool>,
    force_stop: bool,
}

impl TimeManager {
    pub fn new(
        position: &Position,
        control: TimeControl,
        abort: sync::Arc<sync::atomic::AtomicBool>,
    ) -> TimeManager {
        TimeManager {
            started_at: time::Instant::now(),
            control,
            searching_for_white: position.white_to_move,
            abort,
            force_stop: false,
        }
    }

    pub fn update(&mut self, position: &Position, control: TimeControl) {
        self.force_stop = false;
        self.started_at = time::Instant::now();
        self.control = control;
        self.searching_for_white = position.white_to_move;
        self.abort.store(false, sync::atomic::Ordering::SeqCst);
    }

    pub fn elapsed_millis(&self) -> u64 {
        let duration = time::Instant::now() - self.started_at;
        1000 * duration.as_secs() + u64::from(duration.subsec_millis())
    }

    pub fn check_for_stop(&mut self) {
        if self.abort.load(sync::atomic::Ordering::Relaxed) {
            self.force_stop = true;
        }
    }

    pub fn start_another_iteration(&self, ply: Ply) -> bool {
        if ply == MAX_PLY || self.force_stop {
            return false;
        }

        let start_another = match self.control {
            TimeControl::Infinite => true,
            TimeControl::FixedMillis(millis) => {
                let elapsed = self.elapsed_millis();
                elapsed + 10 <= millis
            }
            TimeControl::FixedDepth(stop_depth) => ply <= stop_depth,
            TimeControl::FixedNodes(_) => true, // handled by should_stop
            TimeControl::Variable {
                wtime,
                btime,
                winc,
                binc,
                movestogo,
            } => {
                let elapsed = self.elapsed_millis();
                let time = if self.searching_for_white {
                    wtime
                } else {
                    btime
                };
                let inc = if self.searching_for_white { winc } else { binc }.unwrap_or(0);
                let movestogo = movestogo.unwrap_or(40);
                elapsed <= cmp::min(time, time / movestogo + inc) / 2
            }
        };

        if !start_another {
            self.abort.store(true, sync::atomic::Ordering::Relaxed);
        }
        start_another
    }

    pub fn should_stop(&mut self, visited_nodes: u64) -> bool {
        if visited_nodes & 0x7F == 0 {
            self.check_for_stop();
        }

        if self.force_stop {
            return true;
        }

        let stop = match self.control {
            TimeControl::Infinite => false,
            TimeControl::FixedMillis(millis) => {
                if visited_nodes & 0x7F == 0 {
                    let elapsed = self.elapsed_millis();
                    return elapsed + 10 > millis;
                }
                false
            }
            TimeControl::FixedDepth(_) => false, // handled by start_another_iteration
            TimeControl::FixedNodes(nodes) => visited_nodes >= nodes,
            TimeControl::Variable {
                wtime,
                btime,
                winc,
                binc,
                movestogo,
            } => {
                if visited_nodes & 0x7F == 0 {
                    let elapsed = self.elapsed_millis();
                    let time = if self.searching_for_white {
                        wtime
                    } else {
                        btime
                    };
                    let inc = if self.searching_for_white { winc } else { binc }.unwrap_or(0);
                    let movestogo = cmp::min(10, movestogo.unwrap_or(10));
                    return elapsed >= cmp::min(time - 10, time / movestogo + inc);
                }
                false
            }
        };

        if stop {
            self.abort.store(true, sync::atomic::Ordering::Relaxed);
        }
        stop
    }
}
