import * as Common from "./common";
import type { Env } from "./graphics/index";
import { Draw } from "./graphics/index";

export type TimerState = { remainingTime: number };

export const remainingTimeString = (state: TimerState): string =>
  String(Math.trunc(state.remainingTime / Common.frameRate));

export const addTimeInSeconds = (seconds: number, state: TimerState): TimerState => ({
  remainingTime: seconds * Common.frameRate + state.remainingTime,
});

export const reduce = (state: TimerState): TimerState =>
  0 >= state.remainingTime ? { remainingTime: 0 } : { remainingTime: state.remainingTime - 1 };

export const gameOver = (state: TimerState): boolean => 0 >= state.remainingTime;

export const init = (time = 59.5): TimerState => ({ remainingTime: time * Common.frameRate });

export const draw = (state: TimerState, env: Env): void => {
  Draw.text({ body: remainingTimeString(state), pos: [Math.trunc(Common.width / 2), 20] }, env);
};
