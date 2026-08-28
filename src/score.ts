import type { Env } from "./graphics/index";
import { Draw } from "./graphics/index";

export type ScoreState = { score: number };

export const scoreString = (state: ScoreState): string => String(Math.trunc(state.score / 10));

export const increment = (x: number, state: ScoreState): ScoreState => ({ score: state.score + x });

export const init: ScoreState = { score: 0 };

export const draw = (state: ScoreState, env: Env): void => {
  Draw.text({ body: scoreString(state), pos: [60, 20] }, env);
};
