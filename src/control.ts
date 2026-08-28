import type { KeyCode } from "./graphics/index";

export type ControlState = {
  right: boolean;
  left: boolean;
  brake: boolean;
  reset: boolean;
};

export type Turn = "LEFT" | "P_LEFT" | "RIGHT" | "P_RIGHT" | "NONE";

export const init: ControlState = { right: false, left: false, brake: false, reset: false };

export const isLeft = (state: ControlState): boolean => state.left && !state.right;
export const isRight = (state: ControlState): boolean => state.right && !state.left;
export const isBrake = (state: ControlState): boolean => state.brake;
export const isReset = (state: ControlState): boolean => state.reset;

export const getTurn = (state: ControlState): Turn => {
  switch (true) {
    case isRight(state) && !isLeft(state) && !isBrake(state):
      return "RIGHT";
    case isRight(state) && !isLeft(state) && isBrake(state):
      return "P_RIGHT";
    case !isRight(state) && isLeft(state) && !isBrake(state):
      return "LEFT";
    case !isRight(state) && isLeft(state) && isBrake(state):
      return "P_LEFT";
    default:
      return "NONE";
  }
};

export const keyDown = (code: KeyCode, state: ControlState): ControlState => {
  switch (code) {
    case "Left":
      return { ...state, left: true };
    case "Right":
      return { ...state, right: true };
    case "Down":
      return { ...state, brake: true };
    case "Up":
    case "Space":
      return { ...state, reset: true };
    default:
      return state;
  }
};

export const keyUp = (code: KeyCode, state: ControlState): ControlState => {
  switch (code) {
    case "Left":
      return { ...state, left: false };
    case "Right":
      return { ...state, right: false };
    case "Down":
      return { ...state, brake: false };
    case "Up":
    case "Space":
      return { ...state, reset: false };
    default:
      return state;
  }
};

const breakY = 350;
const isMHardLeft = (x: number, y: number) => 250 > x && y > breakY;
const isMHardRight = (x: number, y: number) => x > 525 && y > breakY;
const isMLeft = (x: number) => 250 > x;
const isMRight = (x: number) => x > 525;

const handleCurrentPress = (x: number, y: number, state: ControlState): ControlState => {
  if (isMHardLeft(x, y)) {
    return { ...state, brake: true, left: true };
  }
  if (isMHardRight(x, y)) {
    return { ...state, brake: true, right: true };
  }
  if (isMLeft(x)) {
    return { ...state, left: true };
  }
  if (isMRight(x)) {
    return { ...state, right: true };
  }
  if (y > breakY) {
    return { ...state, brake: true };
  }
  if (60 > y) {
    return { ...state, reset: true };
  }
  return state;
};

const handleRemovePress = (x: number, y: number, state: ControlState): ControlState => {
  if (isMHardLeft(x, y)) {
    return { ...state, brake: false, left: false };
  }
  if (isMHardRight(x, y)) {
    return { ...state, brake: false, right: false };
  }
  if (isMLeft(x)) {
    return { ...state, left: false };
  }
  if (isMRight(x)) {
    return { ...state, right: false };
  }
  if (y > breakY) {
    return { ...state, brake: false };
  }
  return state;
};

export const mouseDown = (mousePos: readonly [number, number], state: ControlState): ControlState =>
  handleCurrentPress(mousePos[0], mousePos[1], state);

export const mouseUp = (mousePos: readonly [number, number], state: ControlState): ControlState =>
  handleRemovePress(mousePos[0], mousePos[1], state);

export const mouseDragged = (
  mousePos: readonly [number, number],
  previousPosition: readonly [number, number],
  state: ControlState,
): ControlState =>
  handleCurrentPress(
    mousePos[0],
    mousePos[1],
    handleRemovePress(previousPosition[0], previousPosition[1], state),
  );
