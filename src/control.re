type state = {
  right: bool,
  left: bool,
  brake: bool,
  reset: bool,
};

type turn =
  | LEFT
  | P_LEFT
  | RIGHT
  | P_RIGHT
  | NONE;

let init = {right: false, left: false, brake: false, reset: false};
let isLeft = state => state.left && !state.right;
let isRight = state => state.right && !state.left;
let isBrake = state => state.brake;
let isReset = state => state.reset;

let getTurn = state =>
  switch (isRight(state), isLeft(state), isBrake(state)) {
  | (true, false, false) => RIGHT
  | (true, false, true) => P_RIGHT
  | (false, true, false) => LEFT
  | (false, true, true) => P_LEFT
  | _ => NONE
  };

open Reprocessing_Events;
let keyDown = (code, state) =>
  switch (code) {
  | Left => {...state, left: true}
  | Right => {...state, right: true}
  | Down => {...state, brake: true}
  | Up => {...state, reset: true}
  | _ => state
  };

let keyUp = (code, state) =>
  switch (code) {
  | Left => {...state, left: false}
  | Right => {...state, right: false}
  | Down => {...state, brake: false}
  | Up => {...state, reset: false}
  | _ => state
  };
