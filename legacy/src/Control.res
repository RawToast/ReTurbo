type state = {
  right: bool,
  left: bool,
  brake: bool,
  reset: bool,
}

type turn =
  | LEFT
  | P_LEFT
  | RIGHT
  | P_RIGHT
  | NONE

let init = {right: false, left: false, brake: false, reset: false}
let isLeft = state => state.left && !state.right
let isRight = state => state.right && !state.left
let isBrake = state => state.brake
let isReset = state => state.reset

let getTurn = state =>
  switch (isRight(state), isLeft(state), isBrake(state)) {
  | (true, false, false) => RIGHT
  | (true, false, true) => P_RIGHT
  | (false, true, false) => LEFT
  | (false, true, true) => P_LEFT
  | _ => NONE
  }

open Reprocessing_Events
let keyDown = (code, state) =>
  switch code {
  | Left => {...state, left: true}
  | Right => {...state, right: true}
  | Down => {...state, brake: true}
  | Up => {...state, reset: true}
  | _ => state
  }

let keyUp = (code, state) =>
  switch code {
  | Left => {...state, left: false}
  | Right => {...state, right: false}
  | Down => {...state, brake: false}
  | Up => {...state, reset: false}
  | _ => state
  }

let breakY = 350
let isMHardLeft = (x, y) => 250 > x && y > breakY
let isMHardRight = (x, y) => x > 525 && y > breakY
let isMLeft = x => 250 > x
let isMRight = x => x > 525

let handleCurrentPress = (x, y, state) =>
  switch (x, y) {
  | _ if isMHardLeft(x, y) => {...state, brake: true, left: true}
  | _ if isMHardRight(x, y) => {...state, brake: true, right: true}
  | _ if isMLeft(x) => {...state, left: true}
  | _ if isMRight(x) => {...state, right: true}
  | _ if y > breakY => {...state, brake: true}
  | _ if 60 > y => {...state, reset: true}
  | _ => state
  }

let handleRemovePress = (x, y, state) =>
  switch (x, y) {
  | _ if isMHardLeft(x, y) => {...state, brake: false, left: false}
  | _ if isMHardRight(x, y) => {...state, brake: false, right: false}
  | _ if isMLeft(x) => {...state, left: false}
  | _ if isMRight(x) => {...state, right: false}
  | _ if y > breakY => {...state, brake: false}
  | _ => state
  }

let mouseDown = (mousePos, state) => {
  let (x, y) = mousePos
  handleCurrentPress(x, y, state)
}

let mouseUp = (mousePos, state) => {
  let (x, y) = mousePos
  handleRemovePress(x, y, state)
}

let mouseDragged = (mousePos, previousPosition, state) => {
  let (x, y) = mousePos
  let (px, py) = previousPosition

  state |> handleRemovePress(px, py) |> handleCurrentPress(x, y)
}
