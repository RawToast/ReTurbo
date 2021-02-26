type state = {
  speed: float,
  positionBonus: float,
  velocity: int,
  offset: float,
}
// 68, 34
let carWidth = 110
let carHeight = 55

module Display = {
  type asset =
    | Straight
    | LeftTurn
    | HeavyLeftTurn
    | RightTurn
    | HeavyRightTurn

  type t = {
    offset: float,
    width: float,
    height: float,
    asset: asset,
    z: float,
  }

  let make = car => {
    let asset = switch car.velocity {
    | _ if car.velocity == 0 => Straight
    | _ if car.velocity > 6 => HeavyRightTurn
    | _ if car.velocity > 0 => RightTurn
    | _ if car.velocity < -6 => HeavyLeftTurn
    | _ if car.velocity < 0 => LeftTurn
    | _ => Straight
    }

    {
      offset: car.offset,
      asset: asset,
      width: float_of_int(carWidth),
      height: float_of_int(carHeight),
      z: 1.,
    }
  }
}

let vLowSpeed = 90.
let lowSpeed = 110.
let midSpeed = 160.
let highSpeed = 220.
let vHighSpeed = 260.

let grassMaxSpeed = 100.

let maxSpeed = 250.
/* 60-0 in 3 seconds */
let brakeFactor = 60. *. 1.6 /. (Common.frameRate *. 3.)

let speedInMph = state => state.speed /. 1.6 |> int_of_float |> string_of_int

let updateOffset = (state, force) => {
  let offset = state.offset -. force
  let offset = max(offset, Common.minOffset)
  let offset = min(offset, Common.maxOffset)

  {...state, offset: offset}
}

let turn = (key: Control.turn, state: state) => {
  let updateOffsetUsingForce = s => updateOffset(s, float_of_int(s.velocity) /. 700.)
  let highSpeed = state.speed > 176. && state.speed < 200.
  let vHighSpeed = state.speed > 200.
  let updateVelocity = amount => {
    ...state,
    velocity: state.velocity + amount,
  }
  switch key {
  | LEFT if state.velocity > -12 && vHighSpeed => updateVelocity(-1)
  | LEFT if state.velocity > -13 && highSpeed => updateVelocity(-1)
  | LEFT if state.velocity > -14 => updateVelocity(-1)
  | P_LEFT if state.velocity > -16 && vHighSpeed => updateVelocity(-2)
  | P_LEFT if state.velocity > -14 => updateVelocity(-2)
  | RIGHT if state.velocity < 12 && vHighSpeed => updateVelocity(1)
  | RIGHT if state.velocity < 13 && highSpeed => updateVelocity(1)
  | RIGHT if state.velocity < 14 => updateVelocity(1)
  | P_RIGHT if state.velocity < 16 && vHighSpeed => updateVelocity(2)
  | P_RIGHT if state.velocity < 14 => updateVelocity(2)
  | _ if state.velocity > 0 => updateVelocity(-1)
  | _ if state.velocity < 0 => updateVelocity(1)
  | _ => state
  } |> updateOffsetUsingForce
}

let progression = state =>
  0. >= state.speed ? 0. : state.speed *. (1. +. state.positionBonus /. 100.) /. 25.

let roadEffect = (direction, incline, state) => {
  /* Current max velocity is 6, curves are from 0.08 to 0.6 */
  let offTrack = state => {
    let halfRoad = Common.roadWidth /. 2.
    let carCentre = float_of_int(carWidth) /. 2.1
    let offset = state.offset

    let offRoadAdjustment = state => {
      let cameraDepth = Common.cameraDepth
      let scale = cameraDepth // /. 8.
      let isOffLeft = (offset > 0. && offset > 1.) || (offset < 0. && offset < -1.)
      let isOffRight =
        (offset < 0. && offset < -1. +. carCentre) || (offset > 0. && offset > 1. +. carCentre)

      let offRoadFactor = switch (isOffLeft, isOffRight) {
      | (true, true) => 1.
      | (false, false) => 0.3
      | _ => 0.
      }
      let isOff = isOffRight || isOffLeft
      let update = state =>
        state.speed > grassMaxSpeed
          ? {...state, speed: state.speed -. offRoadFactor *. 0.8}
          : {...state, speed: state.speed -. offRoadFactor *. 0.1}

      isOff ? update(state) : state
    }
    state |> offRoadAdjustment
  }

  let apexBonus = state => {
    let offset = state.offset
    let initBonus = switch direction {
    | Track.Right(t) => (0. -. t) *. offset /. 22.
    | Track.Left(t) => t *. offset /. 22.
    | _ => 0.
    }

    let positionBonus = switch initBonus {
    | _ if initBonus > 5. => 5.
    | _ if initBonus < -5. => -5.
    | 0. => 0.
    | _ => initBonus
    }

    {...state, positionBonus: positionBonus}
  }

  let cornerEffect = state =>
    switch direction {
    | Track.Left(force) => force *. 0.1 *. state.speed /. 350. |> updateOffset(state)
    | Track.Right(force) => force *. -0.1 *. state.speed /. 350. |> updateOffset(state)
    | _ => state
    }

  let hillEffect = state => {
    let effect = incline *. 0.02

    effect != 0. ? {...state, speed: state.speed -. effect} : state
  }

  state |> cornerEffect |> offTrack |> apexBonus |> hillEffect
}

let accelerate = (isBrake, state) => {
  let accel = switch state.speed {
  | _ if maxSpeed == state.speed => maxSpeed
  | _ if vLowSpeed > state.speed => log((highSpeed -. state.speed) /. 6.) /. 8.
  | _ if lowSpeed > state.speed => log((highSpeed -. state.speed) /. 8.) /. 12.
  | _ if midSpeed > state.speed => log((vHighSpeed -. state.speed) /. 10.) /. 20.
  | _ if highSpeed > state.speed => log((maxSpeed -. state.speed) /. 12.) /. 22.
  | _ if vHighSpeed > state.speed => log((maxSpeed -. state.speed) /. 14.) /. 25.
  | _ => log((maxSpeed -. state.speed) /. 16.) /. 25.
  }

  let speed = isBrake ? max(0., state.speed -. brakeFactor) : state.speed +. accel
  let speed = max(0., speed)
  let speed = min(maxSpeed, speed)

  {...state, speed: speed}
}

let init = {
  velocity: 0,
  offset: 0.,
  speed: 0.,
  positionBonus: 0.,
}
