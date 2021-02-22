module Draw = Reprocessing.Draw

type assets = {
  straight: Reprocessing_Common.imageT,
  leftTurn: Reprocessing_Common.imageT,
  heavyLeftTurn: Reprocessing_Common.imageT,
  rightTurn: Reprocessing_Common.imageT,
  heavyRightTurn: Reprocessing_Common.imageT,
}

type state = {
  position: (int, int),
  speed: float,
  positionBonus: float,
  velocity: int,
  offset: float,
  assets: assets,
}

let carWidth = 105
let carHeight = 53

module Display = {
  type t = {
    position: (int, int),
    asset: Reprocessing_Common.imageT,
    width: int,
    height: int,
    z: float,
  }

  let make = car => {
    let image = switch car.velocity {
    | _ if car.velocity == 0 => car.assets.straight
    | _ if car.velocity > 6 => car.assets.heavyRightTurn
    | _ if car.velocity > 0 => car.assets.rightTurn
    | _ if car.velocity < -6 => car.assets.heavyLeftTurn
    | _ if car.velocity < 0 => car.assets.leftTurn
    | _ => car.assets.straight
    }

    {
      position: car.position,
      asset: image,
      width: carWidth,
      height: carHeight,
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
let draw = (state, env) => {
  let image = switch state.velocity {
  | _ if state.velocity == 0 => state.assets.straight
  | _ if state.velocity > 6 => state.assets.heavyRightTurn
  | _ if state.velocity > 0 => state.assets.rightTurn
  | _ if state.velocity < -6 => state.assets.heavyLeftTurn
  | _ if state.velocity < 0 => state.assets.leftTurn
  | _ => state.assets.straight
  }
  Draw.image(image, ~pos=state.position, ~width=carWidth, ~height=carHeight, env)
}

let updateOffset = (state, force) => {
  let offset = state.offset -. force
  let offset = max(offset, Common.minOffset)
  let offset = min(offset, Common.maxOffset)

  {...state, offset: offset}
}

let turn = (key: Control.turn, state: state) => {
  let updateOffsetUsingForce = s => updateOffset(s, float_of_int(s.velocity) /. 2.)
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
    let carCentre = float_of_int(carWidth) /. 2.
    let offset = state.offset

    let offRoadAdjustment = state => {
      let isOffRight = offset > 0. && offset > halfRoad
      let isOffLeft = offset < 0. && offset < halfRoad *. -1. +. carCentre
      let isOff = isOffRight || isOffLeft
      let update = state =>
        state.speed > grassMaxSpeed
          ? {...state, speed: state.speed -. 0.45}
          : {...state, speed: state.speed -. 0.1}

      isOff ? update(state) : state
      // let hillFactor = state => {}
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
    | Track.Left(force) => force *. 0.1 *. state.speed |> updateOffset(state)
    | Track.Right(force) => force *. -0.1 *. state.speed |> updateOffset(state)
    | _ => state
    }

  let hillEffect = state => {
    let effect = (incline *. 0.01)

    effect != 0. ?
    {...state, speed: state.speed -. effect} :
    state
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

let init = (x, y, env) => {
  let loadImage = file => Draw.loadImage(~filename=file, ~isPixel=true, env)
  {
    position: (x, y),
    velocity: 0,
    offset: 0.,
    speed: 0.,
    positionBonus: 0.,
    assets: {
      straight: loadImage("assets/car_1.png"),
      leftTurn: loadImage("assets/car_2.png"),
      heavyLeftTurn: loadImage("assets/car_3.png"),
      rightTurn: loadImage("assets/car_4.png"),
      heavyRightTurn: loadImage("assets/car_5.png"),
    },
  }
}
