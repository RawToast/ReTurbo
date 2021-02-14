module Draw = Reprocessing.Draw;

type assets = {
  straight: Reprocessing_Common.imageT,
  leftTurn: Reprocessing_Common.imageT,
  heavyLeftTurn: Reprocessing_Common.imageT,
  rightTurn: Reprocessing_Common.imageT,
  heavyRightTurn: Reprocessing_Common.imageT,
};

type state = {
  position: (int, int),
  speed: float,
  positionBonus: float,
  velocity: int,
  offset: float,
  assets,
};
let carWidth = 105;
let carHeight = 53;
let vLowSpeed = 90.;
let lowSpeed = 110.;
let midSpeed = 160.;
let highSpeed = 220.;
let vHighSpeed = 260.;

let grassMaxSpeed = 100.;

let maxSpeed = 250.;
/* 60-0 in 3 seconds */
let brakeFactor = 60. *. 1.6 /. (Common.frameRate *. 3.);

let speedInMph = state => state.speed /. 1.6 |> int_of_float |> string_of_int;
let draw = (state, env) => {
  let image =
    switch (state.velocity) {
    | _ when state.velocity == 0 => state.assets.straight
    | _ when state.velocity > 6 => state.assets.heavyRightTurn
    | _ when state.velocity > 0 => state.assets.rightTurn
    | _ when state.velocity < (-6) => state.assets.heavyLeftTurn
    | _ when state.velocity < 0 => state.assets.leftTurn
    | _ => state.assets.straight
    };
  Draw.image(image, ~pos=state.position, ~width=carWidth, ~height=carHeight, env);
};

let updateOffset = (state, force) => {
  let offset = state.offset -. force;
  let offset = max(offset, Common.minOffset);
  let offset = min(offset, Common.maxOffset);

  {...state, offset};
};

let turn = (key: Control.turn, state: state) => {
  let updateOffsetUsingForce = s =>
    updateOffset(s, float_of_int(s.velocity) /. 2.);
  let highSpeed = state.speed > 176. && state.speed < 200.;
  let vHighSpeed = state.speed > 200.;
  let updateVelocity = amount => {
    ...state,
    velocity: state.velocity + amount,
  };
  (
    switch (key) {
    | LEFT when state.velocity > (-12) && vHighSpeed => updateVelocity(-1)
    | LEFT when state.velocity > (-13) && highSpeed => updateVelocity(-1)
    | LEFT when state.velocity > (-14) => updateVelocity(-1)
    | P_LEFT when state.velocity > (-16) && vHighSpeed => updateVelocity(-2)
    | P_LEFT when state.velocity > (-14) => updateVelocity(-2)
    | RIGHT when state.velocity < 12 && vHighSpeed => updateVelocity(1)
    | RIGHT when state.velocity < 13 && highSpeed => updateVelocity(1)
    | RIGHT when state.velocity < 14 => updateVelocity(1)
    | P_RIGHT when state.velocity < 16 && vHighSpeed => updateVelocity(2)
    | P_RIGHT when state.velocity < 14 => updateVelocity(2)
    | _ when state.velocity > 0 => updateVelocity(-1)
    | _ when state.velocity < 0 => updateVelocity(1)
    | _ => state
    }
  )
  |> updateOffsetUsingForce;
};

let progression = state =>
  0. >= state.speed
    ? 0. : state.speed *. (1. +. state.positionBonus /. 100.) /. 25.;

let roadEffect = (direction, state) => {
  /* Current max velocity is 6, curves are from 0.08 to 0.6 */
  let update = updateOffset(state);
  let offTrack = state => {
    let halfRoad = Common.roadWidth /. 2.;
    let carCentre = float_of_int(carWidth) /. 2.;
    let offset = state.offset;

    let isOffRight = offset > 0. && offset > halfRoad;
    let isOffLeft = offset < 0. && offset < halfRoad *. (-1.) +. carCentre;
    let isOff = isOffRight || isOffLeft;

    let reduce = state => {
      state.speed > grassMaxSpeed
        ? {...state, speed: state.speed -. 0.45}
        : {...state, speed: state.speed -. 0.1};
    };

    let grantBonus = state => {
      let initBonus =
        switch (direction) {
        | Track.Right(t) => (0. -. t) *. offset /. 22.
        | Track.Left(t) => t *. offset /. 22.
        | _ => 0.
        };

      let positionBonus =
        switch (initBonus) {
        | _ when initBonus > 5. => 5.
        | _ when initBonus < (-5.) => (-5.)
        | 0. => 0.
        | _ => initBonus
        };

      {...state, positionBonus};
    };

    (isOff ? reduce(state) : state) |> grantBonus;
  };

  (
    switch (direction) {
    | Track.Left(force) => force *. 0.1 *. state.speed |> update
    | Track.Right(force) => force *. (-0.1) *. state.speed |> update
    | _ => state
    }
  )
  |> offTrack;
};

let accelerate = (isBrake, state) => {
  let accel =
    switch (state.speed) {
    | _ when maxSpeed == state.speed => maxSpeed
    | _ when vLowSpeed > state.speed =>
      log((highSpeed -. state.speed) /. 6.) /. 8.
    | _ when lowSpeed > state.speed =>
      log((highSpeed -. state.speed) /. 8.) /. 12.
    | _ when midSpeed > state.speed =>
      log((vHighSpeed -. state.speed) /. 10.) /. 20.
    | _ when highSpeed > state.speed =>
      log((maxSpeed -. state.speed) /. 12.) /. 22.
    | _ when vHighSpeed > state.speed =>
      log((maxSpeed -. state.speed) /. 14.) /. 25.
    | _ => log((maxSpeed -. state.speed) /. 16.) /. 25.
    };

  let speed =
    isBrake ? max(0., state.speed -. brakeFactor) : state.speed +. accel;
  let speed = max(0., speed);
  let speed = min(maxSpeed, speed);

  {...state, speed};
};

let init = (x, y, env) => {
  let loadImage = file => Draw.loadImage(~filename=file, ~isPixel=true, env);
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
  };
};
