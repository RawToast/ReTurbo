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
  velocity: int,
  offset: float,
  assets,
};
let carWidth = 105;
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
  Draw.image(image, ~pos=state.position, ~width=carWidth, ~height=51, env);
};

let updateOffset = (state, force) => {
  let offset = state.offset -. force;
  let offset = max(offset, Common.minOffset);
  let offset = min(offset, Common.maxOffset);

  {...state, offset};
};

let turn = (key: Types.key, state: state) => {
  let updateOffsetUsingForce = s =>
    updateOffset(s, float_of_int(s.velocity) /. 2.);

  (
    switch (key) {
    | LEFT when state.velocity > (-12) => {
        ...state,
        velocity: state.velocity - 1,
      }
    | RIGHT when state.velocity < 12 => {
        ...state,
        velocity: state.velocity + 1,
      }
    | NONE when state.velocity > 0 => {...state, velocity: state.velocity - 1}
    | NONE when state.velocity < 0 => {...state, velocity: state.velocity + 1}
    | _ => state
    }
  )
  |> updateOffsetUsingForce;
};

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

    isOff ? reduce(state) : state;
  };

  (
    switch (direction) {
    | Track.Left(force) => force *. 2.5 *. 0.04 *. state.speed |> update
    | Track.Right(force) => force *. 2.5 *. (-0.04) *. state.speed |> update
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
      log((highSpeed -. state.speed) /. 6.) /. 10.
    | _ when lowSpeed > state.speed =>
      log((highSpeed -. state.speed) /. 8.) /. 15.
    | _ when midSpeed > state.speed =>
      log((vHighSpeed -. state.speed) /. 10.) /. 25.
    | _ when highSpeed > state.speed =>
      log((maxSpeed -. state.speed) /. 12.) /. 25.
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
    assets: {
      straight: loadImage("assets/car_1.png"),
      leftTurn: loadImage("assets/car_2.png"),
      heavyLeftTurn: loadImage("assets/car_3.png"),
      rightTurn: loadImage("assets/car_4.png"),
      heavyRightTurn: loadImage("assets/car_5.png"),
    },
  };
};
