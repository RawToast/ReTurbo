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
  assets,
};

let vLowSpeed = 75.;
let lowSpeed = 90.;
let midSpeed = 135.;
let highSpeed = 190.;
let vHighSpeed = 225.;
let maxSpeed = 250.;

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
  Draw.image(image, ~pos=state.position, ~width=105, ~height=51, env);
};

let turn = (key: Types.key, state: state) => {
  let (x, y) = state.position;
  let updatePosition = s => {...s, position: (x + s.velocity / 2, y)};

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
  |> updatePosition;
};

let accelerate = state => {
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
      log((maxSpeed -. state.speed) /. 16.) /. 25.
    | _ when vHighSpeed > state.speed =>
      log((maxSpeed -. state.speed) /. 18.) /. 25.
    | _ => log((maxSpeed -. state.speed) /. 19.) /. 25.
    };
  let speed = state.speed +. accel;

  {...state, speed};
};

let init = (x, y, env) => {
  let loadImage = file => Draw.loadImage(~filename=file, ~isPixel=true, env);
  {
    position: (x, y),
    velocity: 0,
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
