open Reprocessing;

type break = bool;

let frameRate = 25.0;
let maxFrameDelta = 1.0 /. (frameRate *. 60.);
let height = 320;
let width = 568;

let maxSpeed = 170.;

type state = {
  car: Car.state,
  speed: float,
  road: Road.state,
  key: Types.key,
  frameDelta: float,
};

let setup = env => {
  Env.size(~width, ~height, env);
  {
    car: Car.init(width / 2 - 30, height - 60, env),
    speed: 0.,
    road: {
      position: 0.,
    },
    key: Types.NONE,
    frameDelta: 0.,
  };
};

let control = state => {
  let car = Car.turn(state.key, state.car);

  let vLowSpeed = 30.;
  let lowSpeed = 60.;
  let midSpeed = 90.;
  let highSpeed = 130.;
  let vHighSpeed = 150.;
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
  let position = state.road.position +. speed /. 25.;

  {
    ...state,
    car,
    speed,
    road: {
      position: position,
    },
  };
};

let drawGound = env => {
  Draw.fill(Utils.color(~r=20, ~g=150, ~b=20, ~a=255), env);
  Draw.quad(
    ~p1=(0, height),
    ~p2=(width, height),
    ~p3=(width, height / 2),
    ~p4=(0, height / 2),
    env,
  );
};
let drawSky = env => {
  Draw.fill(Utils.color(~r=5, ~g=5, ~b=200, ~a=255), env);
  Draw.quad(
    ~p1=(0, 0),
    ~p2=(width, 0),
    ~p3=(width, height / 2),
    ~p4=(0, height / 2),
    env,
  );
};

let drawGame = (state, env) => {
  Draw.background(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
  drawGound(env);
  Road.draw(state.road, env);
  drawSky(env);
  Car.draw(state.car, env);
  Draw.fill(Utils.color(~r=25, ~g=25, ~b=25, ~a=255), env);

  let text = string_of_int(int_of_float(state.speed));
  let mph = "MPH";

  Draw.text(~body=text, ~pos=(420, 20), env);
  Draw.text(~body=mph, ~pos=(480, 20), env);

  state;
};

let draw = (state, env) => {
  let frameTime = state.frameDelta +. Env.deltaTime(env);
  if (frameTime > maxFrameDelta) {
    let state = control(state);

    let state = {...state, frameDelta: frameTime -. maxFrameDelta};
    drawGame(state, env);
  } else {
    drawGame(state, env);
  };
};

let keyPressed = (state, env) => {
  switch (Env.keyCode(env)) {
  | Left => {...state, key: LEFT}
  | Right => {...state, key: RIGHT}
  | Down => setup(env)
  | _ => state
  };
};
let keyReleased = (state, env) => {
  switch (Env.keyCode(env)) {
  | Left => {...state, key: NONE}
  | Right => {...state, key: NONE}
  | _ => state
  };
};

run(~setup, ~draw, ~keyPressed, ~keyReleased, ());
