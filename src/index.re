open Common;
open Reprocessing;

type state = {
  car: Car.state,
  road: Road.state,
  key: Types.key,
};

let setup = env => {
  Env.size(~width, ~height, env);
  {
    car: Car.init(width / 2 - 30, height - 60, env),
    road: Road.init,
    key: Types.NONE,
  };
};

let control = state => {
  let currentRoadDirection = Road.currentDirection(state.road);
  let isBrake = state.key == Types.BRAKE ? true : false;
  let car =
    Car.turn(state.key, state.car)
    |> Car.roadEffect(currentRoadDirection)
    |> Car.accelerate(isBrake);

  let position = state.road.position +. car.speed /. Common.frameRate;
  let newRoadState = Road.moveForward(position, state.road);

  {...state, car, road: newRoadState};
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
  Road.draw(state.car.offset, state.road, env);
  drawSky(env);
  Car.draw(state.car, env);
  Draw.fill(Utils.color(~r=25, ~g=25, ~b=25, ~a=255), env);

  let text = Car.speedInMph(state.car);
  let mph = "MPH";

  Draw.text(~body=text, ~pos=(420, 20), env);
  Draw.text(~body=mph, ~pos=(480, 20), env);

  state;
};

let draw = (state, env) => {
  let state = control(state);

  drawGame(state, env);
};

let keyPressed = (state, env) => {
  switch (Env.keyCode(env)) {
  | Left => {...state, key: LEFT}
  | Right => {...state, key: RIGHT}
  | Down => {...state, key: BRAKE}
  | Up => setup(env)
  | _ => state
  };
};
let keyReleased = (state, env) => {
  switch (Env.keyCode(env)) {
  | Left => {...state, key: NONE}
  | Right => {...state, key: NONE}
  | Down => {...state, key: NONE}
  | _ => state
  };
};

run(~setup, ~draw, ~keyPressed, ~keyReleased, ());
