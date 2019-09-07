open Reprocessing;

type point = (int, int);
type key =
  | LEFT
  | RIGHT
  | NONE;

let frameRate = 25.0;
let maxFrameDelta = 1.0 /. (frameRate *. 60.);
let height = 568;
let width = 320;

type state = {
  player: point,
  key,
  velocity: int,
  frameDelta: float,
};

let setup = env => {
  Env.size(~width, ~height, env);
  { 
    player: (width / 2 - 30, height - 40),
    key: NONE,
    velocity: 0,
    frameDelta: 0.,
  };
};

let control = state => {
  let vel = state.velocity;
  let (px, py) = state.player;

  let state =
    switch (state.key) {
    | LEFT when vel > (-6) => {...state, velocity: vel - 1}
    | RIGHT when vel < 6 => {...state, velocity: vel + 1}
    | NONE when vel > 0 => {...state, velocity: vel - 1}
    | NONE when vel < 0 => {...state, velocity: vel + 1}
    | _ => state
    };

  {...state, player: (px + state.velocity, py)};
};

let drawGame = (state, env) => {
  Draw.background(Utils.color(~r=255, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  Draw.rect(~pos=state.player, ~width=60, ~height=30, env);
  state;
};

let draw = (state, env) => {
  let frameTime = state.frameDelta +. Env.deltaTime(env);
  if (frameTime > maxFrameDelta) {
    /* Control */
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
