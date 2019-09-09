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

module Car = {
  type assets = {
    straight: imageT,
    leftTurn: imageT,
    heavyLeftTurn: imageT,
    rightTurn: imageT,
    heavyRightTurn: imageT,
  };
  type state = {
    position: point,
    velocity: int,
    assets,
  };

  let draw = (state: state, env) => {
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

  let turn = (key: key, state: state) => {
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
      | NONE when state.velocity > 0 => {
          ...state,
          velocity: state.velocity - 1,
        }
      | NONE when state.velocity < 0 => {
          ...state,
          velocity: state.velocity + 1,
        }
      | _ => state
      }
    )
    |> updatePosition;
  };
};

type state = {
  car: Car.state,
  key,
  frameDelta: float,
};

let setup = env => {
  Env.size(~width, ~height, env);
  let loadImage = file => Draw.loadImage(~filename=file, ~isPixel=true, env);
  {
    car: {
      position: (width / 2 - 30, height - 60),
      velocity: 0,
      assets: {
        straight: loadImage("assets/car_1.png"),
        leftTurn: loadImage("assets/car_2.png"),
        heavyLeftTurn: loadImage("assets/car_3.png"),
        rightTurn: loadImage("assets/car_4.png"),
        heavyRightTurn: loadImage("assets/car_5.png"),
      },
    },
    key: NONE,
    frameDelta: 0.,
  };
};

let control = state => {
  let car = Car.turn(state.key, state.car);

  {...state, car};
};

let drawGame = (state, env) => {
  Draw.background(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  Car.draw(state.car, env);
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
