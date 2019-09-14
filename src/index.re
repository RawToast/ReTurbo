open Reprocessing;

type point = (int, int);
type key =
  | LEFT
  | RIGHT
  | NONE;

let frameRate = 25.0;
let maxFrameDelta = 1.0 /. (frameRate *. 60.);
let height = 320;
let width = 568;

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

module Road = {
  let float = x => float_of_int(x);
  let baseHeight = float(40);
  let baseWidth = float(400);
  let maxHeight = float(height / 2);
  let fillDarkGrey = Draw.fill(Utils.color(~r=65, ~g=65, ~b=65, ~a=255));
  let fillLightGrey = Draw.fill(Utils.color(~r=90, ~g=90, ~b=90, ~a=255));

  let draw = (_state, env) => {
    let x0 = (float(width) -. baseWidth) /. 2.;
    let x1 = x0 +. baseWidth;
    let y0 = float(height);

    let rec drawRoad = (p1, p2, lastLength, isDark, env) => {
      let (x0, y0) = p1;
      let (x1, _) = p2;

      if (isDark) {
        fillDarkGrey(env);
      } else {
        fillLightGrey(env);
      };

      let length = {
        let nextLength = lastLength /. 4. *. 3.;
        if (2. > nextLength) {
          2.;
        } else {
          nextLength;
        };
      };

      let dx = length *. sin(40.);
      let dy = 0. -. length *. cos(40.);

      let y1 = y0 -. dy;
      let xx0 = x1 -. dx;
      let xx1 = x0 +. dx;
      Draw.quadf(~p1, ~p2, ~p3=(xx0, y1), ~p4=(xx1, y1), env);

      if (maxHeight > y1) {
        env;
      } else {
        drawRoad((xx1, y1), (xx0, y1), length, !isDark, env);
      };
    };

    Draw.fill(Utils.color(~r=20, ~g=150, ~b=20, ~a=255), env);
    Draw.quad(
      ~p1=(0, height),
      ~p2=(width, height),
      ~p3=(width, height / 2),
      ~p4=(0, height / 2),
      env,
    );

    env
    |> drawRoad((x0, y0), (x1, y0), 80., true)
    |> Draw.fill(Utils.color(~r=5, ~g=5, ~b=200, ~a=255));

    Draw.quad(
      ~p1=(0, 0),
      ~p2=(width, 0),
      ~p3=(width, height / 2),
      ~p4=(0, height / 2),
      env,
    );
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
  Road.draw(state, env);
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
