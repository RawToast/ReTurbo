open Reprocessing;

type assets = {
  straight: Reprocessing_Common.imageT,
  leftTurn: Reprocessing_Common.imageT,
  heavyLeftTurn: Reprocessing_Common.imageT,
  rightTurn: Reprocessing_Common.imageT,
  heavyRightTurn: Reprocessing_Common.imageT,
};

type state = {
  position: (int, int),
  velocity: int,
  assets,
};

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

let init = (x, y, env) => {
  let loadImage = file => Draw.loadImage(~filename=file, ~isPixel=true, env);
  {
    position: (x, y),
    velocity: 0,
    assets: {
      straight: loadImage("assets/car_1.png"),
      leftTurn: loadImage("assets/car_2.png"),
      heavyLeftTurn: loadImage("assets/car_3.png"),
      rightTurn: loadImage("assets/car_4.png"),
      heavyRightTurn: loadImage("assets/car_5.png"),
    },
  };
};
