let height = 320.;
let width = 568.;

let centrePoint = 284.;
let finalWidth = 0.;

open Reprocessing;

let baseWidth = 520.;
let maxHeight = height /. 2.;
let fillDarkGrey = Draw.fill(Utils.color(~r=65, ~g=65, ~b=65, ~a=255));
let fillLightGrey = Draw.fill(Utils.color(~r=80, ~g=80, ~b=80, ~a=255));
let baseLength = 40.;

type state = {
  position: float,
  lastPiece: int,
  track: Track.state,
};

let moveForward = (newPosition, state) =>
  if (float_of_int(state.lastPiece) *. baseLength -. newPosition <= 0.) {
    {
      lastPiece: state.lastPiece + 1,
      position: newPosition,
      track: Track.progress(state.track),
    };
  } else {
    {...state, position: newPosition};
  };
let nextY = currentY =>
  if (currentY >= 320.) {
    currentY -. baseLength;
  } else {
    let yDelta = 160. /. baseLength;
    let revY = 0. -. (currentY -. 320.);
    let height = baseLength -. revY /. yDelta;
    let delta = height > 6. ? height : 6.;
    currentY -. delta;
  };

let calcDeltaX = (yDistance, angle) => {
  let fortySevenRadians = Util.toRadians(angle);
  yDistance *. tan(fortySevenRadians);
};

let rec drawRoad =
        (
          leftBottom,
          rightBottom,
          firstHeight,
          track: list(Track.direction),
          goals,
          isDark,
          env,
        ) => {
  let (x0, y0) = leftBottom;
  let (x1, _) = rightBottom;

  isDark ? fillDarkGrey(env) : fillLightGrey(env);

  let y1 =
    if (y0 == height) {
      height -. baseLength +. firstHeight;
    } else {
      nextY(y0);
    };
  let (gl, gr) = goals;

  /**
   * CurveStength directs the road.
   * 0 == Straight
   * 0.1 == Easy Right
   * 0.2 == Med Right, etc  (btw 1/6 is good too), 1 is super tight
   * -0.1 == Simple Left
   */
  let trackPiece = List.hd(track);

  let curveStength =
    switch (trackPiece) {
    | Track.Straight => 0.0
    | Track.Left => (-0.16)
    | Track.LeftMedium => (-0.2)
    | Track.LeftHard => (-0.33)
    | Track.LeftK => (-0.6)
    | Track.Right => 0.16
    | Track.RightMedium => 0.2
    | Track.RightHard => 0.33
    | Track.RightK => 0.6
    };
  let nextGoalL = gl + int_of_float((height -. y1) *. curveStength);
  let nextGoalR = gr + int_of_float((height -. y1) *. curveStength);

  let (rightX, leftX) = {
    let opposite = y0 -. maxHeight;
    let adjacentL = float_of_int(nextGoalL) -. x0;
    let adjacentR = x1 -. float_of_int(nextGoalR);
    let leftAngleRadians = atan(opposite /. adjacentL);
    let rightAngleRadians = atan(opposite /. adjacentR);

    let left = (y0 -. y1) /. tan(leftAngleRadians);
    let right = (y0 -. y1) /. tan(rightAngleRadians);

    (x1 -. right, x0 +. left);
  };

  Draw.quadf(
    ~p1=leftBottom,
    ~p2=rightBottom,
    ~p3=(rightX, y1),
    ~p4=(leftX, y1),
    env,
  );

  let isOutOfBounds = maxHeight >= y1 || x1 < 0. || x0 > width;
  if (isOutOfBounds) {
    ();
  } else {
    drawRoad(
      (leftX, y1),
      (rightX, y1),
      firstHeight,
      List.tl(track),
      (nextGoalL, nextGoalR),
      !isDark,
      env,
    );
  };
};

let findInitialCoordinates = state => {
  let (isLight, rem) = {
    let adj = mod_float(state.position, baseLength *. 2.);
    adj >= baseLength ? (true, adj -. baseLength) : (false, adj);
  };
  let x0 = width /. 2. -. baseWidth /. 2.;
  let x1 = width /. 2. +. baseWidth /. 2.;
  (x0, x1, rem, isLight);
};

let init = {position: 0., track: Track.init, lastPiece: 1};

let draw = (state, env) => {
  let (x0, x1, remainder, isLight) = findInitialCoordinates(state);
  let goal = (244, 324);

  drawRoad(
    (x0, height),
    (x1, height),
    remainder,
    state.track.track,
    goal,
    isLight,
    env,
  );
};
