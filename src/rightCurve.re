let height = 320.;
let width = 568.;

let centrePoint = 284.;
let finalWidth = 0.

open Reprocessing;

let baseWidth = 520.;
let maxHeight = height /. 2.;
let fillDarkGrey = Draw.fill(Utils.color(~r=65, ~g=65, ~b=65, ~a=255));
let fillLightGrey = Draw.fill(Utils.color(~r=80, ~g=80, ~b=80, ~a=255));
let baseLength = 40.;

type state = {
  position: float
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

let rec drawRoad = (leftBottom, rightBottom, firstHeight, count, goals, isDark, env) => {
  let (x0, y0) = leftBottom;
  let (x1, _) = rightBottom;

  isDark ? fillDarkGrey(env) : fillLightGrey(env);

  let y1 = if (count == 0) {
    height -. baseLength +. firstHeight;
  } else {
    nextY(y0)
  };
  let (gl, gr) = goals;

  let nextGoalL = gl + int_of_float((height -. y1) /. 6.);
  let nextGoalR = gr + int_of_float((320. -. y1) /. 6.);

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

  if (maxHeight >= y1) {
    ();
  } else {
    drawRoad((leftX, y1), (rightX, y1), firstHeight, count + 1, (nextGoalL, nextGoalR), !isDark, env);
  };
};
/* default 48. */
let lAngle = 48.
let rAngle = 48.

let findInitialCoordinates = state => {
  let (isLight, rem) = {
    let adj = mod_float(state.position, baseLength *. 2.);
    adj >= baseLength ? (true, adj -. baseLength) : (false, adj);
  };
  let x0 = width /. 2. -. baseWidth /. 2.;
  let x1 = width /. 2. +. baseWidth /. 2.;
  let y1 = rem; /* 1 */
  (x0, x1, y1, isLight);
};

let draw = (state, env) => {
  let (x0, x1, y1, isLight) = findInitialCoordinates(state);
  let goal = (244, 324);
  
  drawRoad((x0, height), (x1, height), y1, 0, goal, isLight, env);
};
