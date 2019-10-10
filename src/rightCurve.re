let height = 320.;
let width = 568.;

let centrePoint = 284.;
let finalWidth = 40.

open Reprocessing;

let baseWidth = 400.;
let maxHeight = height /. 2.;
let fillDarkGrey = Draw.fill(Utils.color(~r=65, ~g=65, ~b=65, ~a=255));
let fillLightGrey = Draw.fill(Utils.color(~r=90, ~g=90, ~b=90, ~a=255));
let baseLength = 60.;

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
    let delta = height > 1. ? height : 1.;
    currentY -. delta;
  };

let calcDeltaX = (yDistance, angle) => {
  let fortySevenRadians = Util.toRadians(angle);
  yDistance *. tan(fortySevenRadians);
};

let rec drawRoad = (leftBottom, rightBottom, firstHeight, first, lAngle, rAngle, isDark, env) => {
  let (x0, y0) = leftBottom;
  let (x1, _) = rightBottom;

  isDark ? fillDarkGrey(env) : fillLightGrey(env);

  let y1 = if (first) {
    height -. baseLength +. firstHeight;
  } else {
    nextY(y0)
  }

  let leftAngle = lAngle +. 2.;
  let rightAngle = rAngle -. 3.;

  let (rightX, leftX) = {
    let deltaXL = calcDeltaX(y0 -. y1, leftAngle);
    let deltaXR = calcDeltaX(y0 -. y1, rightAngle);
    (x1 -. deltaXR, x0 +. deltaXL);
  };

  Draw.quadf(
    ~p1=leftBottom,
    ~p2=rightBottom,
    ~p3=(rightX, y1),
    ~p4=(leftX, y1),
    env,
  );

  if (maxHeight >= y0) {
    (leftAngle, rightAngle);
  } else {
    drawRoad((leftX, y1), (rightX, y1), firstHeight, false, leftAngle, rightAngle, !isDark, env);
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

  let la = lAngle;
  let ra = rAngle;

  let deltaXLeft = calcDeltaX(rem, lAngle);
  let deltaXRight = calcDeltaX(rem, rAngle);

  let x0 = width /. 2. -. baseWidth /. 2.;
  let x1 = width /. 2. +. baseWidth /. 2.;
  let y1 = rem; /* 1 */
  (x0, x1, y1, la, ra, isLight);
};

let draw = (state, env) => {
  let (x0, x1, y1, la, ra, isLight) = findInitialCoordinates(state);

  drawRoad((x0, height), (x1, height), y1, true, lAngle, rAngle, isLight, env);
  (la, ra);
};
