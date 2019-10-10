open Reprocessing;

let height = 320.;
let width = 568.;

let centrePoint = 284.;
let maxHeight = height /. 2.;
let finalWidth = 40.;
let halfFinalWidth = finalWidth /. 2.;
let baseWidth = 400.;
let fillDarkGrey = Draw.fill(Utils.color(~r=65, ~g=65, ~b=65, ~a=255));
let fillLightGrey = Draw.fill(Utils.color(~r=90, ~g=90, ~b=90, ~a=255));
let baseLength = 60.;

type state = {position: float};
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

let calcDeltaX = yDistance => {
  let fortySevenRadians = Util.toRadians(48.);
  yDistance *. tan(fortySevenRadians);
};

let rec drawRoad = (leftBottom, rightBottom, isDark, env) => {
  let (x0, y0) = leftBottom;
  let (x1, _) = rightBottom;

  isDark ? fillDarkGrey(env) : fillLightGrey(env);

  let y1 = nextY(y0);
  let (rightX, leftX) = {
    let deltaX = calcDeltaX(y0 -. y1);
    (x1 -. deltaX, x0 +. deltaX);
  };

  Draw.quadf(
    ~p1=leftBottom,
    ~p2=rightBottom,
    ~p3=(rightX, y1),
    ~p4=(leftX, y1),
    env,
  );

  if (maxHeight >= y0) {
    ();
  } else {
    drawRoad((leftX, y1), (rightX, y1), !isDark, env);
  };
};

let findInitialCoordinates = state => {
  let (isLight, rem) = {
    let adj = mod_float(state.position, baseLength *. 2.);
    adj >= baseLength ? (true, adj -. baseLength) : (false, adj);
  };

  let deltaX = calcDeltaX(rem);
  let x0 = centrePoint -. baseWidth /. 2. -. deltaX;
  let x1 = centrePoint /. 2. +. baseWidth /. 2. +. deltaX;
  let y0 = height +. rem;
  (x0, x1, y0, isLight);
};

let draw = (state, env) => {
  let (x0, x1, y0, isLight) = findInitialCoordinates(state);

  drawRoad((x0, y0), (x1, y0), isLight, env);
};
