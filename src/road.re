let height = 320;
let width = 568;

open Reprocessing;

let float = x => float_of_int(x);
let baseWidth = float(400);
let maxHeight = float(height / 2);
let fillDarkGrey = Draw.fill(Utils.color(~r=65, ~g=65, ~b=65, ~a=255));
let fillLightGrey = Draw.fill(Utils.color(~r=90, ~g=90, ~b=90, ~a=255));
let baseLength = 60.;

type state = {position: float};
let seventyRadians = Util.toRadians(70.);

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

let calcRemX = yDistance => yDistance /. sin(seventyRadians);

let draw = (state, env) => {
  let rec drawRoad = (p1, p2, isDark, env) => {
    let (x0, y0) = p1;
    let (x1, _) = p2;

    isDark ? fillDarkGrey(env) : fillLightGrey(env);

    let y1 = nextY(y0);
    let dx = calcRemX(y0 -. y1);

    let p3x = x1 -. dx;
    let p4x = x0 +. dx;
    Draw.quadf(~p1, ~p2, ~p3=(p3x, y1), ~p4=(p4x, y1), env);

    if (maxHeight >= y0) {
      ();
    } else {
      drawRoad((p4x, y1), (p3x, y1), !isDark, env);
    };
  };

  let (x0, x1, y0, isLight) = {
    let (isLight, rem) = {
      let adj = mod_float(state.position, baseLength *. 2.);
      adj >= baseLength ? (true, adj -. baseLength) : (false, adj);
    };

    let adjX = calcRemX(rem); /*rem *. tan(40.);*/
    let x0 = float(width) /. 2. -. baseWidth /. 2. -. adjX;
    let x1 = float(width) /. 2. +. baseWidth /. 2. +. adjX;
    let y0 = float(height) +. rem;
    (x0, x1, y0, isLight);
  };

  drawRoad((x0, y0), (x1, y0), isLight, env);
};
