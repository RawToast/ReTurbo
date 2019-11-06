open Reprocessing;

/* Screen constants */
let height = float_of_int(Common.height);
let width = float_of_int(Common.width);
/* Road constants */
let baseWidth = Common.roadWidth;
let baseLength = 40.;
let maxHeight = height /. 2.;

let fillDarkGrey = Draw.fill(Utils.color(~r=65, ~g=65, ~b=65, ~a=255));
let fillLightGrey = Draw.fill(Utils.color(~r=80, ~g=80, ~b=80, ~a=255));
let fillRed = Draw.fill(Utils.color(~r=150, ~g=80, ~b=80, ~a=255));

type assets = {
  roadSign: Reprocessing_Common.imageT
};
type state = {
  position: float,
  lastPiece: int,
  track: Track.state,
  assets: assets
};

let currentPlane = state => Track.head(state.track);

let moveForward = (newPosition, state) =>
  if (float_of_int(state.lastPiece) *. baseLength -. newPosition <= 0.) {
    {
      ...state,
      lastPiece: state.lastPiece + 1,
      position: newPosition,
      track: Track.progress(state.track),
    };
  } else {
    {...state, position: newPosition};
  };
let onCheckpoint = state => state.track |> Track.head |> Track.isCheckpoint;
let checkpointBonus = state =>
  state.track
  |> Track.head
  |> (
    p =>
      switch (p.direction) {
      | Track.Checkpoint(t) => t
      | _ => 0
      }
  );

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

let _piFactor = 4. *. atan(1.) /. 180.;
let calcDeltaX = (yDistance, angle) => {
  let toRadians = d => d *. _piFactor;
  yDistance *. (angle |> toRadians |> tan);
};

let rec drawRoad =
        (leftBottom, rightBottom, firstHeight, track, goals, isDark, assets, env) => {
  let (x0, y0) = leftBottom;
  let (x1, _) = rightBottom;
  let trackPiece = List.hd(track);

  let isCheckpoint = Track.isCheckpoint(trackPiece);
  isDark ? fillDarkGrey(env) : fillLightGrey(env);
  isCheckpoint ? fillRed(env) : ();

  let y1 =
    if (y0 == height) {
      height -. baseLength +. firstHeight;
    } else {
      nextY(y0);
    };
  let (gl, gr) = goals;

  let curveStength =
    switch (trackPiece.direction) {
    | Track.Left(lc) => 0. -. lc
    | Track.Right(rc) => rc
    | _ => 0.0
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

  let findPosition = obs => Track.Obsticle.({
    let size = 48;
    let ((xl, by), (xr, _)) = (leftBottom, rightBottom);
    let (offsetX, oy) = obs.offset;

    let h = 48. *. if (by >= height) {1.} else {(by -. y1) /. baseLength};
    let w = 48. *. (((Common.maxOffset +. xr) -. (Common.maxOffset +. xl)) /. baseWidth);

    (int_of_float(((xr )) ), int_of_float(y1 +. h), int_of_float(h), int_of_float(w))
  });

  let drawObs = en => Track.Obsticle.(
    trackPiece.obsticles |> List.iter{ obs =>
      switch obs.objectType {
      | SIGN_LEFT => {
        let (xx, yy, h, w) = findPosition(obs);
        fillRed(env);
        Draw.quad(~p1=(xx, yy), ~p2=(xx+w, yy), ~p3=(xx +w, yy +h), ~p4=(xx, yy + h), env);
        isDark ? fillDarkGrey(env) : fillLightGrey(env);
        Draw.image(assets.roadSign, ~pos=(xx, yy), ~width=w, ~height=h, en)}
      | SIGN_RIGHT => Draw.image(assets.roadSign, ~pos=(130, 230), en)
      };
    }
  );

  drawObs(env);
  


  let isOutOfBounds =
    maxHeight >= y1
    || x1 < 0.
    +. Common.minOffset
    || x0 > width
    +. Common.maxOffset;
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
      assets,
      env,
    );
  };
};

let findInitialCoordinates = (offset, state) => {
  let (isLight, rem) = {
    let adj = mod_float(state.position, baseLength *. 2.);
    adj >= baseLength ? (true, adj -. baseLength) : (false, adj);
  };
  let x0 = width /. 2. -. baseWidth /. 2. +. offset;
  let x1 = width /. 2. +. baseWidth /. 2. +. offset;
  (x0, x1, rem, isLight);
};

let loadAssets(env) = {
    roadSign: Draw.loadImage(~filename="assets/roadSign.png", ~isPixel=true, env)
  };

let init(env) = {position: 0., track: Track.init, lastPiece: 1, assets: loadAssets(env)};

let draw = (offset, state, env) => {
  let (x0, x1, remainder, isLight) = findInitialCoordinates(offset, state);
  let iOffset = int_of_float(offset *. 0.4); /* interesting */
  let goal = (244 + iOffset, 324 + iOffset);

  drawRoad(
    (x0, height),
    (x1, height),
    remainder,
    state.track.track,
    goal,
    isLight,
    state.assets,
    env,
  );
};
