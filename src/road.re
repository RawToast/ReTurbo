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

type state = {
  position: float,
  lastPiece: int,
  track: Track.state,
};

let currentPlane = state => Track.head(state.track);

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

let _piFactor = 4. *. atan(1.) /. 180.;
let calcDeltaX = (yDistance, angle) => {
  let toRadians = d => d *. _piFactor;
  yDistance *. (angle |> toRadians |> tan);
};

let rec drawRoad =
        (
          leftBottom,
          rightBottom,
          firstHeight,
          track,
          goals,
          isDark,
          state,
          objects,
          env,
        ) => {
  let (x0, y0) = leftBottom;
  let (x1, _) = rightBottom;
  let trackPiece = List.hd(track);
  let isCheckpoint = Track.isCheckpoint(trackPiece);
  isDark ? fillDarkGrey(env) : fillLightGrey(env);
  isCheckpoint ? fillRed(env) : ();

  let nextHeight =
    RoadCalc.calcNextYPosition(y0, height, baseLength, firstHeight);

  let curveStength = RoadCalc.curveStength(trackPiece.direction);

  let (nextGoalL, nextGoalR) =
    RoadCalc.nextGoals(goals, curveStength, height, nextHeight);

  let roadQuad =
    RoadCalc.calcRoadQuad(
      leftBottom,
      rightBottom,
      nextHeight,
      maxHeight,
      nextGoalL,
      nextGoalR,
    );

  Draw.quadf(
    ~p1=roadQuad.leftBottom,
    ~p2=roadQuad.rightBottom,
    ~p3=roadQuad.rightTop,
    ~p4=roadQuad.leftTop,
    env,
  );

  let objects =
    List.append(Objects.calculatePositions(trackPiece, roadQuad), objects);

  let isOutOfBounds =
    maxHeight >= nextHeight
    || x1 < 0.
    +. Common.minOffset
    || x0 > width
    +. Common.maxOffset;
 
  isOutOfBounds ?
    objects 
    : drawRoad(
      roadQuad.leftTop,
      roadQuad.rightTop,
      firstHeight,
      List.tl(track),
      (nextGoalL, nextGoalR),
      !isDark,
      state,
      objects,
      env,
    );
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

let init = {position: 0., track: Track.init, lastPiece: 1};

let draw = (offset, state, env) => {
  let (x0, x1, remainder, isLight) = findInitialCoordinates(offset, state);
  let iOffset = int_of_float(offset *. 0.4); /* interesting */
  let goal = (269 + iOffset, 299 + iOffset);

  drawRoad(
    (x0, height),
    (x1, height),
    remainder,
    state.track.track,
    goal,
    isLight,
    state,
    [],
    env,
  );
};
