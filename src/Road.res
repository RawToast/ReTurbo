open Reprocessing

/* Screen constants */
let screenHeightF = Common.heightF
let screenWidthF = Common.widthF
/* Road constants */
let baseWidth = Common.roadWidth
let baseLength = 36.
let maxHeight = 2. *. (screenHeightF /. 3.)

let fillDarkGrey = Draw.fill(Utils.color(~r=60, ~g=60, ~b=60, ~a=255))
let fillLightGrey = Draw.fill(Utils.color(~r=68, ~g=68, ~b=68, ~a=255))
let fillRed = Draw.fill(Utils.color(~r=150, ~g=80, ~b=80, ~a=255))

let fillDarkGreen = Draw.fill(Utils.color(~r=30, ~g=120, ~b=30, ~a=255))
let fillLightGreen = Draw.fill(Utils.color(~r=45, ~g=140, ~b=30, ~a=255))

type state = {
  position: float,
  lastPiece: int,
  track: Track.state,
}

let currentPlane = state => Track.head(state.track)

let moveForward = (newPosition, state) =>
  float_of_int(state.lastPiece) *. baseLength -. newPosition <= 0.
    ? {
        lastPiece: state.lastPiece + 1,
        position: newPosition,
        track: Track.progress(state.track),
      }
    : {...state, position: newPosition}

let onCheckpoint = state => state.track |> Track.head |> Track.isCheckpoint

let checkpointBonus = state =>
  state.track
  |> Track.head
  |> (
    p =>
      switch p.direction {
      | Track.Checkpoint(t) => t
      | _ => 0
      }
  )

let _piFactor = 4. *. atan(1.) /. 180.
let calcDeltaX = (yDistance, angle) => {
  let toRadians = d => d *. _piFactor
  yDistance *. (angle |> toRadians |> tan)
}

let rec drawRoad = (
  ~leftBottom,
  ~rightBottom,
  ~firstHeight,
  ~track,
  ~goals,
  ~isDark,
  ~state,
  ~objects,
  env,
) => {
  let (x0, y0) = leftBottom
  let (x1, _) = rightBottom
  let trackPiece = List.hd(track)
  let isCheckpoint = Track.isCheckpoint(trackPiece)

  let nextHeight = RoadCalc.calcNextYPosition(y0, baseLength, firstHeight)

  let (nextGoalL, nextGoalR) = RoadCalc.nextGoals(~goals, ~nextHeight, trackPiece)

  let roadQuad = RoadCalc.calcRoadQuad(
    leftBottom,
    rightBottom,
    nextHeight,
    maxHeight,
    nextGoalL,
    nextGoalR,
  )

  // Road "tile" drawing
  // First ground, then road
  isDark ? fillLightGreen(env) : fillDarkGreen(env)
  let (_, bottomY) = roadQuad.leftBottom
  let (_, topY) = roadQuad.leftTop
  Draw.quadf(
    ~p1=(0., bottomY),
    ~p2=(screenWidthF, bottomY),
    ~p3=(screenWidthF, topY),
    ~p4=(0., topY),
    env,
  )

  isDark ? fillDarkGrey(env) : fillLightGrey(env)
  isCheckpoint ? fillRed(env) : ()
  Draw.quadf(
    ~p1=roadQuad.leftBottom,
    ~p2=roadQuad.rightBottom,
    ~p3=roadQuad.rightTop,
    ~p4=roadQuad.leftTop,
    env,
  )

  let objects = List.append(Objects.calculatePositions(trackPiece, roadQuad), objects)

  let isOutOfBounds =
    maxHeight >= nextHeight ||
      (x1 < 0. +. Common.minOffset ||
      x0 > screenWidthF +. Common.maxOffset)

  isOutOfBounds
    ? objects
    : drawRoad(
        ~leftBottom=roadQuad.leftTop,
        ~rightBottom=roadQuad.rightTop,
        ~firstHeight,
        ~track=List.tl(track),
        ~goals=(nextGoalL, nextGoalR),
        ~isDark=!isDark,
        ~state,
        ~objects,
        env,
      )
}

let findInitialCoordinates = (offset, state) => {
  let (isLight, remainder) = {
    let adj = mod_float(state.position, baseLength *. 2.)
    adj >= baseLength ? (true, adj -. baseLength) : (false, adj)
  }
  let x0 = screenWidthF /. 2. -. baseWidth /. 2. +. offset
  let x1 = screenWidthF /. 2. +. baseWidth /. 2. +. offset
  (x0, x1, remainder, isLight)
}

let init = {position: 0., track: Track.init, lastPiece: 1}

let draw = (offset, state, env) => {
  let (x0, x1, remainder, isLight) = findInitialCoordinates(offset, state)
  let iOffset = int_of_float(offset *. 0.4) /* interesting */
  // middle = 284
  let goal = (264 + iOffset, 304 + iOffset)

  drawRoad(
    ~leftBottom=(x0, screenHeightF),
    ~rightBottom=(x1, screenHeightF),
    ~firstHeight=remainder,
    ~track=state.track.track,
    ~goals=goal,
    ~isDark=isLight,
    ~state,
    ~objects=list{},
    env,
  )
}

module Display = {
  type colour = {
    r: int,
    g: int,
    b: int,
    a: int,
  }
  type t = {
    x: float,
    y: float,
    z: float,
    previous: (float, float, float),
    colour: colour,
    terrainColour: colour,
  }

  %%private(let darkGrey = {r: 60, g: 60, b: 60, a:255})
  %%private(let lightGrey = {r: 68, g: 68, b: 68, a:255})
  %%private(let _red = {r: 150, g: 80, b: 80, a:255})
  %%private(let darkGreen = {r: 30, g: 120, b: 30, a:255})
  %%private(let lightGreen = {r: 45, g: 140, b: 30, a:255})


  let make = (~offset, state) => {
    let (_, _, remainder, isLight) = findInitialCoordinates(offset, state)
    // let iOffset = int_of_float(offset *. 0.4) /* interesting */
    let remainder = baseLength -. remainder
    let convert = (~remainder, ~isDark=isLight, track) => {
      let isDark = ref(isDark)
      let previous = ref(None)
      let ddx = ref(0.)
      track |> List.mapi((i, plane) => {
        let curve = switch plane.Track.direction {
        | Track.Left(curve) => i == 0 ? (remainder /. baseLength) *. curve *. -2. : curve *. -2.
        | Right(curve) => i == 0 ? (remainder /. baseLength) *. curve *. 2. : curve *. 2.
        | _ => 0.
        }
        let curve = curve +. ddx.contents
        ddx := curve
        let i = float_of_int(i)
        let prev = switch previous.contents {
          | Some(xyz) => xyz
          | None => (0., 0., 0.)
        }
        let (px, py, _pz) = prev

        let x = px +. (curve)
        let y = py
        let z = i *. baseLength +. remainder
        previous := Some((x, y, z))
        let result = {
          x: x,
          y: y,
          z: z,
          previous: prev,
          colour: isDark.contents ? lightGrey : darkGrey,
          terrainColour: isDark.contents ? darkGreen : lightGreen,
        }

        isDark := !isDark.contents
        result
      })
    }

    convert(~remainder, ~isDark=isLight, state.track.track)
  }
}
