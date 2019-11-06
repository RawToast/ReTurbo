type direction =
  | Straight
  | Left(float)
  | Right(float)
  | Checkpoint(int);

module Obsticle = {
  type objectType =
    | SIGN_RIGHT
    | SIGN_LEFT;

  type state = {
    objectType,
    offset: (float, float),
  };
};

type plane = {
  direction,
  obsticles: list(Obsticle.state),
};
type state = {track: list(plane)};
let (|+|) = (a, b) => List.append(a, b);

let demoTrack = {
  let ec1 = 0.08;
  let ec2 = 0.16;
  let mc1 = 0.24;
  let mc2 = 0.32;
  let hc1 = 0.4;
  let hc2 = 0.6;

  let make = (direction: direction, times: int) =>
    Array.make(times, {direction, obsticles: []}) |> Array.to_list;

  let makeCheckpoint = (duration: int) => [{direction: Checkpoint(duration), obsticles: []}];

  let make2 = make(_, 2);
  let make4 = make(_, 4);
  let make8 = make(_, 8);
  let make12 = make(_, 12);
  let make24 = make(_, 24);

  make2(Straight)
  |+| [
    {direction: Straight, obsticles: [{objectType: Obsticle.SIGN_LEFT, offset: (1.1, 0.)}]},
  ]
  |+| make24(Left(ec1))
  |+| make4(Straight)
  |+| make12(Right(ec2))
  |+| make8(Straight)
  |+| make8(Right(ec1))
  |+| make12(Right(ec1))
  |+| make8(Right(ec2))
  |+| make8(Right(mc1))
  |+| make8(Right(mc2))
  |+| make2(Right(hc1))
  |+| make12(Straight)
  |+| make4(Left(mc2))
  |+| make12(Straight)
  |+| [
    {
      direction: Straight,
      obsticles: [{objectType: Obsticle.SIGN_RIGHT, offset: ((-1.1), 0.)}],
    },
  ]
  |+| make4(Left(hc1))
  |+| make4(Left(hc2))
  |+| make8(Straight)
  |+| make4(Right(1.))
  |+| make4(Straight)
  |+| make8(Left(hc2))
  |+| make4(Straight)
  |+| makeCheckpoint(10)
  |+| make4(Straight)
  |+| make24(Right(mc2))
  |+| make8(Left(hc2))
  |+| make8(Straight)
  |+| make12(Left(hc2))
  |+| make4(Straight)
  |+| make24(Right(hc1))
  |+| make4(Straight)
  |+| makeCheckpoint(10);
};
let init = {track: demoTrack};

let isCheckpoint = t =>
  switch (t.direction) {
  | Checkpoint(_) => true
  | _ => false
  };

let progress = state =>
  if (List.length(state.track) > 15) {
    {track: List.tl(state.track)};
  } else {
    {track: List.tl(state.track |+| demoTrack)};
  };

let head = state => List.hd(state.track);