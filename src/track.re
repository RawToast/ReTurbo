type direction =
  | Straight
  | Left(float)
  | Right(float);

let ec1 = 0.08;
let ec2 = 0.16;
let mc1 = 0.24;
let mc2 = 0.32;
let hc1 = 0.4;
let hc2 = 0.6;
let (|+|) = (a, b) => List.append(a, b);

let make = (direction: direction, times: int) =>
  Array.make(times, direction) |> Array.to_list;

let make2 = make(_, 2);
let make4 = make(_, 4);
let make8 = make(_, 8);
let make12 = make(_, 12);

let demoTrack =
  make12(Straight)
  |+| make12(Straight)
  |+| make12(Straight)
  |+| make4(Left(ec1))
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
  |+| make4(Left(hc1))
  |+| make4(Left(hc2))
  |+| make8(Straight)
  |+| make4(Right(1.))
  |+| make4(Straight)
  |+| make8(Left(hc2));

type state = {track: list(direction)};

let init = {track: demoTrack};

let progress = state =>
  if (List.length(state.track) > 15) {
    {track: List.tl(state.track)};
  } else {
    {track: List.tl(state.track |+| demoTrack)};
  };

let head = state => List.hd(state.track);
