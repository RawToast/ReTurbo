type direction =
  | Straight
  | Left(float)
  | Right(float)
  | Checkpoint(int);

module Obsticle = {
  type objectType =
    | SIGN_RIGHT
    | SIGN_LEFT;

  type location = 
     | LEFT
     | RIGHT
     | CENTRE;

  type state = {
    objectType: objectType,
    location: location,
    offset: (float, float),
    size: (int, int)
  };

  let makeSignRight = {objectType: SIGN_RIGHT, location: LEFT, offset: (-80., 0.), size: (96, 96) };
  let makeSignLeft = {objectType: SIGN_LEFT, location: RIGHT, offset: (30., 0.), size: (96, 96)};
};

type plane = {
  direction,
  obsticles: list(Obsticle.state),
  incline: float
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

  let makeCheckpoint = (duration: int) => [{direction: Checkpoint(duration), obsticles: [], incline: 0.}];
  let make = (~times=1, ~obsticles=[], ~incline=0., road) =>
     Array.make(times, {direction: road, obsticles, incline}) |> Array.to_list;

  make(~times=8, Straight)
  |+| make(~times=2, Straight)
  |+| make(~times=4, ~obsticles=[Obsticle.makeSignRight], Right(ec2))
  |+| make(~times=8, Straight)
  |+| make(~times=24, Left(ec1))
  |+| make(~times=8, ~obsticles=[Obsticle.makeSignLeft], Left(mc1))
  |+| make(~times=4, ~obsticles=[ Obsticle.makeSignRight], Straight)
  |+| make(~times=12, ~obsticles=[Obsticle.makeSignRight], Right(ec2))
  |+| make(~times=8, Straight)
  |+| make(~times=8, Right(ec1))
  |+| make(~times=12, Right(ec1))
  |+| make(~times=8, Right(ec2))
  |+| make(~times=8, Right(mc1))
  |+| make(~times=8, Right(mc2))
  |+| make(~times=2, Right(hc1))
  |+| make(~times=12, Straight)
  |+| make(~times=4, Left(mc2))
  |+| make(~times=4, ~obsticles=[Obsticle.makeSignRight], Right(mc2))
  |+| make(~times=12, Straight)
  |+| make(~times=4, ~obsticles=[Obsticle.makeSignLeft], Left(hc1))
  |+| make(~times=4, ~obsticles=[Obsticle.makeSignLeft], Left(hc2))
  |+| make(~times=8, Straight)
  |+| make(~times=4, Right(1.))
  |+| make(~times=4, Straight)
  |+| make(~times=8, Left(hc2))
  |+| make(~times=4, Straight)
  |+| makeCheckpoint(12)
  |+| make(~times=4, Straight)
  |+| make(~times=24, ~obsticles=[Obsticle.makeSignRight], Right(mc2))
  |+| make(~times=8, Left(hc2))
  |+| make(~times=8, Straight)
  |+| make(~times=12, ~obsticles=[Obsticle.makeSignLeft], Left(hc2))
  |+| make(~times=4, Straight)
  |+| make(~times=4, Left(1.))
  |+| make(~times=8, Straight)
  |+| make(~times=24, Right(hc1))
  |+| make(~times=4, Straight)
  |+| makeCheckpoint(5);
};
let init = {track: demoTrack};

let isCheckpoint = t =>
  switch (t.direction) {
  | Checkpoint(_) => true
  | _ => false
  };

let progress = state => 
  (List.length(state.track) > 25) ?
    {track: List.tl(state.track)} :
    {track: List.tl(state.track |+| demoTrack)};

let head = state => List.hd(state.track);
