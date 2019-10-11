type direction =
  | Straight
  | Left
  | LeftMedium
  | LeftHard
  | LeftK
  | Right
  | RightMedium
  | RightHard
  | RightK;

let _straights = [Straight, Straight, Straight, Straight];
let _lefts = [Left, Left, Left, Left];
let _lefts2 = [LeftMedium, LeftMedium, LeftMedium, LeftMedium];
let _lefts3 = [LeftHard, LeftHard, LeftHard, LeftHard];
let _lefts4 = [LeftK, LeftK, LeftK, LeftK];

let _rights = [Right, Right, Right, Right];
let _rights2 = [RightMedium, RightMedium, RightMedium, RightMedium];
let _rights3 = [RightHard, RightHard, RightHard, RightHard];
let _rights4 = [RightK, RightK, RightK, RightK];

let (|+|) = (a, b) => List.append(a, b);

let demoTrack =
  _straights
  |+| _straights
  |+| _straights
  |+| _rights
  |+| _rights
  |+| _rights
  |+| _rights2
  |+| _rights3
  |+| _rights4
  |+| _straights
  |+| _straights
  |+| _straights
  |+| _straights
  |+| _rights
  |+| _rights2
  |+| _lefts3
  |+| _lefts4
  |+| _rights3
  |+| _rights
  |+| _straights
  |+| _straights
  |+| _lefts
  |+| _lefts2
  |+| _lefts3
  |+| _lefts2
  |+| _lefts4
  |+| _lefts4
  |+| _lefts3
  |+| _straights
  |+| _straights
  |+| _straights
  |+| _straights
  |+| _straights
  |+| _straights
  |+| _straights;

type state = {track: list(direction)};

let init = {track: demoTrack};

let progress = state =>
  if (List.length(state.track) > 15) {
    {track: List.tl(state.track)};
  } else {
    {track: List.tl(state.track |+| demoTrack)};
  };
