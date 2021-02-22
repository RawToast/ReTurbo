type direction =
  | Straight
  | Left(float)
  | Right(float)
  | Checkpoint(int)

module Obsticle = {
  type objectType =
    | SIGN_RIGHT
    | SIGN_LEFT
    | TREE
    | STONE

  type location =
    | LEFT
    | RIGHT
    | CENTRE

  type state = {
    objectType: objectType,
    location: location,
    offset: (float, float),
    size: (int, int),
  }

  let makeSignRight = {objectType: SIGN_RIGHT, location: LEFT, offset: (-80., 0.), size: (96, 96)}
  let makeSignLeft = {objectType: SIGN_LEFT, location: RIGHT, offset: (30., 0.), size: (96, 96)}
  let makeTree = (~where, x) => {
    objectType: TREE,
    location: where,
    offset: (x, 0.),
    size: (128, 208),
  }
  let smallTree = (~where, x) => {
    objectType: TREE,
    location: where,
    offset: (x, 0.),
    size: (64, 104),
  }
  let makeStone = (~where=CENTRE, x) => {
    objectType: STONE,
    location: where,
    offset: (x, 0.),
    size: (64, 64),
  }

  let bunchOfSmallTrees = (~offset=0., where) => switch where { 
    | RIGHT => list{smallTree(~where, 40. +. offset), smallTree(~where, 100.+. offset), smallTree(~where, 160.+. offset)}
    | LEFT => list{smallTree(~where, -40. +. offset), smallTree(~where, -100.+. offset), smallTree(~where, -160.+. offset)}
    | CENTRE => list{smallTree(~where, -60. +. offset), smallTree(~where, 0.+. offset), smallTree(~where, 60.+. offset)}
  }
}

type plane = {
  direction: direction,
  obsticles: list<Obsticle.state>,
  incline: float,
}

type state = {track: list<plane>}

let demoTrack = {
  open Obsticle;
  let ec1 = 0.08
  let ec2 = 0.16
  let mc1 = 0.24
  let mc2 = 0.32
  let hc1 = 0.4
  let hc2 = 0.6

  let makeCheckpoint = (duration: int) => list{
    {direction: Checkpoint(duration), obsticles: list{}, incline: 0.},
  }
  let make = (~times=1, ~obsticles=list{}, ~incline=0., road) =>
    Array.make(times, {direction: road, obsticles: obsticles, incline: incline}) |> Array.to_list

  make(~times=24, Straight)
  ->List.append(make(~times=2, ~obsticles=bunchOfSmallTrees(RIGHT), Straight))
  ->List.append(make(~times=2, ~obsticles=bunchOfSmallTrees(LEFT), Straight))
  ->List.append(
    make(
      ~times=2,
      ~obsticles=list{
        makeSignRight,
        smallTree(~where=RIGHT, 35.),
        makeTree(~where=RIGHT, 90.),
      },
      Right(ec2),
    ),
  )->List.append(
    make(
      ~times=2,
      ~obsticles=bunchOfSmallTrees(RIGHT),
      Right(ec2),
    ),
  )
  ->List.append(
    make(
      ~times=2,
      ~obsticles=list{
        makeSignRight,
        smallTree(~where=RIGHT, 107.),
        makeTree(~where=RIGHT, 55.),
      },
      Right(ec2),
    ))
  ->List.append(make(~times=3, ~incline=2., Straight))
  ->List.append(make(~times=12, ~incline=5., Straight))
  ->List.append(make(~times=10, ~incline=3., Straight))
  ->List.append(make(~times=3, ~incline=0.5, Straight))
  ->List.append(make(~times=32, Left(ec1)))
  ->List.append(make(~times=12, ~obsticles=list{makeSignLeft}, Left(mc1)))
  ->List.append(make(~times=6, ~obsticles=list{makeSignRight}, Straight))
  ->List.append(make(~times=18, ~obsticles=list{makeSignRight}, Right(ec2)))
  ->List.append(make(~times=12, ~incline=-4., Straight))
  ->List.append(make(~times=18, Right(ec1)))
  ->List.append(make(~times=12, Right(ec2)))
  ->List.append(make(~times=12, Right(mc1)))
  ->List.append(make(~times=12, Right(mc2)))
  ->List.append(
    make(~times=2, ~obsticles=list{makeStone(~where=RIGHT, 55.)}, Right(hc1)),
  )
  ->List.append(make(~times=18, Straight))
  ->List.append(make(~times=6, Left(mc2)))
  ->List.append(make(~times=6, ~obsticles=list{makeSignRight}, Right(mc2)))
  ->List.append(make(~times=18, Straight))
  ->List.append(make(~times=6, ~obsticles=list{makeSignLeft}, Left(hc1)))
  ->List.append(make(~times=6, ~obsticles=list{makeSignLeft}, Left(hc2)))
  ->List.append(make(~times=6, Straight))
  ->List.append(make(~times=1, ~obsticles=bunchOfSmallTrees(~offset=90., RIGHT), Straight))
  ->List.append(make(~times=1, ~obsticles=bunchOfSmallTrees(~offset=-90., LEFT), Straight))
  ->List.append(make(~times=1, ~obsticles=bunchOfSmallTrees(~offset=60., RIGHT), Straight))
  ->List.append(make(~times=1, ~obsticles=bunchOfSmallTrees(~offset=-60., LEFT), Straight))
  ->List.append(make(~times=1, ~obsticles=bunchOfSmallTrees(~offset=30., RIGHT), Straight))
  ->List.append(make(~times=1, ~obsticles=bunchOfSmallTrees(~offset=-30., LEFT), Straight))
  ->List.append(make(~times=6, Right(1.)))
  ->List.append(make(~times=4, Straight))
  ->List.append(make(~times=1, ~obsticles=list{makeStone(~where=RIGHT, 40.), makeStone(~where=RIGHT, 170.)}, Straight))
  ->List.append(make(~times=1, ~obsticles=list{makeStone(~where=RIGHT, 85.)}, Straight))
  ->List.append(make(~times=12, Left(hc2)))
  ->List.append(make(~times=6, Straight))
  ->List.append(makeCheckpoint(12))
  ->List.append(
    make(
      ~times=6,
      ~obsticles=list{
        Obsticle.smallTree(~where=LEFT, -35.),
        Obsticle.makeTree(~where=LEFT, -90.),
        Obsticle.smallTree(~where=RIGHT, 35.),
        Obsticle.makeTree(~where=RIGHT, 90.),
      },
      Straight,
    ),
  )
  ->List.append(make(~times=36, ~obsticles=list{makeSignRight}, Right(mc2)))
  ->List.append(make(~times=12, Left(hc2)))
  ->List.append(make(~times=12, Straight))
  ->List.append(make(~times=18, ~obsticles=list{makeSignLeft}, Left(hc2)))
  ->List.append(make(~times=6, ~obsticles=list{makeSignLeft}, Straight))
  ->List.append(make(~times=6, Left(1.)))
  ->List.append(make(~times=12, Straight))
  ->List.append(make(~times=36, Right(hc1)))
  ->List.append(make(~times=6, Straight))
  ->List.append(makeCheckpoint(5))
}
let init = {track: demoTrack}

let isCheckpoint = t =>
  switch t.direction {
  | Checkpoint(_) => true
  | _ => false
  }

let lastTrack = ref(demoTrack)
let progress = state =>
  List.length(state.track) > 25
    ? {track: List.tl(state.track)}
    : {
        lastTrack := lastTrack.contents->List.rev
        {track: List.tl(List.append(state.track, lastTrack.contents))}
      }

let head = state => List.hd(state.track)
