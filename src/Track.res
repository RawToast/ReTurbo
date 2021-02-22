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
    offset: float,
    z: float,
    size: (int, int),
  }

  let makeSignRight = {objectType: SIGN_RIGHT, offset: 1.25, z: 0., size: (96, 96)}
  let makeSignLeft = {objectType: SIGN_LEFT, offset: 1.25, z: 0., size: (96, 96)}
  let makeTree = (x) => {
    objectType: TREE,
    offset: x,
    z: 0.,
    size: (128, 208),
  }
  let smallTree = (x) => {
    objectType: TREE,
    offset: x,
    z: 0.,
    size: (64, 104),
  }
  let makeStone = (x) => {
    objectType: STONE,
    offset: x,
    z: 0.,
    size: (64, 64),
  }

  let bunchOfSmallTrees = (offset1, offset2, offset3) =>
    list{smallTree(offset1), smallTree(offset2), smallTree(offset3)}
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

  make(~times=5, Straight)
  ->List.append(make(~times=1, ~incline=-1., Straight))
  ->List.append(make(~times=1, ~incline=-2., Straight))
  ->List.append(make(~times=1, ~incline=-3., Straight))
  ->List.append(make(~times=1, ~incline=-4., Straight))
  ->List.append(make(~times=1, ~incline=-5., Straight))
  ->List.append(make(~times=8, ~incline=-6., Straight))
  ->List.append(make(~times=8, ~incline=-5., Straight))
  ->List.append(make(~times=4, ~incline=-4., ~obsticles=bunchOfSmallTrees(1.3, 1.5, 1.7), Straight))
  ->List.append(make(~times=2, ~incline=-3., ~obsticles=bunchOfSmallTrees(1.3, 1.5, 1.7), Straight))
  ->List.append(make(~times=2, ~incline=-2., ~obsticles=bunchOfSmallTrees(1.3, 1.5, 1.7), Straight))
  ->List.append(make(~times=2, ~incline=-1., ~obsticles=bunchOfSmallTrees(1.3, 1.5, 1.7), Straight))
  ->List.append(
    make(
      ~times=2,
      ~obsticles=list{
        makeSignRight,
        smallTree(1.35),
        makeTree(1.95),
      },
      Right(ec2),
    ),
  )->List.append(
    make(
      ~times=2,
      ~obsticles=bunchOfSmallTrees(-1.3, 1.5, -1.7),
      Right(ec2),
    ),
  )
  ->List.append(
    make(
      ~times=2,
      ~obsticles=list{
        makeSignRight,
        smallTree(1.17),
        makeTree(1.55),
      },
      Right(ec2),
    ))
  ->List.append(make(~times=1, ~incline=1., Straight))
  ->List.append(make(~times=2, ~incline=2., Straight))
  ->List.append(make(~times=9, ~incline=3., Straight))
  ->List.append(make(~times=6, ~incline=2., Straight))
  ->List.append(make(~times=3, ~incline=0.5, Straight))
  ->List.append(make(~times=16, Left(ec1)))
  ->List.append(makeCheckpoint(8))
  ->List.append(make(~times=16, Left(ec1)))
  ->List.append(make(~times=6, ~incline=0.8, ~obsticles=list{makeSignLeft}, Left(ec2)))
  ->List.append(make(~times=8, ~incline=1.5, ~obsticles=list{makeSignLeft}, Left(mc1)))
  ->List.append(make(~times=8, ~incline=3.7, ~obsticles=list{makeSignLeft}, Left(mc2)))
  ->List.append(make(~times=8, ~incline=4.2, ~obsticles=list{makeSignLeft}, Right(mc1)))
  ->List.append(make(~times=12, ~incline=6.1, ~obsticles=list{makeSignLeft}, Right(mc2)))
  ->List.append(make(~times=4, ~incline=4.2, ~obsticles=list{makeSignLeft}, Right(mc1)))
  ->List.append(make(~times=6, ~incline=3.2, ~obsticles=list{makeSignLeft}, Right(mc1)))
  ->List.append(make(~times=4, ~incline=1.2, ~obsticles=list{makeSignLeft}, Right(mc1)))
  ->List.append(make(~times=4, ~obsticles=list{makeSignLeft}, Right(mc1)))
  ->List.append(make(~times=6, ~obsticles=list{makeSignRight}, Straight))
  ->List.append(make(~times=18, ~obsticles=list{makeSignRight}, Right(ec2)))
  ->List.append(make(~times=2, ~incline=-0.2, Straight))
  ->List.append(make(~times=10, ~incline=-0.8, Straight))
  ->List.append(make(~times=18, ~incline=-1.2, Right(ec1)))
  ->List.append(make(~times=10, ~incline=-0.7, Right(ec2)))
  ->List.append(make(~times=2, ~incline=-0.3, Right(ec2)))
  ->List.append(make(~times=12, Right(mc1)))
  ->List.append(make(~times=12, Right(mc2)))
  ->List.append(
    make(~times=2, ~obsticles=list{makeStone(1.55)}, Right(hc1)),
  )
  ->List.append(make(~times=6, ~incline=-0.5, Straight))
  ->List.append(make(~times=6, ~incline=-1.5, Straight))
  ->List.append(make(~times=6, ~incline=-0.5, Straight))
  ->List.append(make(~times=6, Left(mc2)))
  ->List.append(make(~times=6, ~obsticles=list{makeSignRight}, Right(mc2)))
  ->List.append(make(~times=6, Straight))
  ->List.append(make(~times=6, Straight))
  ->List.append(make(~times=6, ~incline=0.6, Straight))
  ->List.append(make(~times=6, ~incline=1.8, ~obsticles=list{makeSignLeft}, Left(hc1)))
  ->List.append(make(~times=3, ~incline=0.3, ~obsticles=list{makeSignLeft}, Left(hc1)))
  ->List.append(make(~times=6, ~incline=-0.2, ~obsticles=list{makeSignLeft}, Left(hc2)))
  ->List.append(make(~times=6, Straight))
  ->List.append(make(~times=1, ~obsticles=bunchOfSmallTrees(1.3, 1.5, 1.7), Straight))
  ->List.append(make(~times=1, ~obsticles=bunchOfSmallTrees(-1.25, -1.5, -1.75), Straight))
  ->List.append(make(~times=1, ~obsticles=bunchOfSmallTrees(1.2, 1.4, 1.6), Straight))
  ->List.append(make(~times=1, ~obsticles=bunchOfSmallTrees(-1.2, -1.4, -1.6), Straight))
  ->List.append(make(~times=1, ~obsticles=bunchOfSmallTrees(1.3, 1.5, 1.7), Straight))
  ->List.append(make(~times=1, ~incline=0.4, ~obsticles=bunchOfSmallTrees(-1.2, -1.5, -1.7), Straight))
  ->List.append(make(~times=1, ~incline=0.9, Right(1.)))
  ->List.append(make(~times=1, ~incline=1.3, Right(1.)))
  ->List.append(make(~times=4, ~incline=2.8, Right(1.)))
  ->List.append(make(~times=1, ~incline=1.7, Right(1.)))
  ->List.append(make(~times=1, ~incline=0.5, Right(1.)))
  ->List.append(make(~times=4, ~incline=0.2, Straight))
  ->List.append(make(~times=1, ~obsticles=list{makeStone(1.4), makeStone(1.75)}, Straight))
  ->List.append(make(~times=1, ~obsticles=list{makeStone(1.65)}, Straight))
  ->List.append(make(~times=12, Left(hc2)))
  ->List.append(make(~times=6, Straight))
  ->List.append(makeCheckpoint(12))
  ->List.append(
    make(
      ~times=6,
      ~obsticles=list{
        Obsticle.smallTree(-1.35),
        Obsticle.makeTree(-1.9),
        Obsticle.smallTree(1.35),
        Obsticle.makeTree(1.9),
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
  ->List.append(make(~times=2, ~incline=-1., Straight))
  ->List.append(make(~times=2, ~incline=-2., Straight))
  ->List.append(make(~times=36, ~incline=-3., Right(hc1)))
  ->List.append(make(~times=2, ~incline=-2., Straight))
  ->List.append(make(~times=2, ~incline=-1., Straight))
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
  List.length(state.track) > 106
    ? {track: List.tl(state.track)}
    : {
        lastTrack := lastTrack.contents->List.rev
        {track: List.tl(List.append(state.track, lastTrack.contents))}
      }

let head = state => List.hd(state.track)
