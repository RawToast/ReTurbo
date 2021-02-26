type direction =
  | Straight
  | Left(float)
  | Right(float)
  | Checkpoint(int)

let bunchOfSmallTrees = (offset1, offset2, offset3) => list{
  Object.Prefabs.smallTree(offset1),
  Object.Prefabs.smallTree(offset2),
  Object.Prefabs.smallTree(offset3),
}

type plane = {
  direction: direction,
  objects: list<Object.state>,
  incline: float,
}

type state = {track: list<plane>}

let demoTrack = {
  open Object.Prefabs

  let ec1 = 0.08
  let ec2 = 0.16
  let mc1 = 0.24
  let mc2 = 0.32
  let hc1 = 0.4
  let hc2 = 0.6

  let makeCheckpoint = (~incline=0., duration: int) => list{
    {direction: Checkpoint(duration), objects: list{}, incline},
  }
  let make = (~times=1, ~objects=list{}, ~incline=0., road) =>
    Array.make(times, {direction: road, objects: objects, incline: incline}) |> Array.to_list

  make(~times=3, Straight)
  ->List.append(make(~times=2, ~incline=0.5, Straight))
  ->List.append(make(~times=1, ~incline=-1., ~objects=list{makeStone(0.)}, Straight))
  ->List.append(make(~times=1, ~incline=-2., ~objects=list{makeStone(0.5)}, Straight))
  ->List.append(
    make(~times=1, ~objects=list{smallTree(1.17), makeTree(1.55)}, ~incline=-3., Straight),
  )
  ->List.append(make(~times=1, ~incline=-4., ~objects=list{}, Straight))
  ->List.append(make(~times=1, ~incline=-5., ~objects=list{}, Straight))
  ->List.append(make(~times=8, ~incline=-6., Straight))
  ->List.append(make(~times=8, ~incline=-5., Straight))
  ->List.append(make(~times=4, ~incline=-4., ~objects=bunchOfSmallTrees(1.3, 1.5, 1.7), Straight))
  ->List.append(make(~times=2, ~incline=-3., Straight))
  ->List.append(
    make(~times=2, ~incline=-2., ~objects=bunchOfSmallTrees(1.35, 1.5, 1.65), Straight),
  )
  ->List.append(make(~times=2, ~incline=-1., ~objects=list{makeTree(1.6)}, Straight))
  ->List.append(make(~times=2, ~objects=list{smallTree(1.35), makeTree(1.95)}, Right(ec2)))
  ->List.append(make(~times=2, ~objects=bunchOfSmallTrees(-1.3, 1.5, -1.7), Right(ec2)))
  ->List.append(make(~times=2, ~objects=list{smallTree(1.17), makeTree(1.55)}, Right(ec2)))
  ->List.append(make(~times=1, ~incline=1., Straight))
  ->List.append(make(~times=2, ~incline=2., Straight))
  ->List.append(make(~times=9, ~incline=3., Straight))
  ->List.append(make(~times=6, ~incline=2., Straight))
  ->List.append(make(~times=3, ~incline=0.5, Straight))
  ->List.append(make(~times=16, Left(ec1)))
  ->List.append(makeCheckpoint(9))
  ->List.append(make(~times=16, Left(ec1)))
  ->List.append(make(~times=6, ~incline=0.8, Left(ec2)))
  ->List.append(make(~times=8, ~incline=1.5, ~objects=list{makePost(0.), makeSignLeft}, Left(mc1)))
  ->List.append(make(~times=8, ~incline=3.7, ~objects=list{makePost(0.), makeSignLeft}, Left(mc2)))
  ->List.append(make(~times=8, ~incline=4.2, ~objects=list{makePost(0.), makeSignRight}, Right(mc1)))
  ->List.append(make(~times=12, ~incline=6.1, ~objects=list{makeSignRight}, Right(mc2)))
  ->List.append(
    make(~times=4, ~incline=4.2, ~objects=list{makeSignRight, makeTree(-1.65)}, Right(mc1)),
  )
  ->List.append(make(~times=6, ~incline=3.2, ~objects=list{makeSignRight}, Right(mc1)))
  ->List.append(make(~times=4, ~incline=1.2, Right(mc1)))
  ->List.append(make(~times=4, ~objects=list{makeSignRight}, Right(mc1)))
  ->List.append(make(~times=6, Straight))
  ->List.append(make(~times=18, ~objects=list{makeSignRight}, Right(ec2)))
  ->List.append(make(~times=2, ~incline=-0.2, Straight))
  ->List.append(make(~times=10, ~incline=-0.8, Straight))
  ->List.append(make(~times=18, ~incline=-1.2, Right(ec1)))
  ->List.append(make(~times=10, ~incline=-0.7, Right(ec2)))
  ->List.append(make(~times=2, ~incline=-0.3, Right(ec2)))
  ->List.append(make(~times=12, Right(mc1)))
  ->List.append(make(~times=12, ~objects=list{makeSignRight}, Right(mc2)))
  ->List.append(make(~times=2, ~objects=list{makeStone(1.55)}, Right(hc1)))
  ->List.append(make(~times=4, ~incline=-0.5, Straight))
  ->List.append(
    make(~times=2, ~objects=list{makeTree(-1.25), makeTree(1.25)}, ~incline=-1.5, Straight),
  )
  ->List.append(
    make(~times=2, ~objects=list{makeTree(-1.35), makeTree(1.35)}, ~incline=-1.5, Straight),
  )
  ->List.append(make(~times=2, ~incline=-1.5, Straight))
  ->List.append(
    make(
      ~times=2,
      ~objects=list{makeTree(-1.25), makeTree(-1.45), makeTree(1.25), makeTree(1.45)},
      ~incline=-1.5,
      Straight,
    ),
  )
  ->List.append(make(~times=4, ~incline=-0.5, Straight))
  ->List.append(make(~times=8, Left(mc2)))
  ->List.append(make(~times=6, ~objects=list{makeSignRight}, Right(mc2)))
  ->List.append(make(~times=6, Straight))
  ->List.append(make(~times=6, Straight))
  ->List.append(make(~times=6, ~incline=0.6, Straight))
  ->List.append(make(~times=6, ~incline=1.8, ~objects=list{makeSignLeft}, Left(hc1)))
  ->List.append(make(~times=3, ~incline=0.3, ~objects=list{makeSignLeft}, Left(hc1)))
  ->List.append(make(~times=6, ~incline=-0.2, ~objects=list{makeSignLeft}, Left(hc2)))
  ->List.append(make(~times=6, Straight))
  ->List.append(make(~times=1, ~objects=bunchOfSmallTrees(1.3, 1.5, 1.7), Straight))
  ->List.append(make(~times=1, ~objects=bunchOfSmallTrees(-1.25, -1.5, -1.75), Straight))
  ->List.append(make(~times=1, ~objects=bunchOfSmallTrees(1.2, 1.4, 1.6), Straight))
  ->List.append(make(~times=1, ~objects=bunchOfSmallTrees(-1.2, -1.4, -1.6), Straight))
  ->List.append(make(~times=1, ~objects=bunchOfSmallTrees(1.3, 1.5, 1.7), Straight))
  ->List.append(
    make(~times=1, ~incline=0.4, ~objects=bunchOfSmallTrees(-1.2, -1.5, -1.7), Straight),
  )
  ->List.append(make(~times=1, ~incline=0.9, Right(1.)))
  ->List.append(make(~times=1, ~incline=1.3, Right(1.)))
  ->List.append(make(~times=4, ~incline=2.8, Right(1.)))
  ->List.append(make(~times=1, ~incline=1.7, Right(1.)))
  ->List.append(make(~times=1, ~incline=0.5, Right(1.)))
  ->List.append(make(~times=4, ~incline=0.2, Straight))
  ->List.append(make(~times=1, ~objects=list{makeStone(1.4), makeStone(1.75)}, Straight))
  ->List.append(make(~times=1, ~objects=list{makeStone(1.65)}, Straight))
  ->List.append(make(~times=12, Left(hc2)))
  ->List.append(make(~times=6, Straight))
  ->List.append(makeCheckpoint(9))
  ->List.append(
    make(
      ~times=6,
      ~objects=list{smallTree(-1.35), makeTree(-1.9), smallTree(1.35), makeTree(1.9)},
      Straight,
    ),
  )
  ->List.append(make(~times=36, ~objects=list{makeSignRight}, Right(mc2)))
  ->List.append(make(~times=12, Left(hc2)))
  ->List.append(make(~times=12, Straight))
  ->List.append(make(~times=18, ~objects=list{makeSignLeft}, Left(hc2)))
  ->List.append(make(~times=6, ~objects=list{makeSignLeft}, Straight))
  ->List.append(make(~times=6, Left(1.)))
  ->List.append(make(~times=12, Straight))
  ->List.append(make(~times=2, ~incline=-1., Straight))
  ->List.append(make(~times=2, ~incline=-2., Straight))
  ->List.append(make(~times=36, ~incline=-2.4, Right(hc1)))
  ->List.append(make(~times=2, ~incline=-2., Straight))
  ->List.append(make(~times=2, ~incline=-1., Straight))
  ->List.append(make(~times=6, Straight))
  ->List.append(makeCheckpoint(9))
  ->List.append(make(~times=3, Straight)
  ->List.append(make(~times=2, ~incline=0.5, Straight))
  ->List.append(make(~times=1, ~incline=-1., Straight))
  ->List.append(make(~times=1, ~incline=-2., Straight))
  ->List.append(
    make(~times=1, ~objects=list{smallTree(1.17), makeTree(1.55)}, ~incline=-3., Straight),
  )
  ->List.append(make(~times=1, ~incline=-4., ~objects=list{makeStone(-1.75), makeStone(-1.5)}, Straight))
  ->List.append(make(~times=1, ~incline=-5., ~objects=list{makeStone(1.75)}, Straight))
  ->List.append(make(~times=8, ~incline=-6., Straight))
  ->List.append(make(~times=6, ~objects=list{smallTree(1.35), makeStone(1.95)}, ~incline=-7., Right(ec2)))
  ->List.append(make(~times=14, ~incline=-7., Left(ec2)))
  ->List.append(make(~times=8, ~incline=-7.5, Straight))
  ->List.append(make(~times=8, ~incline=-8., Right(ec2)))
  ->List.append(make(~times=12, ~objects=list{makeSignRight}, ~incline=-8.5, Right(mc1)))
  ->List.append(make(~times=8, ~incline=-8., Right(mc2)))
  ->List.append(make(~times=6, ~objects=list{makeStone(1.95)}, ~incline=-7., Right(ec2)))
  ->List.append(make(~times=8, ~incline=-5., Straight))
  ->List.append(make(~times=4, ~incline=-4., ~objects=bunchOfSmallTrees(1.3, 1.5, 1.7), Straight))
  ->List.append(make(~times=2, ~incline=-3., Straight))
  ->List.append(
    make(~times=2, ~incline=-2., ~objects=bunchOfSmallTrees(1.35, 1.5, 1.65), Straight),
  )
  ->List.append(make(~times=2, ~incline=-1., ~objects=list{makeTree(1.6)}, Straight))
  ->List.append(make(~times=2, ~objects=list{smallTree(1.35), makeTree(1.95)}, Right(ec2)))
  ->List.append(make(~times=2, ~objects=bunchOfSmallTrees(-1.3, 1.5, -1.7), Right(ec2)))
  ->List.append(make(~times=2, ~objects=list{smallTree(1.17), makeTree(1.55)}, Right(ec2)))
  ->List.append(make(~times=1, ~incline=1., Straight))
  ->List.append(make(~times=2, ~incline=1.5, Straight))
  ->List.append(make(~times=4, ~incline=2.4, Straight))
  ->List.append(make(~times=9, ~incline=3.5, Straight))
  ->List.append(make(~times=6, ~incline=2., Straight))
  ->List.append(make(~times=3, ~incline=0.5, Straight))
  ->List.append(make(~times=16, Left(ec1))))
  ->List.append(make(~times=8, Left(mc1)))
  ->List.append(make(~times=6, ~incline=1., Left(ec1)))
  ->List.append(make(~times=6, ~objects=list{makePost(0.)}, ~incline=0.2, Straight))
  ->List.append(make(~times=8, ~objects=list{makePost(0.)}, ~incline=0.8, Right(mc1)))
  ->List.append(make(~times=8, ~incline=2.2, ~objects=list{makePost(0.), makeSignRight}, Right(hc1)))
  ->List.append(make(~times=2, ~incline=3.2, Straight))
  ->List.append(make(~times=8, ~incline=5.2, Straight))
  ->List.append(make(~times=6, ~incline=6.1, Straight))
  ->List.append(make(~times=4, ~incline=5.8, ~objects=list{makePost(0.)}, Left(ec1)))
  ->List.append(make(~times=8, ~incline=6.5, ~objects=list{makePost(0.)}, Left(mc1)))
  ->List.append(make(~times=8, ~incline=7.5, ~objects=list{makeSignLeft}, Left(hc1)))
  ->List.append(make(~times=2, ~incline=8.8, Straight))
  ->List.append(makeCheckpoint(~incline=9.2, 1))
  ->List.append(make(~times=3, ~incline=9.9, ~objects=list{makePost(0.)}, Left(mc1)))
  ->List.append(make(~times=4, ~incline=12.4, ~objects=list{makePost(0.)}, Left(hc2)))
  ->List.append(make(~times=4, ~incline=9.4, Left(mc2)))
  ->List.append(make(~times=4, ~incline=8.4, ~objects=list{makePost(0.)},Left(ec1)))
  ->List.append(make(~times=4, ~incline=6.2, ~objects=list{makePost(0.)}, Straight))
  ->List.append(make(~times=4, ~incline=4.2, Straight))
  ->List.append(make(~times=2, ~incline=5.4, Straight))
  ->List.append(make(~times=4, ~incline=6.2, Right(ec1)))
  ->List.append(make(~times=4, ~incline=6.6, Right(ec2)))
  ->List.append(make(~times=8, ~incline=7.1, Right(mc1)))
  ->List.append(make(~times=8, ~incline=6.9, Right(mc2)))
  ->List.append(make(~times=8, ~incline=6.4, ~objects=list{makePost(0.)},Right(hc1)))
  ->List.append(make(~times=8, ~incline=6.1, ~objects=list{makePost(0.),makeSignRight}, Right(hc2)))
  ->List.append(make(~times=4, ~incline=5.2, Straight))
  ->List.append(make(~times=2, ~incline=4.7, Straight))
  ->List.append(make(~times=2, ~incline=4.4, Straight))
  ->List.append(make(~times=6, ~incline=4.2, Straight))
  ->List.append(make(~times=2, ~incline=4.5, Straight))
  ->List.append(make(~times=6, ~incline=5.1, Left(mc1)))
  ->List.append(make(~times=4, ~incline=6.2, Straight))
  ->List.append(make(~times=4, ~incline=4.2, Straight))
  ->List.append(make(~times=2, ~incline=2.2, Straight))
  ->List.append(make(~times=2, ~incline=1.2, Straight))
  ->List.append(make(~times=1, ~incline=0.5, Straight))
  ->List.append(make(~times=1, ~incline=0., Straight))
  ->List.append(make(~times=1, ~incline=-0.5, Straight))
  ->List.append(makeCheckpoint(~incline=-1.3, 9))
  ->List.append(make(~times=2, ~incline=-1.2, Straight))
  ->List.append(make(~times=8, ~incline=-2.4, Left(mc1)))
  ->List.append(make(~times=12, ~incline=-3.6, Straight))
  ->List.append(make(~times=8, ~incline=-2.4, Straight))
  ->List.append(make(~times=8, ~incline=-1.2, Straight))
  ->List.append(make(~times=4, ~incline=0., Straight))
  ->List.append(make(~times=4, ~incline=1., Straight))
  ->List.append(make(~times=4, ~incline=4., Straight))
  ->List.append(make(~times=12, ~incline=5., Straight))
  ->List.append(make(~times=4, ~incline=5.4, Straight))
  ->List.append(make(~times=4, ~incline=3.2, Straight))
  ->List.append(make(~times=4, ~incline=1.1, Straight))
  ->List.append(make(~times=8, ~incline=0.2, ~objects=list{makeSignRight}, Right(hc1)))
  ->List.append(make(~times=8, ~incline=-2.2, Right(mc1)))
  ->List.append(make(~times=8, ~incline=-0.2, Straight))
  ->List.append(make(~times=4, ~incline=1.2, Straight))
  ->List.append(make(~times=4, ~incline=0.2, Straight))
  ->List.append(make(~times=4, ~incline=-1.2, Straight))
  ->List.append(make(~times=4, ~incline=-0.2, Straight))
  ->List.append(make(~times=6, ~incline=1.2, Left(ec1)))
  ->List.append(make(~times=14, ~incline=3.2, Left(mc1)))
  ->List.append(make(~times=10, ~incline=5.2, Left(ec2)))
  ->List.append(make(~times=8, ~incline=3.5, Left(mc1)))
  ->List.append(make(~times=4, ~incline=2.1, Left(ec1)))
  ->List.append(make(~times=4, ~incline=1.1, Left(ec1)))
  ->List.append(make(~times=4, ~incline=0.5, Left(ec1)))
  ->List.append(make(~times=4, Straight))
  ->List.append(makeCheckpoint(9))
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
        lastTrack := lastTrack.contents->List.rev|>List.map(t => {...t, incline: t.incline *. 1.1})
        {track: List.tl(List.append(state.track, lastTrack.contents))}
      }

let head = state => List.hd(state.track)
let tail = state => List.tl(state.track)
