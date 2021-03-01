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

let edgePosts = list{Object.Prefabs.makePost(1.), Object.Prefabs.makePost(-1.)}

type roadSurface =
  | Tarmac
  | Dirt

type groundSurface =
  | Grass
  | Soil
  | Water
  | Gravel

type plane = {
  direction: direction,
  objects: list<Object.state>,
  incline: float,
  roadSurface: roadSurface,
  groundSurface: groundSurface,
}

type state = {track: list<plane>}

let demoTrack = {
  open Object.Prefabs

  let ec1 = 0.08
  let ec2 = 0.16
  let ec3 = 0.20
  let ec4 = 0.24
  let mc1 = 0.28
  let mc2 = 0.32
  let mc3 = 0.4
  let mc4 = 0.48
  let hc1 = 0.54
  let hc2 = 0.6
  let hc3 = 0.68
  let hc4 = 0.72
  let hp1 = 0.8

  let makeCheckpoint = (
    ~incline=0.,
    ~roadSurface=Tarmac,
    ~groundSurface=Grass,
    duration: int,
  ) => list{
    {
      direction: Checkpoint(duration),
      objects: list{},
      incline: incline,
      roadSurface: roadSurface,
      groundSurface: groundSurface,
    },
  }
  let make = (
    ~times=1,
    ~objects=list{},
    ~incline=0.,
    ~roadSurface=Tarmac,
    ~groundSurface=Grass,
    direction,
  ) =>
    Array.make(
      times,
      {
        direction: direction,
        objects: objects,
        incline: incline,
        roadSurface: roadSurface,
        groundSurface: groundSurface,
      },
    ) |> Array.to_list

  let track =
    list{
      make(~times=3, Straight),
      make(~times=2, ~incline=0.5, Straight),
      make(~times=1, ~incline=-1., Straight),
      make(~times=1, ~incline=-2., ~objects=list{makeStone(0.5)}, Straight),
      make(~times=1, ~objects=list{smallTree(1.17), makeTree(1.55)}, ~incline=-3., Straight),
      make(~times=1, ~incline=-4., Straight),
      make(~times=1, ~incline=-5., Straight),
      make(~times=8, ~incline=-6., Straight),
      make(~times=8, ~incline=-5., Straight),
      make(~times=4, ~incline=-4., ~objects=bunchOfSmallTrees(1.3, 1.5, 1.7), Straight),
      make(~times=2, ~incline=-3., Straight),
      make(~times=2, ~incline=-2., ~objects=bunchOfSmallTrees(1.35, 1.5, 1.65), Straight),
      make(~times=2, ~incline=-1., ~objects=list{makeTree(1.6)}, Straight),
      make(~times=2, ~objects=list{smallTree(1.35), makeTree(1.95)}, Right(ec2)),
      make(~times=2, ~objects=bunchOfSmallTrees(-1.3, 1.5, -1.7), Right(ec2)),
      make(~times=2, ~objects=list{smallTree(1.17), makeTree(1.55)}, Right(ec2)),
      make(~times=1, ~incline=1., Straight),
      make(~times=2, ~incline=2., Straight),
      make(~times=9, ~incline=3., Straight),
      make(~times=6, ~incline=2., Straight),
      make(~times=3, ~incline=0.5, Straight),
      make(~times=16, Left(ec1)),
      makeCheckpoint(9),
      make(~times=16, Left(ec1)),
      make(~times=6, ~incline=0.8, Left(ec2)),
      make(~times=8, ~incline=1.5, ~objects=list{makePost(0.), makeSignLeft}, Left(ec4)),
      make(~times=8, ~incline=3.7, ~objects=list{makePost(0.), makeSignLeft}, Left(mc2)),
      make(~times=8, ~incline=4.2, ~objects=list{makePost(0.), makeSignRight}, Right(ec4)),
      make(~times=12, ~incline=6.1, ~objects=list{makeSignRight}, Right(mc2)),
      make(~times=4, ~incline=4.2, ~objects=list{makeSignRight, makeTree(-1.65)}, Right(ec4)),
      make(~times=6, ~incline=3.2, ~objects=list{makeSignRight}, Right(ec4)),
      make(~times=4, ~incline=1.2, Right(ec4)),
      make(~times=4, ~objects=list{makeSignRight}, Right(ec4)),
      make(~times=6, Straight),
      make(~times=18, ~objects=list{makeSignRight}, Right(ec2)),
      make(~times=2, ~incline=-0.2, Straight),
      make(~times=10, ~incline=-0.8, Straight),
      make(~times=18, ~incline=-1.2, Right(ec1)),
      make(~times=10, ~incline=-0.7, Right(ec2)),
      make(~times=2, ~incline=-0.3, Right(ec2)),
      make(~times=12, Right(ec4)),
      make(~times=12, ~objects=list{makeSignRight}, Right(mc2)),
      make(~times=2, ~objects=list{makeStone(1.55)}, Right(mc3)),
      make(~times=4, ~incline=-0.5, Straight),
      make(~times=2, ~objects=list{makeTree(-1.25), makeTree(1.25)}, ~incline=-1.5, Straight),
      make(~times=2, ~objects=list{makeTree(-1.35), makeTree(1.35)}, ~incline=-1.5, Straight),
      make(~times=2, ~incline=-1.5, Straight),
      make(
        ~times=2,
        ~objects=list{makeTree(-1.25), makeTree(-1.45), makeTree(1.25), makeTree(1.45)},
        ~incline=-1.5,
        Straight,
      ),
      make(~times=4, ~incline=-0.5, Straight),
      make(~times=8, Left(ec1)),
      make(~times=1, ~objects=list{makeStone(0.9), makeStone(0.6), makeStone(0.3)}, Left(ec1)),
      make(~times=1, ~objects=list{makeStone(0.1)}, Left(ec1)),
      make(~times=8, Left(ec1)),
      make(
        ~times=1,
        ~objects=list{makeStone(-0.85), makeStone(-0.65), makeStone(-0.45)},
        Left(ec1),
      ),
      make(~times=1, ~objects=list{makeStone(-0.1)}, Left(ec1)),
      make(~times=4, Left(ec1)),
      make(~times=6, Straight),
      make(~times=6, Straight),
      make(~times=6, ~incline=0.6, Straight),
      make(~times=6, ~incline=1.8, ~objects=list{makeSignLeft}, Left(mc3)),
      make(~times=3, ~incline=0.3, ~objects=list{makeSignLeft}, Left(mc3)),
      make(~times=6, ~incline=-0.2, ~objects=list{makeSignLeft}, Left(hc2)),
      make(~times=6, Straight),
      make(~times=1, ~objects=bunchOfSmallTrees(1.3, 1.5, 1.7), Straight),
      make(~times=1, ~objects=bunchOfSmallTrees(-1.25, -1.5, -1.75), Straight),
      make(~times=1, ~objects=bunchOfSmallTrees(1.2, 1.4, 1.6), Straight),
      make(~times=1, ~objects=bunchOfSmallTrees(-1.2, -1.4, -1.6), Straight),
      make(~times=1, ~objects=bunchOfSmallTrees(1.3, 1.5, 1.7), Straight),
      make(~times=1, ~incline=0.4, ~objects=bunchOfSmallTrees(-1.2, -1.5, -1.7), Straight),
      make(~times=1, ~incline=0.9, Right(hc3)),
      make(~times=1, ~incline=1.3, Right(hc4)),
      make(~times=4, ~incline=2.8, Right(hp1)),
      make(~times=1, ~incline=1.7, Right(hc4)),
      make(~times=1, ~incline=0.5, Right(hc3)),
      make(~times=4, ~incline=0.2, Straight),
      make(~times=1, ~objects=list{makeStone(1.4), makeStone(1.75)}, Straight),
      make(~times=1, ~objects=list{makeStone(1.65)}, Straight),
      make(~times=12, Left(hc2)),
      make(~times=6, Straight),
      makeCheckpoint(9),
      make(
        ~times=6,
        ~objects=list{smallTree(-1.35), makeTree(-1.9), smallTree(1.35), makeTree(1.9)},
        Straight,
      ),
      make(~times=36, ~objects=list{makeSignRight}, Right(mc2)),
      make(~times=12, Left(hc2)),
      make(~times=12, Straight),
      make(~times=18, ~objects=list{makeSignLeft}, Left(hc2)),
      make(~times=6, ~objects=list{makeSignLeft}, Straight),
      make(~times=6, Left(1.)),
      make(~times=12, Straight),
      make(~times=2, ~incline=-1., Straight),
      make(~times=2, ~incline=-2., Straight),
      make(~times=36, ~incline=-2.4, Right(mc3)),
      make(~times=2, ~incline=-2., Straight),
      make(~times=2, ~incline=-1., Straight),
      make(~times=6, Straight),
      makeCheckpoint(9),
      make(~times=3, Straight),
      make(~times=2, ~incline=0.5, Straight),
      make(~times=1, ~incline=-1., Straight),
      make(~times=1, ~incline=-2., Straight),
      make(~times=1, ~objects=list{smallTree(1.17), makeTree(1.55)}, ~incline=-3., Straight),
      make(~times=1, ~incline=-4., ~objects=list{makeStone(-1.75), makeStone(-1.5)}, Straight),
      make(~times=1, ~incline=-5., ~objects=list{makeStone(1.75)}, Straight),
      make(~times=8, ~incline=-6., Straight),
      make(~times=6, ~objects=list{smallTree(1.35), makeStone(1.95)}, ~incline=-7., Right(ec2)),
      make(~times=14, ~incline=-7., Left(ec2)),
      make(~times=8, ~incline=-7.5, Straight),
      make(~times=8, ~incline=-8., Right(ec2)),
      make(~times=12, ~objects=list{makeSignRight}, ~incline=-8.5, Right(ec4)),
      make(~times=8, ~incline=-8., Right(mc2)),
      make(~times=6, ~objects=list{makeStone(1.95)}, ~incline=-7., Right(ec2)),
      make(~times=8, ~incline=-5., Straight),
      make(~times=4, ~incline=-4., ~objects=bunchOfSmallTrees(1.3, 1.5, 1.7), Straight),
      make(~times=2, ~incline=-3., Straight),
      make(~times=2, ~incline=-2., ~objects=bunchOfSmallTrees(1.35, 1.5, 1.65), Straight),
      make(~times=2, ~incline=-1., ~objects=list{makeTree(1.6)}, Straight),
      make(~times=2, ~objects=list{smallTree(1.35), makeTree(1.95)}, Right(ec2)),
      make(~times=2, ~objects=bunchOfSmallTrees(-1.3, 1.5, -1.7), Right(ec2)),
      make(~times=2, ~objects=list{smallTree(1.17), makeTree(1.55)}, Right(ec2)),
      make(~times=1, ~incline=1., Straight),
      make(~times=2, ~incline=1.5, Straight),
      make(~times=4, ~incline=2.4, Straight),
      make(~times=9, ~incline=3.5, Straight),
      make(~times=6, ~incline=2., Straight),
      make(~times=3, ~incline=0.5, Straight),
      make(~times=16, Left(ec1)),
      make(~times=8, Left(ec4)),
      make(~times=6, ~incline=1., Left(ec1)),
      make(~times=6, ~objects=list{makePost(0.)}, ~incline=0.2, Straight),
      make(~times=8, ~objects=list{makePost(0.)}, ~incline=0.8, Right(ec4)),
      make(~times=8, ~incline=2.2, ~objects=list{makePost(0.), makeSignRight}, Right(mc3)),
      make(~times=2, ~incline=3.2, Straight),
      make(~times=8, ~incline=5.2, Straight),
      make(~times=6, ~incline=6.1, Straight),
      make(~times=4, ~incline=5.8, ~objects=list{makePost(0.)}, Left(ec1)),
      make(~times=8, ~incline=6.5, ~objects=list{makePost(0.)}, Left(ec4)),
      make(~times=8, ~incline=7.5, ~objects=list{makeSignLeft}, Left(mc3)),
      make(~times=2, ~incline=8.8, Straight),
      makeCheckpoint(~incline=9.2, 6),
      make(~times=3, ~incline=9.9, ~objects=list{makePost(0.)}, Left(ec4)),
      make(~times=4, ~incline=12.4, ~objects=list{makePost(0.)}, Left(hc2)),
      make(~times=4, ~incline=9.4, Left(mc2)),
      make(~times=4, ~incline=8.4, ~objects=list{makePost(0.)}, Left(ec1)),
      make(~times=4, ~incline=6.2, ~objects=list{makePost(0.)}, Straight),
      make(~times=4, ~incline=4.2, Straight),
      make(~times=2, ~incline=5.4, Straight),
      make(~times=4, ~incline=6.2, Right(ec1)),
      make(~times=4, ~incline=6.6, Right(ec2)),
      make(~times=8, ~incline=7.1, Right(ec4)),
      make(~times=8, ~incline=6.9, Right(mc2)),
      make(~times=8, ~incline=6.4, ~objects=list{makePost(0.)}, Right(mc3)),
      make(~times=8, ~incline=6.1, ~objects=list{makePost(0.), makeSignRight}, Right(hc2)),
      make(~times=4, ~incline=5.2, Straight),
      make(~times=2, ~incline=4.7, Straight),
      make(~times=2, ~incline=4.4, Straight),
      make(~times=6, ~incline=4.2, Straight),
      make(~times=1, ~incline=4.5, ~objects=list{makeStone(-0.2)}, Straight),
      make(~times=1, ~incline=4.5, ~objects=list{makeStone(-0.4)}, Straight),
      make(~times=6, ~incline=5.1, Left(ec4)),
      make(~times=4, ~incline=6.2, Straight),
      make(~times=4, ~incline=4.2, Straight),
      make(~times=2, ~incline=2.2, Straight),
      make(~times=1, ~incline=1.2, ~objects=list{makeStone(-0.8)}, Straight),
      make(~times=1, ~incline=1.2, Straight),
      make(~times=1, ~incline=0.5, Straight),
      make(
        ~times=1,
        ~incline=0.,
        ~objects=list{makeStone(0.3), makeStone(0.6), makeStone(0.9)},
        Straight,
      ),
      make(~times=1, ~incline=-0.5, Straight),
      makeCheckpoint(~incline=-1.3, 9),
      make(~times=1, ~incline=-1.2, Straight),
      make(~times=1, ~incline=-1.2, ~objects=list{makeStone(-0.2)}, Straight),
      make(~times=8, ~incline=-2.4, Left(ec4)),
      make(~times=12, ~incline=-3.6, Straight),
      make(~times=8, ~incline=-2.4, Straight),
      make(~times=8, ~incline=-1.2, Straight),
      make(~times=4, ~incline=0., Straight),
      make(~times=4, ~incline=1., Straight),
      make(~times=4, ~incline=4., Straight),
      make(~times=12, ~incline=5., Straight),
      make(~times=4, ~incline=5.4, Straight),
      make(~times=4, ~incline=3.2, Straight),
      make(~times=4, ~incline=1.1, Straight),
      make(~times=8, ~incline=0.2, ~objects=list{makeSignRight}, Right(mc3)),
      make(~times=8, ~incline=-2.2, Right(ec4)),
      make(~times=8, ~incline=-0.2, Straight),
      make(~times=4, ~incline=1.2, Straight),
      make(~times=4, ~incline=0.2, Straight),
      make(~times=4, ~incline=-1.2, Straight),
      make(~times=4, ~incline=-0.2, Straight),
      make(~times=6, ~incline=1.2, Left(ec1)),
      make(~times=14, ~incline=3.2, Left(ec4)),
      make(~times=10, ~incline=5.2, Left(ec2)),
      make(~times=8, ~incline=3.5, Left(ec4)),
      make(~times=4, ~incline=2.1, Left(ec1)),
      make(~times=4, ~incline=1.1, Left(ec1)),
      make(~times=4, ~incline=0.5, Left(ec1)),
      make(~times=4, Straight),
      makeCheckpoint(6),
      make(~times=4, Left(ec2)),
      make(~times=1, ~objects=list{makeStone(-0.9)}, Left(ec2)),
      make(~times=8, Left(ec2)),
      make(~times=1, ~incline=0.4, ~objects=list{makeStone(-0.8)}, Left(ec2)),
      make(~times=1, ~incline=-0.8, ~objects=list{makeStone(-0.85), makeStone(-0.65)}, Left(ec2)),
      make(~times=5, ~incline=0.3, Left(ec2)),
      make(~times=1, ~objects=list{makeStone(0.95)}, Left(ec2)),
      make(~times=4, Left(ec2)),
      make(~times=1, ~objects=list{makeStone(-0.75), makeStone(-0.5)}, Left(ec2)),
      make(~times=1, ~incline=-0.6, ~objects=list{makeStone(0.0), makeStone(-0.35)}, Left(ec2)),
      make(~times=5, ~incline=-1.2, Left(ec2)),
      make(~times=1, ~incline=-0.4, ~objects=list{makeStone(0.2), makeStone(0.35)}, Left(ec2)),
      make(~times=6, Left(ec2)),
      make(~times=1, ~objects=list{makeStone(0.8), makeStone(-0.35)}, Left(ec2)),
      make(~times=1, ~objects=list{makeStone(-0.8), makeStone(0.6), makeStone(0.9)}, Left(ec2)),
      make(~times=16, Left(ec2)),
      make(~times=4, Left(ec2)),
      make(~times=9, ~objects=list{makeSignRight}, Right(ec2)),
      make(~times=4, ~incline=1.1, Left(ec1)),
      make(~times=4, ~incline=0.5, Left(ec1)),
      makeCheckpoint(19),
    } |> List.concat

  let make = (
    ~times=1,
    ~objects=list{},
    ~incline=0.,
    ~roadSurface=Dirt,
    ~groundSurface=Gravel,
    direction,
  ) => make(~times, ~objects, ~incline, ~roadSurface, ~groundSurface, direction)
  let track2 =
    list{
      make(~times=8, ~incline=-1.5, Straight),
      make(~times=10, ~incline=-1.3, Left(ec2)),
      make(
        ~times=1,
        ~roadSurface=Dirt,
        ~groundSurface=Gravel,
        ~incline=-1.4,
        ~objects=list{makeStone(0.72), makeStone(0.9)},
        Left(ec2),
      ),
      make(~times=6, ~incline=-1.3, Left(ec2)),
      make(~times=1, Left(ec1)),
      make(~times=22, ~incline=-2.3, Right(ec3)),
      make(~times=4, ~incline=-1., Straight),
      make(~times=4, ~incline=-0.5, Straight),
      make(~times=12, ~incline=-0.2, Right(hc3)),
      make(~times=6, Straight),
      make(~times=11, Left(mc1)),
      make(~times=1, ~objects=list{makeStone(0.8)}, Left(mc1)),
      make(~times=10, Left(mc1)),
      make(~times=4, ~incline=0.5, Straight),
      make(~times=4, ~incline=1.5, Straight),
      make(~times=4, ~incline=2.3, Straight),
      make(~times=4, ~incline=3.5, Straight),
      make(~times=10, ~incline=4.2, Straight),
      make(~times=1, ~objects=list{makeStone(0.5), makeStone(0.8)}, ~incline=4.7, Straight),
      make(~times=10, ~incline=4.9, Straight),
      make(~times=1, ~objects=list{makeStone(-0.9), makeStone(-0.55)}, ~incline=4.8, Straight),
      make(~times=4, ~incline=5.2, Straight),
      make(~times=2, ~incline=5.3, Straight),
      make(~times=4, ~incline=5.6, Straight),
      makeCheckpoint(~incline=5.4, 16),
      make(~times=10, ~incline=5.2, ~objects=edgePosts, Straight),
      make(~times=6, ~incline=5.0, ~objects=edgePosts, Right(ec3)),
      make(~times=14, ~incline=5.4, ~objects=edgePosts, Right(mc1)),
      make(~times=6, ~incline=5.2, ~objects=edgePosts, Right(ec3)),
      make(~times=8, ~incline=4.2, ~groundSurface=Gravel, Straight),
      make(~times=3, ~incline=3.2, ~groundSurface=Gravel, Straight),
      make(~times=4, ~incline=2.2, ~objects=edgePosts, Left(ec4)),
      make(~times=12, ~incline=1.2, ~objects=edgePosts, Left(mc4)),
      make(~times=4, ~incline=1., ~objects=edgePosts, Left(ec2)),
      make(~times=16, ~incline=0.6, ~objects=edgePosts, Left(mc4)),
      make(~times=12, ~incline=0.2, ~objects=edgePosts, Right(hc1)),
      make(~times=4, ~incline=-0.2, ~objects=edgePosts, Right(ec2)),
      make(~times=4, ~incline=-0.4, ~objects=edgePosts, Straight),
      make(~times=4, ~incline=-0.2, Straight),
      make(~times=8, Straight),
      make(~times=1, ~objects=list{makeStone(-0.9)}, Straight),
      make(~times=1, ~incline=-0.3, ~objects=list{makeStone(-0.7)}, Straight),
      make(~times=1, ~incline=-0.4, ~objects=list{makeStone(-0.5)}, Straight),
      make(~times=1, ~incline=-0.4, ~objects=list{makeStone(-0.8), makeStone(-0.7)}, Straight),
      make(~times=1, ~incline=-0.4, ~objects=list{makeStone(-0.3)}, Straight),
      make(~times=1, ~incline=-0.4, ~objects=list{makeStone(-0.), makeStone(0.9)}, Straight),
      make(~times=1, ~incline=-0.4, ~objects=list{makeStone(0.3), makeStone(-0.7)}, Straight),
      make(~times=4, ~incline=-0.6, Straight),
      make(~times=12, ~incline=-1.2, Straight),
      make(~times=4, ~incline=-0.6, Straight),
      make(~times=4, ~incline=-0.2, Straight),
      make(~times=6, Straight),
      make(~times=4, ~incline=0.6, Straight),
      make(~times=6, ~incline=1.6, Straight),
      make(~times=4, ~incline=3.1, Right(hc1)),
      make(~times=12, ~incline=4.6, Right(hc2)),
      make(~times=8, ~incline=3.6, Right(mc2)),
      make(~times=4, ~incline=2.6, Straight),
      make(~times=4, ~incline=1., Straight),
      make(~times=4, ~incline=0.2, Straight),
      make(~times=4, Straight),
      make(~times=8, ~incline=-0.6, Straight),
      make(~times=8, ~incline=-1.6, Straight),
      make(~times=8, ~incline=-0.6, Straight),
      make(~times=4, Straight),
      make(~times=8, Left(ec4)),
      make(~times=6, ~incline=1., Left(ec1)),
      make(~times=6, ~incline=0.2, Straight),
      make(~times=12, ~incline=0.8, Right(ec4)),
      make(~times=14, ~incline=2.2, ~objects=list{makeSignRight}, Right(mc3)),
      make(~times=6, ~incline=3.4, Right(ec3)),
      make(~times=2, ~incline=4.2, Straight),
      make(~times=8, ~incline=5.2, Straight),
      make(~times=6, ~incline=6.1, Straight),
      make(~times=8, ~incline=5.8, ~objects=edgePosts, Left(mc1)),
      make(~times=14, ~incline=6.5, ~objects=edgePosts, Left(hc1)),
      make(~times=8, ~incline=7.5, ~objects=list{makeSignLeft}, Left(mc3)),
      make(~times=4, ~incline=7.8, ~objects=list{makeSignLeft}, Left(ec2)),
      make(~times=2, ~incline=8.8, Straight),
      makeCheckpoint(~incline=9.2, 16),
      make(~times=12, ~incline=9.9, ~objects=edgePosts, Left(ec4)),
      make(~times=2, ~incline=10.1, Straight),
      make(~times=16, ~incline=10.9, ~objects=edgePosts, Right(mc1)),
      make(~times=10, ~incline=12.4, ~objects=edgePosts, Right(hc2)),
      make(~times=10, ~incline=11.4, ~objects=edgePosts, Right(ec2)),
      make(~times=4, ~incline=9.4, Right(mc2)),
      make(~times=8, ~incline=8.4, ~objects=edgePosts, Left(ec1)),
      make(~times=12, ~incline=6.2, ~objects=edgePosts, Straight),
      make(~times=6, ~incline=4.5, Left(ec3)),
      make(~times=18, ~incline=4.2, ~objects=edgePosts, Left(mc3)),
      make(~times=24, ~incline=4.3, ~objects=edgePosts, Left(hc1)),
      make(~times=28, ~incline=4.1, ~objects=edgePosts, Left(hc3)),
      make(~times=12, ~incline=4.0, ~objects=edgePosts,Right(mc1)),
      make(~times=12, ~incline=3.0, Right(ec4)),
      make(~times=12, ~incline=2.0, Straight),
      make(~times=1, ~incline=1.8, ~objects=list{makeStone(0.8)}, Straight),
      make(~times=1, ~incline=1.5, ~objects=list{makeStone(0.6), makeStone(0.9)}, Straight),
      make(~times=12, ~incline=0.8, ~objects=edgePosts,Right(ec4)),
      make(~times=4, ~incline=0.6, ~objects=edgePosts,Right(hp1)),
      make(~times=18, ~incline=0.4, ~objects=edgePosts,Right(mc2)),
      make(~times=6, ~incline=0.2, ~objects=edgePosts,Right(mc1)),
      make(~times=8, ~incline=0., Straight),
      make(~times=1, ~incline=-1.2, ~objects=list{makeStone(-0.8)}, Straight),
      make(~times=1, ~incline=-1.2, ~objects=list{makeStone(-0.6), makeStone(-0.9)}, Straight),
      make(~times=12, ~incline=0., Straight),
      make(~times=1, ~incline=-1.2, ~objects=list{makeStone(0.)}, Straight),
      make(~times=1, ~incline=-1.2, ~objects=list{makeStone(-0.2), makeStone(0.2)}, Straight),
      make(~times=20, ~incline=0., ~objects=edgePosts, Right(mc3)),
      makeCheckpoint(~incline=1.2, 19),
    } |> List.concat

  track->List.append(track2)
}

Random.init(69)
let roll = ref(Random.int(9) + 1)
let reroll = () => roll.contents = Random.int(9) + 1
let positions = [-1.9, -1.8, -1.7, -1.6, -1.5, 1.5, 1.6, 1.7, 1.8, 1.9]
let track = demoTrack |> List.mapi((i, road) => {
  reroll()
  let t = mod(i, 10)
  roll.contents >= t
    ? {
        reroll()
        {
          ...road,
          objects: road.objects |> List.append(list{
            Object.Prefabs.makeTree(positions[roll.contents - 1]),
          }),
        }
      }
    : road
})

let init = {track: track}

let isCheckpoint = t =>
  switch t.direction {
  | Checkpoint(_) => true
  | _ => false
  }

let lastTrack = ref(track)
let progress = state =>
  List.length(state.track) > 106
    ? {track: List.tl(state.track)}
    : {
        lastTrack :=
          lastTrack.contents->List.rev |> List.map(t => {...t, incline: t.incline *. 1.1})
        {track: List.tl(List.append(state.track, lastTrack.contents))}
      }

let head = state => List.hd(state.track)
let tail = state => List.tl(state.track)
