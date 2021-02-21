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
    size: (208, 128),
  }
  let smallTree = (~where, x) => {
    objectType: TREE,
    location: where,
    offset: (x, 0.),
    size: (156, 80),
  }
  let makeStone = (~where=CENTRE, x) => {
    objectType: STONE,
    location: where,
    offset: (x, 0.),
    size: (64, 64),
  }
}

type plane = {
  direction: direction,
  obsticles: list<Obsticle.state>,
  incline: float,
}

type state = {track: list<plane>}
let \"|+|" = (a, b) => List.append(a, b)

let demoTrack = {
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

  \"|+|"(
    \"|+|"(
      \"|+|"(
        \"|+|"(
          \"|+|"(
            \"|+|"(
              \"|+|"(
                \"|+|"(
                  \"|+|"(
                    \"|+|"(
                      \"|+|"(
                        \"|+|"(
                          \"|+|"(
                            \"|+|"(
                              \"|+|"(
                                \"|+|"(
                                  \"|+|"(
                                    \"|+|"(
                                      \"|+|"(
                                        \"|+|"(
                                          \"|+|"(
                                            \"|+|"(
                                              \"|+|"(
                                                \"|+|"(
                                                  \"|+|"(
                                                    \"|+|"(
                                                      \"|+|"(
                                                        \"|+|"(
                                                          \"|+|"(
                                                            \"|+|"(
                                                              \"|+|"(
                                                                \"|+|"(
                                                                  \"|+|"(
                                                                    \"|+|"(
                                                                      \"|+|"(
                                                                        \"|+|"(
                                                                          \"|+|"(
                                                                            \"|+|"(
                                                                              make(
                                                                                ~times=24,
                                                                                Straight,
                                                                              ),
                                                                              make(
                                                                                ~times=2,
                                                                                ~obsticles=list{Obsticle.smallTree(~where=Obsticle.LEFT, -35.), Obsticle.makeTree(~where=Obsticle.LEFT, -90.), Obsticle.smallTree(~where=Obsticle.RIGHT, 35.), Obsticle.makeTree(~where=Obsticle.RIGHT, 90.)},
                                                                                Left(ec1),
                                                                              ),
                                                                            ),
                                                                            make(
                                                                              ~times=6,
                                                                              ~obsticles=list{
                                                                                Obsticle.makeSignRight,
                                                                              },
                                                                              Right(ec2),
                                                                            ),
                                                                          ),
                                                                          make(
                                                                            ~times=12,
                                                                            ~incline=3.,
                                                                            Straight,
                                                                          ),
                                                                        ),
                                                                        make(
                                                                          ~times=10,
                                                                          ~incline=9.,
                                                                          Straight,
                                                                        ),
                                                                      ),
                                                                      make(
                                                                        ~times=3,
                                                                        ~incline=4.,
                                                                        Straight,
                                                                      ),
                                                                    ),
                                                                    make(~times=32, Left(ec1)),
                                                                  ),
                                                                  make(
                                                                    ~times=12,
                                                                    ~obsticles=list{
                                                                      Obsticle.makeSignLeft,
                                                                    },
                                                                    Left(mc1),
                                                                  ),
                                                                ),
                                                                make(
                                                                  ~times=6,
                                                                  ~obsticles=list{
                                                                    Obsticle.makeSignRight,
                                                                  },
                                                                  Straight,
                                                                ),
                                                              ),
                                                              make(
                                                                ~times=18,
                                                                ~obsticles=list{
                                                                  Obsticle.makeSignRight,
                                                                },
                                                                Right(ec2),
                                                              ),
                                                            ),
                                                            make(~times=12, ~incline=-4., Straight),
                                                          ),
                                                          make(~times=18, Right(ec1)),
                                                        ),
                                                        make(~times=12, Right(ec2)),
                                                      ),
                                                      make(~times=12, Right(mc1)),
                                                    ),
                                                    make(~times=12, Right(mc2)),
                                                  ),
                                                  make(~times=2, ~obsticles=list{Obsticle.makeStone(~where=Obsticle.RIGHT, 55.)}, Right(hc1)),
                                                ),
                                                make(~times=18, Straight),
                                              ),
                                              make(~times=6, Left(mc2)),
                                            ),
                                            make(
                                              ~times=6,
                                              ~obsticles=list{Obsticle.makeSignRight},
                                              Right(mc2),
                                            ),
                                          ),
                                          make(~times=18, Straight),
                                        ),
                                        make(
                                          ~times=6,
                                          ~obsticles=list{Obsticle.makeSignLeft},
                                          Left(hc1),
                                        ),
                                      ),
                                      make(
                                        ~times=6,
                                        ~obsticles=list{Obsticle.makeSignLeft},
                                        Left(hc2),
                                      ),
                                    ),
                                    make(~times=12, Straight),
                                  ),
                                  make(~times=6, Right(1.)),
                                ),
                                make(~times=6, Straight),
                              ),
                              make(~times=12, Left(hc2)),
                            ),
                            make(~times=6, Straight),
                          ),
                          makeCheckpoint(12),
                        ),
                        make(~times=6, ~obsticles=list{Obsticle.smallTree(~where=Obsticle.LEFT, -35.), Obsticle.makeTree(~where=Obsticle.LEFT, -90.), Obsticle.smallTree(~where=Obsticle.RIGHT, 35.), Obsticle.makeTree(~where=Obsticle.RIGHT, 90.)}, Straight),
                      ),
                      make(~times=36, ~obsticles=list{Obsticle.makeSignRight}, Right(mc2)),
                    ),
                    make(~times=12, Left(hc2)),
                  ),
                  make(~times=12, Straight),
                ),
                make(~times=18, ~obsticles=list{Obsticle.makeSignLeft}, Left(hc2)),
              ),
              make(~times=6, Straight),
            ),
            make(~times=6, Left(1.)),
          ),
          make(~times=12, Straight),
        ),
        make(~times=36, Right(hc1)),
      ),
      make(~times=6, Straight),
    ),
    makeCheckpoint(5),
  )
}
let init = {track: demoTrack}

let isCheckpoint = t =>
  switch t.direction {
  | Checkpoint(_) => true
  | _ => false
  }

let progress = state =>
  List.length(state.track) > 25
    ? {track: List.tl(state.track)}
    : {track: List.tl(\"|+|"(state.track, demoTrack))}

let head = state => List.hd(state.track)
