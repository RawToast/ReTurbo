type direction =
  | Straight
  | Left(float)
  | Right(float)
  | Checkpoint(int)

module Obsticle = {
  type objectType =
    | SIGN_RIGHT
    | SIGN_LEFT
    | STONE
    | TREE

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
  let makeTree = (~where=RIGHT, x) => {
    objectType: TREE,
    location: where,
    offset: (x, 0.),
    size: (208, 128),
  }
  let smallTree = (~where=LEFT, x) => {
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
                                                                                                          ~times=8,
                                                                                                          Straight,
                                                                                                        ),
                                                                                                        make(
                                                                                                          ~times=2,
                                                                                                          ~obsticles=list{
                                                                                                            Obsticle.makeStone(
                                                                                                              ~where=Obsticle.LEFT,
                                                                                                              -30.,
                                                                                                            ),
                                                                                                          },
                                                                                                          Straight,
                                                                                                        ),
                                                                                                      ),
                                                                                                      make(
                                                                                                        ~times=4,
                                                                                                        ~obsticles=list{
                                                                                                          Obsticle.makeSignRight,
                                                                                                        },
                                                                                                        Right(
                                                                                                          ec2,
                                                                                                        ),
                                                                                                      ),
                                                                                                    ),
                                                                                                    make(
                                                                                                      ~obsticles=list{
                                                                                                        Obsticle.smallTree(
                                                                                                          ~where=Obsticle.RIGHT,
                                                                                                          20.,
                                                                                                        ),
                                                                                                        Obsticle.makeTree(
                                                                                                          350.,
                                                                                                        ),
                                                                                                      },
                                                                                                      Straight,
                                                                                                    ),
                                                                                                  ),
                                                                                                  make(
                                                                                                    ~obsticles=list{
                                                                                                      Obsticle.makeTree(
                                                                                                        50.,
                                                                                                      ),
                                                                                                      Obsticle.makeTree(
                                                                                                        280.,
                                                                                                      ),
                                                                                                      Obsticle.smallTree(
                                                                                                        -120.,
                                                                                                      ),
                                                                                                    },
                                                                                                    Straight,
                                                                                                  ),
                                                                                                ),
                                                                                                make(
                                                                                                  ~obsticles=list{
                                                                                                    Obsticle.smallTree(
                                                                                                      ~where=Obsticle.LEFT,
                                                                                                      20.,
                                                                                                    ),
                                                                                                    Obsticle.makeTree(
                                                                                                      260.,
                                                                                                    ),
                                                                                                  },
                                                                                                  Straight,
                                                                                                ),
                                                                                              ),
                                                                                              make(
                                                                                                ~obsticles=list{
                                                                                                  Obsticle.makeTree(
                                                                                                    50.,
                                                                                                  ),
                                                                                                  Obsticle.makeTree(
                                                                                                    410.,
                                                                                                  ),
                                                                                                  Obsticle.smallTree(
                                                                                                    -90.,
                                                                                                  ),
                                                                                                },
                                                                                                Straight,
                                                                                              ),
                                                                                            ),
                                                                                            make(
                                                                                              ~obsticles=list{
                                                                                                Obsticle.makeTree(
                                                                                                  100.,
                                                                                                ),
                                                                                                Obsticle.makeTree(
                                                                                                  350.,
                                                                                                ),
                                                                                              },
                                                                                              Straight,
                                                                                            ),
                                                                                          ),
                                                                                          make(
                                                                                            ~times=2,
                                                                                            Straight,
                                                                                          ),
                                                                                        ),
                                                                                        make(
                                                                                          ~obsticles=list{
                                                                                            Obsticle.makeStone(
                                                                                              -10.,
                                                                                            ),
                                                                                            Obsticle.makeStone(
                                                                                              90.,
                                                                                            ),
                                                                                            Obsticle.makeStone(
                                                                                              280.,
                                                                                            ),
                                                                                          },
                                                                                          Straight,
                                                                                        ),
                                                                                      ),
                                                                                      make(
                                                                                        ~times=2,
                                                                                        Straight,
                                                                                      ),
                                                                                    ),
                                                                                    make(
                                                                                      ~obsticles=list{
                                                                                        Obsticle.makeStone(
                                                                                          -220.,
                                                                                        ),
                                                                                      },
                                                                                      Straight,
                                                                                    ),
                                                                                  ),
                                                                                  make(
                                                                                    ~obsticles=list{
                                                                                      Obsticle.makeStone(
                                                                                        -290.,
                                                                                      ),
                                                                                      Obsticle.makeStone(
                                                                                        -120.,
                                                                                      ),
                                                                                    },
                                                                                    Straight,
                                                                                  ),
                                                                                ),
                                                                                make(
                                                                                  ~obsticles=list{
                                                                                    Obsticle.makeStone(
                                                                                      -30.,
                                                                                    ),
                                                                                  },
                                                                                  Straight,
                                                                                ),
                                                                              ),
                                                                              make(
                                                                                ~obsticles=list{
                                                                                  Obsticle.makeTree(
                                                                                    50.,
                                                                                  ),
                                                                                  Obsticle.makeTree(
                                                                                    400.,
                                                                                  ),
                                                                                },
                                                                                Straight,
                                                                              ),
                                                                            ),
                                                                            make(
                                                                              ~obsticles=list{
                                                                                Obsticle.makeTree(
                                                                                  100.,
                                                                                ),
                                                                                Obsticle.makeTree(
                                                                                  250.,
                                                                                ),
                                                                              },
                                                                              Straight,
                                                                            ),
                                                                          ),
                                                                          make(
                                                                            ~times=24,
                                                                            Left(ec1),
                                                                          ),
                                                                        ),
                                                                        make(
                                                                          ~times=8,
                                                                          ~obsticles=list{
                                                                            Obsticle.makeSignLeft,
                                                                          },
                                                                          Left(mc1),
                                                                        ),
                                                                      ),
                                                                      make(
                                                                        ~times=4,
                                                                        ~obsticles=list{
                                                                          Obsticle.makeSignRight,
                                                                        },
                                                                        Straight,
                                                                      ),
                                                                    ),
                                                                    make(
                                                                      ~times=12,
                                                                      ~obsticles=list{
                                                                        Obsticle.makeSignRight,
                                                                      },
                                                                      Right(ec2),
                                                                    ),
                                                                  ),
                                                                  make(~times=8, Straight),
                                                                ),
                                                                make(~times=8, Right(ec1)),
                                                              ),
                                                              make(~times=12, Right(ec1)),
                                                            ),
                                                            make(~times=8, Right(ec2)),
                                                          ),
                                                          make(~times=8, Right(mc1)),
                                                        ),
                                                        make(
                                                          ~times=8,
                                                          ~obsticles=list{
                                                            Obsticle.smallTree(
                                                              ~where=Obsticle.LEFT,
                                                              -120.,
                                                            ),
                                                            Obsticle.makeTree(120.),
                                                          },
                                                          Right(mc1),
                                                        ),
                                                      ),
                                                      make(
                                                        ~times=8,
                                                        ~obsticles=list{
                                                          Obsticle.makeTree(
                                                            ~where=Obsticle.LEFT,
                                                            -150.,
                                                          ),
                                                          Obsticle.makeTree(150.),
                                                        },
                                                        Right(mc2),
                                                      ),
                                                    ),
                                                    make(
                                                      ~times=2,
                                                      ~obsticles=list{
                                                        Obsticle.smallTree(
                                                          ~where=Obsticle.LEFT,
                                                          -120.,
                                                        ),
                                                        Obsticle.makeTree(120.),
                                                      },
                                                      Right(hc1),
                                                    ),
                                                  ),
                                                  make(
                                                    ~times=8,
                                                    ~obsticles=list{
                                                      Obsticle.smallTree(
                                                        ~where=Obsticle.LEFT,
                                                        -120.,
                                                      ),
                                                      Obsticle.makeTree(120.),
                                                    },
                                                    Straight,
                                                  ),
                                                ),
                                                make(~times=4, Straight),
                                              ),
                                              make(~times=4, Left(mc2)),
                                            ),
                                            make(
                                              ~times=4,
                                              ~obsticles=list{Obsticle.makeSignRight},
                                              Right(mc2),
                                            ),
                                          ),
                                          make(~times=12, Straight),
                                        ),
                                        make(
                                          ~times=4,
                                          ~obsticles=list{Obsticle.makeSignLeft},
                                          Left(hc1),
                                        ),
                                      ),
                                      make(
                                        ~times=4,
                                        ~obsticles=list{Obsticle.makeSignLeft},
                                        Left(hc2),
                                      ),
                                    ),
                                    make(~times=8, Straight),
                                  ),
                                  make(~times=4, Right(1.)),
                                ),
                                make(~times=4, Straight),
                              ),
                              make(~times=8, Left(hc2)),
                            ),
                            make(~times=4, Straight),
                          ),
                          makeCheckpoint(12),
                        ),
                        make(~times=4, Straight),
                      ),
                      make(~times=24, ~obsticles=list{Obsticle.makeSignRight}, Right(mc2)),
                    ),
                    make(~times=8, Left(hc2)),
                  ),
                  make(~times=8, Straight),
                ),
                make(~times=12, ~obsticles=list{Obsticle.makeSignLeft}, Left(hc2)),
              ),
              make(~times=4, Straight),
            ),
            make(~times=4, Left(1.)),
          ),
          make(~times=8, Straight),
        ),
        make(~times=24, Right(hc1)),
      ),
      make(~times=4, Straight),
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
