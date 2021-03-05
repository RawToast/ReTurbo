/* Road constants */
let baseLength = 40.

type state = {
  position: float,
  lastPiece: int,
  track: Track.state,
}

let currentPlane = state => Track.head(state.track)
let currentPlane2 = state => state.track |> Track.tail |> List.hd

let moveForward = (newPosition, state) =>
  float_of_int(state.lastPiece) *. baseLength -. newPosition <= 0.
    ? {
        lastPiece: state.lastPiece + 1,
        position: newPosition,
        track: Track.progress(state.track),
      }
    : {...state, position: newPosition}

let onCheckpoint = state => state.track |> Track.head |> Track.isCheckpoint

let checkpointBonus = state =>
  state.track
  |> Track.head
  |> (
    p =>
      switch p.direction {
      | Track.Checkpoint(t) => t
      | _ => 0
      }
  )
let startTime = state =>
  state.track
  |> Track.head
  |> (
    p =>
      switch p.direction {
      | Track.Start(t) => t
      | _ => 0
      }
  )
let init = {position: 0., track: Track.init, lastPiece: 1}

%%private(
  let findInitialCoordinates = (offset, state) => {
    let (isLight, remainder) = {
      let adj = mod_float(state.position, baseLength *. 2.)
      adj >= baseLength ? (true, adj -. baseLength) : (false, adj)
    }
    let x0 = Common.centrePoint -. Common.roadWidth /. 2. +. offset
    let x1 = Common.centrePoint +. Common.roadWidth /. 2. +. offset
    (x0, x1, remainder, isLight)
  }
)

module Display = {
  type colour = {
    r: int,
    g: int,
    b: int,
    a: int,
  }
  type t = {
    x: float,
    y: float,
    z: float,
    previous: (float, float, float),
    colour: colour,
    terrainColour: colour,
    objects: list<Object.Display.t>,
  }

  %%private(let red = {r: 150, g: 80, b: 80, a: 255})

  %%private(let lightGrey = {r: 78, g: 78, b: 78, a: 255})
  %%private(let darkGrey = {r: 70, g: 70, b: 70, a: 255})
  
  %%private(let roadLightGrey = {r: 62, g: 62, b: 62, a: 255})
  %%private(let roadDarkGrey = {r: 56, g: 56, b: 56, a: 255})

  %%private(let roadBrown = {r: 84, g: 66, b: 33, a: 255})
  %%private(let roadDarkBrown = {r: 70, g: 55, b: 30, a: 255})
  
  %%private(let lightGreen = {r: 45, g: 140, b: 30, a: 255})
  %%private(let darkGreen = {r: 30, g: 120, b: 30, a: 255})
  
  %%private(let lightBrown = {r: 82, g: 59, b: 32, a: 255})
  %%private(let darkBrown = {r: 70, g: 50, b: 30, a: 255})
  
  %%private(let lightBlue = {r: 45, g: 40, b: 140, a: 255})
  %%private(let darkBlue = {r: 36, g: 32, b: 130, a: 255}) 
  
  let make = (~offset, state) => {
    let (_, _, remainder, isLight) = findInitialCoordinates(offset, state)
    // let iOffset = int_of_float(offset *. 0.4) /* interesting */
    let remainder = baseLength -. remainder
    let convert = (~remainder, ~isDark=isLight, track) => {
      let isDark = ref(isDark)
      let previous = ref(None)
      let ddx = ref(0.)
      track |> List.mapi((i, plane: Track.plane) => {
        let i = float_of_int(i)
        let {direction, objects, incline, roadSurface, groundSurface} = plane

        let objects = objects |> List.map(Object.Display.make)

        // Calc position
        let findXyz = (~previous, ~remainder, ~direction, ~incline, ~ddx) => {
          let curve = switch direction {
          | Track.Left(curve) => i == 0. ? remainder /. baseLength *. curve *. -2. : curve *. -2.
          | Right(curve) => i == 0. ? remainder /. baseLength *. curve *. 2. : curve *. 2.
          | _ => 0.
          }
          let curve = curve +. ddx
          let ddx = curve
          let prev = switch previous {
          | Some(xyz) => xyz
          | None => (0., 50., 0.)
          }
          let (px, py, _pz) = prev

          let x = px +. curve
          let yFactor = 0.36 *. incline
          let y = i == 0. ? py +. yFactor *. remainder /. baseLength : py +. yFactor
          let z = i *. baseLength +. remainder

          ((x, y, z), ddx, prev)
        }

        let ((x, y, z), newddx, prev) = findXyz(
          ~previous=previous.contents,
          ~remainder,
          ~direction,
          ~incline,
          ~ddx=ddx.contents,
        )

        previous := Some((x, y, z))
        ddx := newddx

        let isCheckpoint = Track.isCheckpoint(plane)

        let dark = isDark.contents

        let terrainColour = switch groundSurface {
        | Grass => dark ? darkGreen : lightGreen
        | Soil => dark ? darkBrown : lightBrown
        | Water => dark ? darkBlue : lightBlue
        | Gravel => dark ? darkGrey : lightGrey
        }

        let colour = switch roadSurface {
          | _ when isCheckpoint => red
          | Tarmac when dark => roadLightGrey 
          | Tarmac => roadDarkGrey
          | Dirt when dark => roadBrown 
          | Dirt => roadDarkBrown 
        }

        let result = {
          x: x,
          y: y,
          z: z,
          previous: prev,
          colour: isCheckpoint ? red : isDark.contents ? lightGrey : darkGrey,
          terrainColour: isDark.contents ? darkGreen : lightGreen,
          objects: objects,
        }

        isDark := !isDark.contents
        result
      })
    }

    convert(~remainder, ~isDark=isLight, state.track.track)
  }
}
