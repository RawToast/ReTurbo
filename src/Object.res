type objectType =
  | SIGN_RIGHT
  | SIGN_LEFT
  | TREE
  | STONE
  | POST

type state = {
  objectType: objectType,
  offset: float,
  z: float,
  size: (int, int),
  hitbox: (float, float),
}

let calcHit = (playerPosition, distance, roadWidth, obj) => {
  // let playerPosition = player.position
  // let playerTravel = player.playerOffset
  // let cameraDepth = 1. /. tan(80. /. 2. *. 3.1459)
  // let scale = cameraDepth /. 9.

  let carSizeFactor = (70. *. 0.5) /. (1. *. roadWidth)
  let playerOffset = -1. *. (playerPosition +. 0.) /. (roadWidth *. 1. *. 0.5)
  let (playerOff1, playerOff2) = (playerOffset -. carSizeFactor, playerOffset +. carSizeFactor)

  let adj = mod_float(distance, 40. *. 2.)
  let playerTravel = adj >= 40. ? adj -. 40. : adj

  let {objectType: _, offset, z, size: (_, _), hitbox: (hbX, hbY)} = obj
  let factor = hbX /. roadWidth 

  let x1 = offset -. (factor /. 2.)
  let x2 = offset +. (factor /. 2.)
  // Js.log4(playerOff1, playerOff2, x1, x2)
  let y1 = 0.
  let y2 = hbY +. z

  let inHbX = playerOff2 >= x1 && playerOff1 <= x2
  let inHbY = playerTravel >= y1 && playerTravel <= y2
  inHbX  && inHbY
}

let speedPenalty = obj =>
  switch obj.objectType {
  | SIGN_RIGHT
  | SIGN_LEFT => 9.
  | TREE => {
      let (x, _) = obj.size
      x > 100 ? 15. : 5.
    }
  | STONE => 12.
  | POST => 4.
  }

module Display = {
  type t = {
    offset: float,
    height: float,
    width: float,
    z: float,
    objectType: objectType,
  }

  let make = (~z=0., ~offset, ~height, ~width, objectType) => {
    z: z,
    height: height,
    width: width,
    offset: offset,
    objectType: objectType,
  }

  let make = (obj: state) => {
    let (width, height) = obj.size

    make(
      ~offset=obj.offset,
      ~height=float_of_int(height),
      ~width=float_of_int(width),
      obj.objectType,
    )
  }
}

module Prefabs = {
  let makeSignRight = {
    objectType: SIGN_RIGHT,
    offset: -1.25,
    z: 0.,
    size: (96, 96),
    hitbox: (96., 72.),
  }
  let makeSignLeft = {
    objectType: SIGN_LEFT,
    offset: 1.25,
    z: 0.,
    size: (96, 96),
    hitbox: (96., 72.),
  }
  let makeTree = offset => {
    objectType: TREE,
    offset: offset,
    z: 0.,
    size: (128, 216),
    hitbox: (128., 162.),
  }
  let smallTree = offset => {
    objectType: TREE,
    offset: offset,
    z: 0.,
    size: (64, 108),
    hitbox: (64., 81.),
  }
  let makeStone = offset => {
    objectType: STONE,
    offset: offset,
    z: 0.,
    size: (64, 64),
    hitbox: (64., 16.),
  }
  let makePost = offset => {
    objectType: POST,
    offset: offset,
    z: 0.,
    size: (40, 40),
    hitbox: (16., 10.),
  }
}
