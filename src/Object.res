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

  let carSizeFactor = 55./. roadWidth
  let playerOffset = playerPosition *. -1.

  let (playerOff1, playerOff2) = (playerOffset -. carSizeFactor, playerOffset +. carSizeFactor)
  let adj = mod_float(distance, 40. *. 2.)
  let playerTravel = adj >= 40. ? adj -. 40. : adj

  let {objectType: _, offset, z, size: _, hitbox: (hbX, hbY)} = obj
  let factor = hbX /. roadWidth 

  let x1 = offset -. (factor /. 2.)
  let x2 = offset +. (factor /. 2.)
  let hbY = hbY *. 0.5

  let y1 = (z -.hbY) *. 0.5
  let y2 = z
  let inHbX = playerOff2 >= x1 && playerOff1 <= x2
  let inHbY = (playerTravel >= y1 && playerTravel <= y2)

  inHbX  && inHbY
}

let speedPenalty = obj =>
  switch obj.objectType {
  | SIGN_RIGHT
  | SIGN_LEFT => 14.
  | TREE => {
      let (x, _) = obj.size
      x > 100 ? 15. : 6.
    }
  | STONE => 22.
  | POST => 9.
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
    z: 16.,
    size: (96, 96),
    hitbox: (96., 4.),
  }
  let makeSignLeft = {
    objectType: SIGN_LEFT,
    offset: 1.25,
    z: 16.,
    size: (96, 96),
    hitbox: (96., 5.),
  }
  let makeTree = offset => {
    objectType: TREE,
    offset: offset,
    z: 16.,
    size: (128, 216),
    hitbox: (128., 12.),
  }
  let smallTree = offset => {
    objectType: TREE,
    offset: offset,
    z: 16.,
    size: (64, 108),
    hitbox: (64., 8.),
  }
  let makeStone = offset => {
    objectType: STONE,
    offset: offset,
    z: 16.,
    size: (64, 64),
    hitbox: (64., 12.),
  }
  let makePost = offset => {
    objectType: POST,
    offset: offset,
    z: 16., // 6, 19
    size: (12, 38),
    hitbox: (4., 4.),
  }
}
