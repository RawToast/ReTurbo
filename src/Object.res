let screenHeightF = Common.heightF
let baseWidth = Common.roadWidth
let baseLength = 40.

type objectType =
  | SIGN_RIGHT
  | SIGN_LEFT
  | TREE
  | STONE

type state = {
  objectType: objectType,
  offset: float,
  z: float,
  size: (int, int),
}

let makeSignRight = {objectType: SIGN_RIGHT, offset: -1.25, z: 0., size: (96, 96)}
let makeSignLeft = {objectType: SIGN_LEFT, offset: 1.25, z: 0., size: (96, 96)}

let makeTree = offset => {
  objectType: TREE,
  offset,
  z: 0.,
  size: (128, 216),
}
let smallTree = offset => {
  objectType: TREE,
  offset,
  z: 0.,
  size: (64, 108),
}
let makeStone = offset => {
  objectType: STONE,
  offset,
  z: 0.,
  size: (64, 64),
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
    {
      z: z,
      height: height,
      width: width,
      offset: offset,
      objectType: objectType,
    }
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
