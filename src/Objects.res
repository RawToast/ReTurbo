open Reprocessing

let screenHeightF = Common.heightF
let baseWidth = Common.roadWidth
let baseLength = 40.

type assets = {
  roadSignRight: Reprocessing_Common.imageT,
  roadSignLeft: Reprocessing_Common.imageT,
  tree: Reprocessing_Common.imageT,
  stone: Reprocessing_Common.imageT,
}

type state = {
  offset: float,
  height: int,
  width: int,
  objectType: Track.Obsticle.objectType,
}

let loadAssets = env => {
  roadSignLeft: Draw.loadImage(~filename="assets/roadsign_left.png", ~isPixel=true, env),
  roadSignRight: Draw.loadImage(~filename="assets/roadsign.png", ~isPixel=true, env),
  tree: Draw.loadImage(~filename="assets/tree.png", ~isPixel=true, env),
  stone: Draw.loadImage(~filename="assets/stone.png", ~isPixel=true, env),
}

let init = env => loadAssets(env)

module Display = {
  type t = {
    offset: float,
    height: float,
    width: float,
    z: float,
    objectType: Track.Obsticle.objectType,
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


  let make = (obj: Track.Obsticle.state) => {
    let (height, width) = obj.size

    make(
      ~offset=obj.offset,
      ~height=float_of_int(height),
      ~width=float_of_int(width),
      obj.objectType
    )
  }
}
