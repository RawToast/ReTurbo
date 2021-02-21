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
  x: int,
  y: int,
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

let findPosition = (quad: RoadCalc.roadQuad, obj: Track.Obsticle.state) => {
  let {
    objectType: _,
    Track.Obsticle.location: location,
    offset: (offsetX, _),
    size: (sizeX, sizeY),
  } = obj
  let ((xl, by), (xr, _), (_, ty)) = (quad.leftBottom, quad.rightBottom, quad.rightTop)
  let (sizeX, sizeY) = (float_of_int(sizeX), float_of_int(sizeY))
  let roadHeight = by -. ty
  let heightAdjustFactor = if by >= screenHeightF {
    1.
  } else {
    roadHeight /. baseLength
  }
  let widthAdjustFactor = heightAdjustFactor
  let objectHeight = sizeX *. heightAdjustFactor
  let objectWidth = sizeY *. widthAdjustFactor

  let objectOffsetX = offsetX *. widthAdjustFactor

  let objY =
    by >= 319.
      ? {
          let remainingRoad = baseLength -. roadHeight
          int_of_float(by -. objectHeight +. remainingRoad)
        }
      : int_of_float(by -. objectHeight)

  let objExtraX =
    by >= 319.
      ? {
          let remaningRoad = baseLength -. roadHeight
          switch location {
          | Track.Obsticle.LEFT => remaningRoad /. tan(quad.leftAngle)
          | Track.Obsticle.RIGHT => remaningRoad /. tan(quad.rightAngle)
          | Track.Obsticle.CENTRE =>
            switch offsetX {
            | 0. => 0.
            | _ if offsetX > 0. => remaningRoad /. tan(quad.rightAngle) /. 3.
            | _ if offsetX < 0. => remaningRoad /. tan(quad.leftAngle) /. 3.
            | _ => 0.
            }
          }
        }
      : 0.

  switch location {
  | Track.Obsticle.LEFT =>
    let objX = int_of_float(xl -. objExtraX -. objectWidth +. objectOffsetX)
    (objX, objY, int_of_float(objectHeight), int_of_float(objectWidth))
  | Track.Obsticle.RIGHT =>
    let objX = int_of_float(xr +. objExtraX +. objectOffsetX)
    (objX, objY, int_of_float(objectHeight), int_of_float(objectWidth))
  | Track.Obsticle.CENTRE =>
    let objX = int_of_float((xr +. xl) /. 2. +. objectOffsetX)
    (objX, objY, int_of_float(objectHeight), int_of_float(objectWidth))
  }
}

let calculatePositions = (trackPiece: Track.plane, quad) => {
  let obsticles: list<Track.Obsticle.state> = trackPiece.obsticles

  obsticles |> List.map((obs: Track.Obsticle.state) => {
    let (x, y, height, width) = findPosition(quad, obs)
    {x: x, y: y, height: height, width: width, objectType: obs.objectType}
  })
}

let draw = (objects, assets, env) =>
  objects |> List.iter(o =>
    switch o.objectType {
    | SIGN_RIGHT =>
      Draw.image(assets.roadSignRight, ~pos=(o.x, o.y), ~width=o.width, ~height=o.height, env)
    | SIGN_LEFT =>
      Draw.image(assets.roadSignLeft, ~pos=(o.x, o.y), ~width=o.width, ~height=o.height, env)
    | TREE => Draw.image(assets.tree, ~pos=(o.x, o.y), ~width=o.width, ~height=o.height, env)
    | STONE => Draw.image(assets.stone, ~pos=(o.x, o.y), ~width=o.width, ~height=o.height, env)
    }
  )
