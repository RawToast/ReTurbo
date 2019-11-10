open Reprocessing;

let height = float_of_int(Common.height);
let baseWidth = Common.roadWidth;
let baseLength = 40.;

type assets = {
  roadSignRight: Reprocessing_Common.imageT,
  roadSignLeft: Reprocessing_Common.imageT
};

type state = {
  x: int,
  y: int,
  height: int,
  width: int,
  objectType: Track.Obsticle.objectType
};

let loadAssets(env) = {
  roadSignLeft: Draw.loadImage(~filename="assets/roadsign_left.png", ~isPixel=true, env),
  roadSignRight: Draw.loadImage(~filename="assets/roadsign.png", ~isPixel=true, env)
};

let init(env) = loadAssets(env);

let findPosition = (~leftBased=true, quad: RoadCalc.roadQuad, offset, size) => {
  let (offsetX, _) = offset;
  let ((xl, by), (xr, _), (_, ty)) = (quad.leftBottom, quad.rightBottom, quad.rightTop);
  let size = float_of_int(size);
  let roadHeight = (by -. ty);
  let heightAdjustFactor = if (by >= height) {1.} else {roadHeight /. baseLength};
  let widthAdjustFactor = heightAdjustFactor;
  let objectHeight = size *. heightAdjustFactor;
  let objectWidth = size *. widthAdjustFactor;

  let objectOffsetX = offsetX *. widthAdjustFactor;

  let objY = (by >= 319.)? {
    let remaningRoad = (baseLength -. roadHeight);
    int_of_float(by -. objectHeight +. remaningRoad)
  } : (int_of_float(by -. objectHeight));

  let objExtraX = (by >= 319.)? {
    let remaningRoad = baseLength -. roadHeight;
    leftBased ? 
      remaningRoad /. tan(quad.leftAngle) : 
      remaningRoad /. tan(quad.rightAngle);
  } : 0.;
  
  if (leftBased == true) {
    let objX = int_of_float(xl -. objExtraX -. objectWidth +. objectOffsetX);
    (objX, objY, int_of_float(objectHeight), int_of_float(objectWidth))
  } else {
    let objX = int_of_float(xr +. objExtraX +. objectOffsetX);
    (objX, objY, int_of_float(objectHeight), int_of_float(objectWidth))
  }
};

let calculatePositions = (trackPiece: Track.plane, quad) => {
  let obsticles: list(Track.Obsticle.state) = trackPiece.obsticles;

  obsticles |> List.map{ (obs: Track.Obsticle.state) => {
    switch obs.objectType {
      | Track.Obsticle.SIGN_RIGHT => {
          let (x, y, height, width) = findPosition( quad, obs.offset, 96);
          let objectType = obs.objectType;
          { x, y, height, width, objectType }
        }
      | Track.Obsticle.SIGN_LEFT => {
          let (x, y, height, width) = findPosition(~leftBased=false, quad, obs.offset, 96);
          let objectType = obs.objectType;
          { x, y, height, width, objectType }
        }
    };
  }};
};

let draw = (objects, assets, env) =>
  objects |> List.iter(o => switch(o.objectType) {
    | SIGN_RIGHT => Draw.image(assets.roadSignRight, ~pos=(o.x, o.y), ~width=o.width, ~height=o.height, env);
    | SIGN_LEFT => Draw.image(assets.roadSignLeft, ~pos=(o.x, o.y), ~width=o.width, ~height=o.height, env);
  });
