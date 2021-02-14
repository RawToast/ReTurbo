let calcNextYPosition = (positionY, length) => {
  let currentHeight = Common.heightF -. positionY; // 1 - 2xx
  let result = length *. (currentHeight /. 105.1); // larger divisor = bigger road
  let result = length -. result;
  let result = result > 0.5 ? result : 0.5;
  positionY -. result;
};

let calcNextYPosition = (positionY, length, firstHeight) =>
  positionY == Common.heightF ?
   Common.heightF -. length +. firstHeight :
   calcNextYPosition(positionY, length);

let curveStength =
  fun
  | Track.Left(lc) => 0. -. lc
  | Track.Right(rc) => rc
  | _ => 0.0;

let nextGoals = (~goals, ~nextHeight, trackPiece: Track.plane) => {
  let height = Common.heightF;
  let curveStength = curveStength(trackPiece.direction);
  let (gl, gr) = goals;
  (
    gl + int_of_float((height -. nextHeight) *. curveStength),
    gr + int_of_float((height -. nextHeight) *. curveStength),
  );
};

type roadQuad = {
  leftBottom: (float, float),
  rightBottom: (float, float),
  leftTop: (float, float),
  rightTop: (float, float),
  leftAngle: float,
  rightAngle: float,
};

let calcRoadQuad =
    (leftBottom, rightBottom, nextHeight, maxRoadHeight, nextGoalL, nextGoalR) => {
  let ((x0, top), (x1, _)) = (leftBottom, rightBottom);
  let opposite = top -. maxRoadHeight;
  let adjacentL = float_of_int(nextGoalL) -. x0;
  let adjacentR = x1 -. float_of_int(nextGoalR);
  let leftAngleRadians = atan(opposite /. adjacentL);
  let rightAngleRadians = atan(opposite /. adjacentR);

  let left = (top -. nextHeight) /. tan(leftAngleRadians);
  let right = (top -. nextHeight) /. tan(rightAngleRadians);
  let (rightX, leftX) = (x1 -. right, x0 +. left);
  {
    leftBottom,
    rightBottom,
    leftTop: (leftX, nextHeight),
    rightTop: (rightX, nextHeight),
    // this is correct, I think there's something fishy going on above
    leftAngle: rightAngleRadians,
    rightAngle: leftAngleRadians,
  };
};
