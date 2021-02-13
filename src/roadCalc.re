
let calcNextYPosition = (currentBottom, roadLength, firstHeight) => {
  let maximumY = Common.heightF;

  switch(currentBottom) {
    | x when x == maximumY => maximumY -. roadLength +. firstHeight
    | _ when currentBottom >= Common.heightF => currentBottom -. roadLength
    | _ => 
      let yDelta = 98. /. roadLength;  // (160 / 32) = 5
      let revY = 0. -. (currentBottom -. Common.heightF); // 288
      let height = roadLength -. revY /. yDelta;
      let height = height > 1. ? height : 1.;
      currentBottom -. height;
  }
};

let curveStength = fun
  | Track.Left(lc) => 0. -. lc
  | Track.Right(rc) => rc
  | _ => 0.0;

let nextGoals = (~goals, ~nextHeight, trackPiece: Track.plane) => {
   let height = Common.heightF;
    let curveStength = curveStength(trackPiece.direction);
    let (gl, gr) = goals;
    ( gl + int_of_float((height -. nextHeight) *. curveStength),
      gr + int_of_float((height -. nextHeight) *. curveStength)
    );
  };

type roadQuad = {
  leftBottom: (float, float),
  rightBottom: (float, float),
  leftTop: (float, float),
  rightTop: (float, float),
  leftAngle: float,
  rightAngle: float
};

let calcRoadQuad = (leftBottom, rightBottom, nextHeight, maxRoadHeight, nextGoalL, nextGoalR) => {
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
    leftBottom: leftBottom,
    rightBottom: rightBottom,
    leftTop: (leftX, nextHeight),
    rightTop: (rightX, nextHeight),
    // this is correct, I think there's something fishy going on above
    leftAngle: rightAngleRadians, 
    rightAngle: leftAngleRadians
  };
};
