
let calcNextYPosition = (currentBottom, maximumY, baseRoadLength, firstHeight) => {
  if (currentBottom == maximumY) {
    maximumY -. baseRoadLength +. firstHeight;
  } else {
    // calc next Y
    if (currentBottom >= 320.) {
      currentBottom -. baseRoadLength;
    } else {
      let yDelta = 160. /. baseRoadLength;  // (160 / 40) = 4
      let revY = 0. -. (currentBottom -. 320.);
      let height = baseRoadLength -. revY /. yDelta; // (40 - ( ? / 4))
      let delta = height > 0.1 ? height : 0.1;
      currentBottom -. delta;
    };
  };
};

let curveStength = direction =>
  switch (direction) {
  | Track.Left(lc) => 0. -. lc
  | Track.Right(rc) => rc
  | _ => 0.0
};


let nextGoals = (goals, curveStength, height, nextHeight) => {
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
    leftAngle: leftAngleRadians,
    rightAngle: rightAngleRadians
  };
};