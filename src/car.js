// Generated by BUCKLESCRIPT VERSION 4.0.7000, PLEASE EDIT WITH CARE
'use strict';

var Common = require("./common.js");
var Caml_primitive = require("bsb-native/lib/js/caml_primitive.js");
var Reprocessing_Draw = require("/Users/jim/Github/returbo/node_modules/reprocessing/src/Reprocessing_Draw.js");

var brakeFactor = 60 * 1.6 / (Common.frameRate * 3);

function speedInMph(state) {
  return String(state[/* speed */1] / 1.6 | 0);
}

function draw(state, env) {
  var image = state[/* velocity */3] === 0 ? state[/* assets */5][/* straight */0] : (
      state[/* velocity */3] > 6 ? state[/* assets */5][/* heavyRightTurn */4] : (
          state[/* velocity */3] > 0 ? state[/* assets */5][/* rightTurn */3] : (
              state[/* velocity */3] < -6 ? state[/* assets */5][/* heavyLeftTurn */2] : (
                  state[/* velocity */3] < 0 ? state[/* assets */5][/* leftTurn */1] : state[/* assets */5][/* straight */0]
                )
            )
        )
    );
  return Reprocessing_Draw.image(image, state[/* position */0], 105, 51, env);
}

function updateOffset(state, force) {
  var offset = state[/* offset */4] - force;
  var offset$1 = offset > Common.minOffset ? offset : Common.minOffset;
  var offset$2 = offset$1 < Common.maxOffset ? offset$1 : Common.maxOffset;
  return /* record */[
          /* position */state[/* position */0],
          /* speed */state[/* speed */1],
          /* positionBonus */state[/* positionBonus */2],
          /* velocity */state[/* velocity */3],
          /* offset */offset$2,
          /* assets */state[/* assets */5]
        ];
}

function turn(key, state) {
  var highSpeed = state[/* speed */1] > 176 && state[/* speed */1] < 200;
  var vHighSpeed = state[/* speed */1] > 200;
  var updateVelocity = function (amount) {
    return /* record */[
            /* position */state[/* position */0],
            /* speed */state[/* speed */1],
            /* positionBonus */state[/* positionBonus */2],
            /* velocity */state[/* velocity */3] + amount | 0,
            /* offset */state[/* offset */4],
            /* assets */state[/* assets */5]
          ];
  };
  var tmp;
  var exit = 0;
  switch (key) {
    case 0 : 
        if (state[/* velocity */3] > -12 && vHighSpeed || state[/* velocity */3] > -13 && highSpeed || state[/* velocity */3] > -14) {
          tmp = updateVelocity(-1);
        } else {
          exit = 1;
        }
        break;
    case 1 : 
        if (state[/* velocity */3] > -16 && vHighSpeed || state[/* velocity */3] > -14) {
          tmp = updateVelocity(-2);
        } else {
          exit = 1;
        }
        break;
    case 2 : 
        if (state[/* velocity */3] < 12 && vHighSpeed || state[/* velocity */3] < 13 && highSpeed || state[/* velocity */3] < 14) {
          tmp = updateVelocity(1);
        } else {
          exit = 1;
        }
        break;
    case 3 : 
        if (state[/* velocity */3] < 16 && vHighSpeed) {
          tmp = updateVelocity(2);
        } else if (state[/* velocity */3] > 14) {
          tmp = updateVelocity(1);
        } else {
          exit = 1;
        }
        break;
    case 4 : 
        exit = 1;
        break;
    
  }
  if (exit === 1) {
    tmp = state[/* velocity */3] > 0 ? updateVelocity(-1) : (
        state[/* velocity */3] < 0 ? updateVelocity(1) : state
      );
  }
  var s = tmp;
  return updateOffset(s, s[/* velocity */3] / 2);
}

function progression(state) {
  var match = 0 >= state[/* speed */1];
  if (match) {
    return 0;
  } else {
    return state[/* speed */1] * (1 + state[/* positionBonus */2] / 100) / 25;
  }
}

function roadEffect(direction, state) {
  var tmp;
  if (typeof direction === "number") {
    tmp = state;
  } else {
    switch (direction.tag | 0) {
      case 0 : 
          tmp = updateOffset(state, direction[0] * 2.5 * 0.04 * state[/* speed */1]);
          break;
      case 1 : 
          tmp = updateOffset(state, direction[0] * 2.5 * -0.04 * state[/* speed */1]);
          break;
      case 2 : 
          tmp = state;
          break;
      
    }
  }
  var state$1 = tmp;
  var halfRoad = Common.roadWidth / 2;
  var carCentre = 105 / 2;
  var offset = state$1[/* offset */4];
  var isOffRight = offset > 0 && offset > halfRoad;
  var isOffLeft = offset < 0 && offset < halfRoad * -1 + carCentre;
  var isOff = isOffRight || isOffLeft;
  var reduce = function (state) {
    var match = state[/* speed */1] > 100;
    if (match) {
      return /* record */[
              /* position */state[/* position */0],
              /* speed */state[/* speed */1] - 0.45,
              /* positionBonus */state[/* positionBonus */2],
              /* velocity */state[/* velocity */3],
              /* offset */state[/* offset */4],
              /* assets */state[/* assets */5]
            ];
    } else {
      return /* record */[
              /* position */state[/* position */0],
              /* speed */state[/* speed */1] - 0.1,
              /* positionBonus */state[/* positionBonus */2],
              /* velocity */state[/* velocity */3],
              /* offset */state[/* offset */4],
              /* assets */state[/* assets */5]
            ];
    }
  };
  var state$2 = isOff ? reduce(state$1) : state$1;
  var initBonus;
  if (typeof direction === "number") {
    initBonus = 0;
  } else {
    switch (direction.tag | 0) {
      case 0 : 
          initBonus = direction[0] * offset / 22;
          break;
      case 1 : 
          initBonus = -direction[0] * offset / 22;
          break;
      case 2 : 
          initBonus = 0;
          break;
      
    }
  }
  var positionBonus = initBonus > 5 ? 5 : (
      initBonus < -5 ? -5 : (
          initBonus !== 0 ? initBonus : 0
        )
    );
  return /* record */[
          /* position */state$2[/* position */0],
          /* speed */state$2[/* speed */1],
          /* positionBonus */positionBonus,
          /* velocity */state$2[/* velocity */3],
          /* offset */state$2[/* offset */4],
          /* assets */state$2[/* assets */5]
        ];
}

function accelerate(isBrake, state) {
  var accel = 250 === state[/* speed */1] ? 250 : (
      90 > state[/* speed */1] ? Math.log((220 - state[/* speed */1]) / 6) / 10 : (
          110 > state[/* speed */1] ? Math.log((220 - state[/* speed */1]) / 8) / 15 : (
              160 > state[/* speed */1] ? Math.log((260 - state[/* speed */1]) / 10) / 25 : (
                  220 > state[/* speed */1] ? Math.log((250 - state[/* speed */1]) / 12) / 25 : (
                      260 > state[/* speed */1] ? Math.log((250 - state[/* speed */1]) / 14) / 25 : Math.log((250 - state[/* speed */1]) / 16) / 25
                    )
                )
            )
        )
    );
  var speed = isBrake ? Caml_primitive.caml_float_max(0, state[/* speed */1] - brakeFactor) : state[/* speed */1] + accel;
  var speed$1 = 0 > speed ? 0 : speed;
  var speed$2 = 250 < speed$1 ? 250 : speed$1;
  return /* record */[
          /* position */state[/* position */0],
          /* speed */speed$2,
          /* positionBonus */state[/* positionBonus */2],
          /* velocity */state[/* velocity */3],
          /* offset */state[/* offset */4],
          /* assets */state[/* assets */5]
        ];
}

function init(x, y, env) {
  return /* record */[
          /* position : tuple */[
            x,
            y
          ],
          /* speed */0,
          /* positionBonus */0,
          /* velocity */0,
          /* offset */0,
          /* assets : record */[
            /* straight */Reprocessing_Draw.loadImage("assets/car_1.png", true, env),
            /* leftTurn */Reprocessing_Draw.loadImage("assets/car_2.png", true, env),
            /* heavyLeftTurn */Reprocessing_Draw.loadImage("assets/car_3.png", true, env),
            /* rightTurn */Reprocessing_Draw.loadImage("assets/car_4.png", true, env),
            /* heavyRightTurn */Reprocessing_Draw.loadImage("assets/car_5.png", true, env)
          ]
        ];
}

var Draw = 0;

var carWidth = 105;

var vLowSpeed = 90;

var lowSpeed = 110;

var midSpeed = 160;

var highSpeed = 220;

var vHighSpeed = 260;

var grassMaxSpeed = 100;

var maxSpeed = 250;

exports.Draw = Draw;
exports.carWidth = carWidth;
exports.vLowSpeed = vLowSpeed;
exports.lowSpeed = lowSpeed;
exports.midSpeed = midSpeed;
exports.highSpeed = highSpeed;
exports.vHighSpeed = vHighSpeed;
exports.grassMaxSpeed = grassMaxSpeed;
exports.maxSpeed = maxSpeed;
exports.brakeFactor = brakeFactor;
exports.speedInMph = speedInMph;
exports.draw = draw;
exports.updateOffset = updateOffset;
exports.turn = turn;
exports.progression = progression;
exports.roadEffect = roadEffect;
exports.accelerate = accelerate;
exports.init = init;
/* Reprocessing_Draw Not a pure module */
