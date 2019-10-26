// Generated by BUCKLESCRIPT VERSION 4.0.7000, PLEASE EDIT WITH CARE
'use strict';

var Common = require("./common.js");
var Reprocessing_Draw = require("/Users/jim/Github/returbo/node_modules/reprocessing/src/Reprocessing_Draw.js");

function remainingTimeString(state) {
  return String(state[/* remainingTime */0] / Common.frameRate | 0);
}

function addTimeInSeconds(seconds, state) {
  var timeInFrames = seconds * Common.frameRate;
  var remainingTime = timeInFrames + state[/* remainingTime */0];
  return /* record */[/* remainingTime */remainingTime];
}

function reduce(state) {
  var match = 0 >= state[/* remainingTime */0];
  if (match) {
    return /* record */[/* remainingTime */0];
  } else {
    return /* record */[/* remainingTime */state[/* remainingTime */0] - 1];
  }
}

function gameOver(state) {
  return 0 >= state[/* remainingTime */0];
}

var init = /* record */[/* remainingTime */27 * Common.frameRate];

function draw(state, env) {
  var time = remainingTimeString(state);
  return Reprocessing_Draw.text(undefined, time, /* tuple */[
              Common.width / 2 | 0,
              20
            ], env);
}

exports.remainingTimeString = remainingTimeString;
exports.addTimeInSeconds = addTimeInSeconds;
exports.reduce = reduce;
exports.gameOver = gameOver;
exports.init = init;
exports.draw = draw;
/* Reprocessing_Draw Not a pure module */
