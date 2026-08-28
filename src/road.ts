import * as Common from "./common";
import * as Object from "./object";
import {
  head,
  init as trackInit,
  isCheckpoint,
  progress as progressTrack,
  tail,
  type GroundSurface,
  type Plane,
  type RoadSurface,
  type TrackState,
} from "./track";

export type RoadState = {
  position: number;
  lastPiece: number;
  track: TrackState;
};

export const baseLength = 40;

export const currentPlane = (state: RoadState): Plane => head(state.track);

export const currentPlane2 = (state: RoadState): Plane => {
  const rest = tail(state.track);
  const plane = rest[0];
  if (!plane) {
    throw new Error("Track tail is empty");
  }
  return plane;
};

export const moveForward = (newPosition: number, state: RoadState): RoadState =>
  state.lastPiece * baseLength - newPosition <= 0
    ? {
        lastPiece: state.lastPiece + 1,
        position: newPosition,
        track: progressTrack(state.track),
      }
    : { ...state, position: newPosition };

export const checkpointBonus = (state: RoadState): number => {
  const direction = head(state.track).direction;
  return direction.tag === "Checkpoint" ? direction.duration : 0;
};

export const startTime = (state: RoadState): number => {
  const direction = head(state.track).direction;
  return direction.tag === "Start" ? direction.time : 0;
};

export const init: RoadState = { position: 0, track: trackInit, lastPiece: 1 };

const findInitialCoordinates = (state: RoadState) => {
  const adj = state.position % (baseLength * 2);
  const isLight = adj >= baseLength;
  const remainder = adj >= baseLength ? adj - baseLength : adj;
  return { remainder, isLight };
};

export type Colour = { r: number; g: number; b: number; a: number };

export type RoadDisplay = {
  x: number;
  y: number;
  z: number;
  previous: readonly [number, number, number];
  colour: Colour;
  terrainColour: Colour;
  objects: Object.ObjectDisplay[];
};

const red: Colour = { r: 150, g: 80, b: 80, a: 255 };
const lightGrey: Colour = { r: 78, g: 78, b: 78, a: 255 };
const darkGrey: Colour = { r: 70, g: 70, b: 70, a: 255 };
const roadLightGrey: Colour = { r: 62, g: 62, b: 62, a: 255 };
const roadDarkGrey: Colour = { r: 56, g: 56, b: 56, a: 255 };
const roadBrown: Colour = { r: 84, g: 66, b: 33, a: 255 };
const roadDarkBrown: Colour = { r: 70, g: 55, b: 30, a: 255 };
const lightGreen: Colour = { r: 45, g: 140, b: 30, a: 255 };
const darkGreen: Colour = { r: 30, g: 120, b: 30, a: 255 };
const lightBrown: Colour = { r: 82, g: 59, b: 32, a: 255 };
const darkBrown: Colour = { r: 70, g: 50, b: 30, a: 255 };
const lightBlue: Colour = { r: 45, g: 40, b: 140, a: 255 };
const darkBlue: Colour = { r: 36, g: 32, b: 130, a: 255 };

const terrainColourFor = (ground: GroundSurface, dark: boolean): Colour => {
  switch (ground) {
    case "Grass":
      return dark ? darkGreen : lightGreen;
    case "Soil":
      return dark ? darkBrown : lightBrown;
    case "Water":
      return dark ? darkBlue : lightBlue;
    case "Gravel":
      return dark ? darkGrey : lightGrey;
  }
};

const roadColourFor = (surface: RoadSurface, dark: boolean, checkpoint: boolean): Colour => {
  if (checkpoint) {
    return red;
  }
  if (surface === "Tarmac") {
    return dark ? roadLightGrey : roadDarkGrey;
  }
  return dark ? roadBrown : roadDarkBrown;
};

export const makeDisplay = (_offset: number, state: RoadState): RoadDisplay[] => {
  const { remainder: rem0, isLight } = findInitialCoordinates(state);
  const remainder = baseLength - rem0;
  const track = state.track.track.slice(0, Common.planes);
  let isDark = isLight;
  let previous: readonly [number, number, number] | undefined;
  let ddx = 0;
  return track.map((plane, i) => {
    const objects = plane.objects.map(Object.makeDisplay);
    let curve = 0;
    if (plane.direction.tag === "Left") {
      curve =
        i === 0
          ? (remainder / baseLength) * plane.direction.force * -2
          : plane.direction.force * -2;
    } else if (plane.direction.tag === "Right") {
      curve =
        i === 0 ? (remainder / baseLength) * plane.direction.force * 2 : plane.direction.force * 2;
    }
    curve += ddx;
    ddx = curve;
    const prev = previous ?? ([0, 50, 0] as const);
    const x = prev[0] + curve;
    const yFactor = 0.36 * plane.incline;
    const y = i === 0 ? prev[1] + yFactor * (remainder / baseLength) : prev[1] + yFactor;
    const z = i * baseLength + remainder;
    previous = [x, y, z];
    const dark = isDark;
    const result: RoadDisplay = {
      x,
      y,
      z,
      previous: prev,
      colour: roadColourFor(plane.roadSurface, dark, isCheckpoint(plane)),
      terrainColour: terrainColourFor(plane.groundSurface, dark),
      objects,
    };
    isDark = !isDark;
    return result;
  });
};
