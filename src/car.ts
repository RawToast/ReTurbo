import * as Common from "./common";
import type { Turn } from "./control";
import type { Direction } from "./track";

export type CarState = {
  speed: number;
  positionBonus: number;
  velocity: number;
  offset: number;
};

export const carWidth = 110;
export const carHeight = 55;

export type CarAsset = "Straight" | "LeftTurn" | "HeavyLeftTurn" | "RightTurn" | "HeavyRightTurn";

export type CarDisplay = {
  offset: number;
  width: number;
  height: number;
  asset: CarAsset;
  z: number;
};

export const makeDisplay = (car: CarState): CarDisplay => {
  let asset: CarAsset = "Straight";
  if (car.velocity === 0) {
    asset = "Straight";
  } else if (car.velocity > 12) {
    asset = "HeavyRightTurn";
  } else if (car.velocity > 0.2) {
    asset = "RightTurn";
  } else if (car.velocity < -12) {
    asset = "HeavyLeftTurn";
  } else if (car.velocity < 0.2) {
    asset = "LeftTurn";
  }
  return {
    offset: car.offset,
    asset,
    width: carWidth,
    height: carHeight,
    z: 1,
  };
};

export const vLowSpeed = 90;
export const lowSpeed = 110;
export const midSpeed = 160;
export const highSpeed = 220;
export const vHighSpeed = 260;
export const grassMaxSpeed = 100;
export const maxSpeed = 250;
export const brakeFactor = (60 * 1.6) / (Common.frameRate * 3);

export const speedInMph = (state: CarState): string => String(Math.trunc(state.speed / 1.6));

const updateOffset = (state: CarState, force: number): CarState => {
  let offset = state.offset - force;
  offset = Math.max(offset, Common.minOffset);
  offset = Math.min(offset, Common.maxOffset);
  return { ...state, offset };
};

export const turn = (key: Turn, state: CarState): CarState => {
  const updateOffsetUsingForce = (s: CarState) => updateOffset(s, s.velocity / 700);
  const high = state.speed > 176 && state.speed < 200;
  const vHigh = state.speed > 200;
  const updateVelocity = (amount: number): CarState => {
    const velocity = state.velocity + amount;
    return {
      ...state,
      velocity: velocity >= -0.5 && velocity <= 0.5 ? 0 : velocity,
    };
  };
  let next: CarState = state;
  if (key === "LEFT" && state.velocity > -12 && vHigh) {
    next = updateVelocity(-0.8);
  } else if (key === "LEFT" && state.velocity > -13 && high) {
    next = updateVelocity(-0.9);
  } else if (key === "LEFT" && state.velocity > -14) {
    next = updateVelocity(-1);
  } else if (key === "P_LEFT" && state.velocity > -18 && vHigh) {
    next = updateVelocity(-2);
  } else if (key === "P_LEFT" && state.velocity > -16) {
    next = updateVelocity(-3.5);
  } else if (key === "RIGHT" && state.velocity < 12 && vHigh) {
    next = updateVelocity(0.8);
  } else if (key === "RIGHT" && state.velocity < 13 && high) {
    next = updateVelocity(0.9);
  } else if (key === "RIGHT" && state.velocity < 14) {
    next = updateVelocity(1);
  } else if (key === "P_RIGHT" && state.velocity < 18 && vHigh) {
    next = updateVelocity(2);
  } else if (key === "P_RIGHT" && state.velocity < 16) {
    next = updateVelocity(3.5);
  } else if (state.velocity > 0) {
    next = updateVelocity(-1);
  } else if (state.velocity < 0) {
    next = updateVelocity(1);
  }
  return updateOffsetUsingForce(next);
};

export const progression = (state: CarState): number =>
  0 >= state.speed ? 0 : (state.speed * (1 + state.positionBonus / 100)) / 25;

export const roadEffect = (direction: Direction, incline: number, state: CarState): CarState => {
  const offTrack = (current: CarState): CarState => {
    const carCentre = carWidth / 2.1;
    const offset = current.offset;
    const isOffLeft = (offset > 0 && offset > 1) || (offset < 0 && offset < -1);
    const isOffRight =
      (offset < 0 && offset < -1 + carCentre) || (offset > 0 && offset > 1 + carCentre);
    let offRoadFactor = 0;
    if (isOffLeft && isOffRight) {
      offRoadFactor = 1;
    } else if (!isOffLeft && !isOffRight) {
      offRoadFactor = 0.3;
    }
    const isOff = isOffRight || isOffLeft;
    if (!isOff) {
      return current;
    }
    if (current.speed > grassMaxSpeed) {
      return { ...current, speed: current.speed - offRoadFactor * 0.8 };
    }
    return { ...current, speed: current.speed - offRoadFactor * 0.1 };
  };

  const apexBonus = (current: CarState): CarState => {
    const offset = current.offset;
    let initBonus = 0;
    if (direction.tag === "Right") {
      initBonus = ((0 - direction.force) * offset) / 22;
    } else if (direction.tag === "Left") {
      initBonus = (direction.force * offset) / 22;
    }
    let positionBonus = initBonus;
    if (initBonus > 5) {
      positionBonus = 5;
    } else if (initBonus < -5) {
      positionBonus = -5;
    } else if (initBonus === 0) {
      positionBonus = 0;
    }
    return { ...current, positionBonus };
  };

  const cornerEffect = (current: CarState): CarState => {
    if (direction.tag === "Left") {
      return updateOffset(current, (direction.force * 0.1 * current.speed) / 350);
    }
    if (direction.tag === "Right") {
      return updateOffset(current, (direction.force * -0.1 * current.speed) / 350);
    }
    return current;
  };

  const hillEffect = (current: CarState): CarState => {
    const hill = incline > 0 ? incline * 0.5 : incline;
    const effect = hill * 0.02;
    return effect !== 0 ? { ...current, speed: current.speed - effect } : current;
  };

  return hillEffect(apexBonus(offTrack(cornerEffect(state))));
};

export const accelerate = (isBraking: boolean, state: CarState): CarState => {
  let accel: number;
  if (maxSpeed === state.speed) {
    accel = maxSpeed;
  } else if (vLowSpeed > state.speed) {
    accel = Math.log((highSpeed - state.speed) / 4) / 8;
  } else if (lowSpeed > state.speed) {
    accel = Math.log((highSpeed - state.speed) / 8) / 12;
  } else if (midSpeed > state.speed) {
    accel = Math.log((vHighSpeed - state.speed) / 10) / 20;
  } else if (highSpeed > state.speed) {
    accel = Math.log((maxSpeed - state.speed) / 12) / 22;
  } else if (vHighSpeed > state.speed) {
    accel = Math.log((maxSpeed - state.speed) / 14) / 25;
  } else {
    accel = Math.log((maxSpeed - state.speed) / 16) / 25;
  }

  let speed = isBraking ? Math.max(0, state.speed - brakeFactor) : state.speed + accel;
  speed = Math.max(0, speed);
  speed = Math.min(maxSpeed, speed);
  return { ...state, speed };
};

export const init: CarState = {
  velocity: 0,
  offset: 0,
  speed: 0,
  positionBonus: 0,
};
