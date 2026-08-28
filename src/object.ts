export type ObjectType = "SIGN_RIGHT" | "SIGN_LEFT" | "TREE" | "STONE" | "POST";

export type GameObject = {
  objectType: ObjectType;
  offset: number;
  z: number;
  size: readonly [number, number];
  hitbox: readonly [number, number];
};

export const calcHit = (
  playerPosition: number,
  distance: number,
  roadWidth: number,
  obj: GameObject,
): boolean => {
  const carSizeFactor = 55 / roadWidth;
  const playerOffset = playerPosition * -1;
  const playerOff1 = playerOffset - carSizeFactor;
  const playerOff2 = playerOffset + carSizeFactor;
  const adj = distance % (40 * 2);
  const playerTravel = adj >= 40 ? adj - 40 : adj;
  const { offset, z, hitbox } = obj;
  const [hbX, hbY0] = hitbox;
  const factor = hbX / roadWidth;
  const x1 = offset - factor / 2;
  const x2 = offset + factor / 2;
  const hbY = hbY0 * 0.5;
  const y1 = (z - hbY) * 0.5;
  const y2 = z;
  const inHbX = playerOff2 >= x1 && playerOff1 <= x2;
  const inHbY = playerTravel >= y1 && playerTravel <= y2;
  return inHbX && inHbY;
};

export const speedPenalty = (obj: GameObject): number => {
  switch (obj.objectType) {
    case "SIGN_RIGHT":
    case "SIGN_LEFT":
      return 14;
    case "TREE":
      return obj.size[0] > 100 ? 15 : 6;
    case "STONE":
      return 22;
    case "POST":
      return 9;
  }
};

export type ObjectDisplay = {
  offset: number;
  height: number;
  width: number;
  z: number;
  objectType: ObjectType;
};

export const makeDisplay = (obj: GameObject): ObjectDisplay => ({
  z: 0,
  height: obj.size[1],
  width: obj.size[0],
  offset: obj.offset,
  objectType: obj.objectType,
});

export const Prefabs = {
  makeSignRight: {
    objectType: "SIGN_RIGHT",
    offset: -1.25,
    z: 16,
    size: [96, 96],
    hitbox: [48, 4],
  } as GameObject,
  makeSignLeft: {
    objectType: "SIGN_LEFT",
    offset: 1.25,
    z: 16,
    size: [92, 92],
    hitbox: [48, 4],
  } as GameObject,
  makeTree: (offset: number): GameObject => ({
    objectType: "TREE",
    offset,
    z: 16,
    size: [128, 216],
    hitbox: [64, 4],
  }),
  smallTree: (offset: number): GameObject => ({
    objectType: "TREE",
    offset,
    z: 16,
    size: [64, 108],
    hitbox: [32, 3],
  }),
  makeStone: (offset: number): GameObject => ({
    objectType: "STONE",
    offset,
    z: 16,
    size: [64, 64],
    hitbox: [48, 4],
  }),
  makePost: (offset: number): GameObject => ({
    objectType: "POST",
    offset,
    z: 16,
    size: [12, 38],
    hitbox: [4, 4],
  }),
};
