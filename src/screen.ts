import * as Common from "./common";
import type { CarAsset, CarDisplay } from "./car";
import type { Env, ImageHandle } from "./graphics/index";
import { Draw, Utils } from "./graphics/index";
import type { ObjectType } from "./object";
import type { Colour, RoadDisplay } from "./road";

export type ScreenState = {
  car: CarDisplay;
  road: RoadDisplay[];
};

const cameraHeight = 55;
const cameraDepth = Common.cameraDepth;

const projectToScreen = (
  offset: number,
  x: number,
  y: number,
  z: number,
): readonly [number, number, number, number] => {
  const iOffset = (Common.roadWidth / 2) * offset * 0.04;
  const cameraX = x + iOffset;
  const cameraY = y - cameraHeight;
  const cameraZ = z === 0 ? 1 : z;
  const scale0 = cameraDepth / cameraZ;
  const screenX = Common.centrePoint + scale0 * cameraX * Common.centrePoint;
  const screenY = Common.centreHeight - scale0 * cameraY * Common.centreHeight;
  const roadWidth = scale0 * Common.roadWidth * (Common.widthF / 100);
  const scale = scale0 * 8;
  return [screenX, screenY, roadWidth, scale];
};

type CarAssets = {
  straight: ImageHandle;
  leftTurn: ImageHandle;
  heavyLeftTurn: ImageHandle;
  rightTurn: ImageHandle;
  heavyRightTurn: ImageHandle;
};

export type Assets = {
  roadSignRight: ImageHandle;
  roadSignLeft: ImageHandle;
  tree: ImageHandle;
  stone: ImageHandle;
  post: ImageHandle;
  car: CarAssets;
};

type SpriteKind =
  | { tag: "SIGN_RIGHT" }
  | { tag: "SIGN_LEFT" }
  | { tag: "TREE" }
  | { tag: "STONE" }
  | { tag: "POST" }
  | { tag: "CAR"; state: CarAsset };

export type Sprite = {
  x: number;
  y: number;
  height: number;
  width: number;
  objectType: SpriteKind;
};

const fromObject = (o: ObjectType): SpriteKind => {
  switch (o) {
    case "SIGN_RIGHT":
      return { tag: "SIGN_RIGHT" };
    case "SIGN_LEFT":
      return { tag: "SIGN_LEFT" };
    case "TREE":
      return { tag: "TREE" };
    case "STONE":
      return { tag: "STONE" };
    case "POST":
      return { tag: "POST" };
  }
};

const fromCar = (c: CarAsset): SpriteKind => ({ tag: "CAR", state: c });

export const loadAssets = (env: Env): Assets => {
  const loadImage = (file: string) => Draw.loadImage({ filename: file, isPixel: true }, env);
  return {
    roadSignLeft: loadImage("assets/roadsign_left.png"),
    roadSignRight: loadImage("assets/roadsign.png"),
    tree: loadImage("assets/tree.png"),
    stone: loadImage("assets/stone.png"),
    post: loadImage("assets/post.png"),
    car: {
      straight: loadImage("assets/car_1.png"),
      leftTurn: loadImage("assets/car_2.png"),
      heavyLeftTurn: loadImage("assets/car_3.png"),
      rightTurn: loadImage("assets/car_4.png"),
      heavyRightTurn: loadImage("assets/car_5.png"),
    },
  };
};

const drawSprite = (sprite: Sprite, assets: Assets, env: Env): void => {
  const pos: readonly [number, number] = [sprite.x - sprite.width / 2, sprite.y];
  const draw = (image: ImageHandle) =>
    Draw.imagef(image, { pos, width: sprite.width, height: sprite.height }, env);
  switch (sprite.objectType.tag) {
    case "SIGN_RIGHT":
      draw(assets.roadSignRight);
      return;
    case "SIGN_LEFT":
      draw(assets.roadSignLeft);
      return;
    case "TREE":
      draw(assets.tree);
      return;
    case "STONE":
      draw(assets.stone);
      return;
    case "POST":
      draw(assets.post);
      return;
    case "CAR":
      switch (sprite.objectType.state) {
        case "Straight":
          draw(assets.car.straight);
          return;
        case "LeftTurn":
          draw(assets.car.leftTurn);
          return;
        case "HeavyLeftTurn":
          draw(assets.car.heavyLeftTurn);
          return;
        case "RightTurn":
          draw(assets.car.rightTurn);
          return;
        case "HeavyRightTurn":
          draw(assets.car.heavyRightTurn);
          return;
      }
  }
};

type Quad = {
  x: number;
  y: number;
  w: number;
  previous: readonly [number, number, number];
  colour: Colour;
  terrainColour: Colour;
  objects: Sprite[];
};

const makeQuad = (offset: number, road: RoadDisplay): Quad => {
  const [x, y, w, scale] = projectToScreen(offset, road.x, road.y, road.z);
  const [px0, py0, pz0] = road.previous;
  const [px, py, pw] = projectToScreen(offset, px0, py0, pz0);
  const ox = x - (x - px) / 2;
  const oy = y - (y - py) / 2;
  const ow = w - (w - pw) / 2;
  const objects = road.objects.map((o) => ({
    x: ox + ow * o.offset,
    y: oy - o.height * scale,
    height: o.height * scale,
    width: o.width * scale,
    objectType: fromObject(o.objectType),
  }));
  return {
    x,
    y,
    w,
    previous: [px, py, pw],
    colour: road.colour,
    terrainColour: road.terrainColour,
    objects,
  };
};

const drawQuad = (quad: Quad, assets: Assets, env: Env): Sprite[] => {
  const { x, y, w, previous, colour, terrainColour } = quad;
  const [px, py, pw] = previous;
  if (Math.abs(py) > Math.abs(y)) {
    Draw.fill(Utils.color(terrainColour), env);
    Draw.quadf(
      {
        p1: [0, py],
        p2: [Common.widthF, py],
        p3: [Common.widthF, y],
        p4: [0, y],
      },
      env,
    );
    Draw.fill(Utils.color(colour), env);
    Draw.quadf(
      {
        p1: [px - pw, py],
        p2: [px + pw, py],
        p3: [x + w, y],
        p4: [x - w, y],
      },
      env,
    );
    const infront: Sprite[] = [];
    const behind: Sprite[] = [];
    for (const sprite of quad.objects) {
      if (sprite.y < 276) {
        infront.push(sprite);
      } else {
        behind.push(sprite);
      }
    }
    for (const sprite of infront) {
      drawSprite(sprite, assets, env);
    }
    return behind;
  }
  return [];
};

const makeCar = (car: CarDisplay): Sprite => ({
  x: Common.centrePoint,
  y: Common.heightF - car.height + 1,
  height: car.height,
  width: car.width,
  objectType: fromCar(car.asset),
});

export const draw = (offset: number, screen: ScreenState, assets: Assets, env: Env): void => {
  const projectedRoad = screen.road
    .slice(0, Common.planes)
    .map((segment) => makeQuad(offset, segment));
  const projectedCar = makeCar(screen.car);
  const behind: Sprite[] = [];
  const ordered = projectedRoad.toReversed();
  for (const quad of ordered) {
    behind.push(...drawQuad(quad, assets, env));
  }
  drawSprite(projectedCar, assets, env);
  for (const sprite of behind) {
    drawSprite(sprite, assets, env);
  }
};
