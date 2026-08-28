export type Color = {
  r: number;
  g: number;
  b: number;
  a: number;
};

export type Point = readonly [number, number];

export type KeyCode = "Left" | "Right" | "Up" | "Down" | "Space" | "Other";

export type ImageHandle = {
  readonly filename: string;
  readonly isPixel: boolean;
  texture: WebGLTexture | null;
  width: number;
  height: number;
  ready: boolean;
};

export type Env = {
  readonly canvas: HTMLCanvasElement;
  readonly gl: WebGLRenderingContext;
  width: number;
  height: number;
  fillColor: Color | null;
  mouse: [number, number];
  pmouse: [number, number];
  keyCode: KeyCode;
};
