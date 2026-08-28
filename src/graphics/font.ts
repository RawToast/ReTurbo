import type { ImageHandle } from "./types";
import { createTextureFromImage } from "./programs";
import type { Renderer } from "./renderer";

const GLYPH_SIZE = 16;
const COLS = 16;
const ROWS = 8;

export type FontAtlas = ImageHandle & {
  glyphWidth: number;
  glyphHeight: number;
};

export function createFontAtlas(renderer: Renderer): FontAtlas {
  const canvas = document.createElement("canvas");
  canvas.width = COLS * GLYPH_SIZE;
  canvas.height = ROWS * GLYPH_SIZE;
  const ctx = canvas.getContext("2d");
  if (!ctx) {
    throw new Error("Unable to create font atlas");
  }
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  ctx.fillStyle = "#ffffff";
  ctx.font = `bold ${GLYPH_SIZE - 2}px ui-monospace, "Cascadia Mono", "Liberation Mono", monospace`;
  ctx.textBaseline = "top";
  ctx.textAlign = "left";
  for (let code = 32; code < 128; code++) {
    const col = code % COLS;
    const row = Math.floor(code / COLS);
    ctx.fillText(String.fromCharCode(code), col * GLYPH_SIZE + 1, row * GLYPH_SIZE + 1);
  }
  const handle: FontAtlas = {
    filename: "<font>",
    isPixel: true,
    texture: createTextureFromImage(renderer.gl, canvas, true),
    width: canvas.width,
    height: canvas.height,
    ready: true,
    glyphWidth: GLYPH_SIZE,
    glyphHeight: GLYPH_SIZE,
  };
  return handle;
}

export function drawText(
  renderer: Renderer,
  font: FontAtlas,
  body: string,
  x: number,
  y: number,
): void {
  let cx = x;
  let cy = y;
  const gw = font.glyphWidth;
  const gh = font.glyphHeight;
  const texW = font.width;
  const texH = font.height;
  for (const char of body) {
    if (char === "\n") {
      cx = x;
      cy += gh;
      continue;
    }
    const code = char.charCodeAt(0);
    if (code < 32 || code >= 128) {
      cx += gw * 0.7;
      continue;
    }
    const col = code % COLS;
    const row = Math.floor(code / COLS);
    const u0 = (col * gw) / texW;
    const v0 = (row * gh) / texH;
    const u1 = ((col + 1) * gw) / texW;
    const v1 = ((row + 1) * gh) / texH;
    drawGlyph(renderer, font, cx, cy, gw, gh, u0, v0, u1, v1);
    cx += gw * 0.7;
  }
}

function drawGlyph(
  renderer: Renderer,
  font: FontAtlas,
  x: number,
  y: number,
  w: number,
  h: number,
  u0: number,
  v0: number,
  u1: number,
  v1: number,
): void {
  renderer.drawSubImage(font, x, y, w, h, u0, v0, u1, v1);
}
