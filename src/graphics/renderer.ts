import type { Color, ImageHandle, Point } from "./types";
import {
  createColorProgram,
  createTextureFromImage,
  createTextureProgram,
  type ColorProgram,
  type TextureProgram,
} from "./programs";

const COLOR_STRIDE = 6;
const TEX_STRIDE = 4;

export class Renderer {
  readonly gl: WebGLRenderingContext;
  readonly canvas: HTMLCanvasElement;
  private readonly colorProg: ColorProgram;
  private readonly texProg: TextureProgram;
  private readonly colorBuf: WebGLBuffer;
  private readonly texBuf: WebGLBuffer;
  private colorData: number[] = [];
  private texData: number[] = [];
  private boundTexture: WebGLTexture | null = null;
  width = 568;
  height = 320;

  constructor(canvas: HTMLCanvasElement) {
    const gl = canvas.getContext("webgl", {
      alpha: false,
      antialias: true,
      premultipliedAlpha: true,
      preserveDrawingBuffer: false,
    });
    if (!gl) {
      throw new Error("WebGL is not available");
    }
    this.canvas = canvas;
    this.gl = gl;
    this.colorProg = createColorProgram(gl);
    this.texProg = createTextureProgram(gl);
    const colorBuf = gl.createBuffer();
    const texBuf = gl.createBuffer();
    if (!colorBuf || !texBuf) {
      throw new Error("Unable to create buffers");
    }
    this.colorBuf = colorBuf;
    this.texBuf = texBuf;
    gl.disable(gl.DEPTH_TEST);
    gl.enable(gl.BLEND);
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
  }

  resize(width: number, height: number): void {
    this.width = width;
    this.height = height;
    const dpr = window.devicePixelRatio || 1;
    this.canvas.width = Math.max(1, Math.floor(width * dpr));
    this.canvas.height = Math.max(1, Math.floor(height * dpr));
    this.canvas.style.width = `${width}px`;
    this.canvas.style.height = `${height}px`;
    this.gl.viewport(0, 0, this.canvas.width, this.canvas.height);
  }

  beginFrame(clear: Color): void {
    const { gl } = this;
    gl.viewport(0, 0, this.canvas.width, this.canvas.height);
    gl.clearColor(clear.r / 255, clear.g / 255, clear.b / 255, clear.a / 255);
    gl.clear(gl.COLOR_BUFFER_BIT);
    this.colorData = [];
    this.texData = [];
    this.boundTexture = null;
  }

  fillQuad(p1: Point, p2: Point, p3: Point, p4: Point, color: Color): void {
    this.flushTextures();
    const r = color.r / 255;
    const g = color.g / 255;
    const b = color.b / 255;
    const a = color.a / 255;
    this.pushColorVert(p1[0], p1[1], r, g, b, a);
    this.pushColorVert(p2[0], p2[1], r, g, b, a);
    this.pushColorVert(p3[0], p3[1], r, g, b, a);
    this.pushColorVert(p1[0], p1[1], r, g, b, a);
    this.pushColorVert(p3[0], p3[1], r, g, b, a);
    this.pushColorVert(p4[0], p4[1], r, g, b, a);
  }

  drawImage(image: ImageHandle, x: number, y: number, width: number, height: number): void {
    this.drawSubImage(image, x, y, width, height, 0, 0, 1, 1);
  }

  drawSubImage(
    image: ImageHandle,
    x: number,
    y: number,
    width: number,
    height: number,
    u0: number,
    v0: number,
    u1: number,
    v1: number,
  ): void {
    if (!image.ready || image.texture === null) {
      return;
    }
    this.flushColors();
    if (this.boundTexture !== null && this.boundTexture !== image.texture) {
      this.flushTextures();
    }
    this.boundTexture = image.texture;
    this.pushTexVert(x, y, u0, v0);
    this.pushTexVert(x + width, y, u1, v0);
    this.pushTexVert(x + width, y + height, u1, v1);
    this.pushTexVert(x, y, u0, v0);
    this.pushTexVert(x + width, y + height, u1, v1);
    this.pushTexVert(x, y + height, u0, v1);
  }

  endFrame(): void {
    this.flushColors();
    this.flushTextures();
  }

  uploadImage(
    image: ImageHandle,
    source: TexImageSource,
    pixelWidth: number,
    pixelHeight: number,
  ): void {
    image.texture = createTextureFromImage(this.gl, source, image.isPixel);
    image.width = pixelWidth;
    image.height = pixelHeight;
    image.ready = true;
  }

  private pushColorVert(x: number, y: number, r: number, g: number, b: number, a: number): void {
    this.colorData.push(x, y, r, g, b, a);
  }

  private pushTexVert(x: number, y: number, u: number, v: number): void {
    this.texData.push(x, y, u, v);
  }

  private flushColors(): void {
    if (this.colorData.length === 0) {
      return;
    }
    const { gl } = this;
    const data = new Float32Array(this.colorData);
    gl.useProgram(this.colorProg.program);
    gl.bindBuffer(gl.ARRAY_BUFFER, this.colorBuf);
    gl.bufferData(gl.ARRAY_BUFFER, data, gl.DYNAMIC_DRAW);
    gl.uniform2f(this.colorProg.uResolution, this.width, this.height);
    gl.enableVertexAttribArray(this.colorProg.aPosition);
    gl.vertexAttribPointer(this.colorProg.aPosition, 2, gl.FLOAT, false, COLOR_STRIDE * 4, 0);
    gl.enableVertexAttribArray(this.colorProg.aColor);
    gl.vertexAttribPointer(this.colorProg.aColor, 4, gl.FLOAT, false, COLOR_STRIDE * 4, 8);
    gl.drawArrays(gl.TRIANGLES, 0, this.colorData.length / COLOR_STRIDE);
    this.colorData = [];
  }

  private flushTextures(): void {
    if (this.texData.length === 0 || this.boundTexture === null) {
      this.texData = [];
      return;
    }
    const { gl } = this;
    const data = new Float32Array(this.texData);
    gl.useProgram(this.texProg.program);
    gl.bindBuffer(gl.ARRAY_BUFFER, this.texBuf);
    gl.bufferData(gl.ARRAY_BUFFER, data, gl.DYNAMIC_DRAW);
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, this.boundTexture);
    gl.uniform1i(this.texProg.uImage, 0);
    gl.uniform2f(this.texProg.uResolution, this.width, this.height);
    gl.enableVertexAttribArray(this.texProg.aPosition);
    gl.vertexAttribPointer(this.texProg.aPosition, 2, gl.FLOAT, false, TEX_STRIDE * 4, 0);
    gl.enableVertexAttribArray(this.texProg.aTexCoord);
    gl.vertexAttribPointer(this.texProg.aTexCoord, 2, gl.FLOAT, false, TEX_STRIDE * 4, 8);
    gl.drawArrays(gl.TRIANGLES, 0, this.texData.length / TEX_STRIDE);
    this.texData = [];
    this.boundTexture = null;
  }
}
