function compileShader(gl: WebGLRenderingContext, type: number, source: string): WebGLShader {
  const shader = gl.createShader(type);
  if (!shader) {
    throw new Error("Unable to create shader");
  }
  gl.shaderSource(shader, source);
  gl.compileShader(shader);
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    const info = gl.getShaderInfoLog(shader) ?? "unknown";
    gl.deleteShader(shader);
    throw new Error(`Shader compile failed: ${info}`);
  }
  return shader;
}

function createProgram(gl: WebGLRenderingContext, vert: string, frag: string): WebGLProgram {
  const program = gl.createProgram();
  if (!program) {
    throw new Error("Unable to create program");
  }
  const vs = compileShader(gl, gl.VERTEX_SHADER, vert);
  const fs = compileShader(gl, gl.FRAGMENT_SHADER, frag);
  gl.attachShader(program, vs);
  gl.attachShader(program, fs);
  gl.linkProgram(program);
  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
    const info = gl.getProgramInfoLog(program) ?? "unknown";
    throw new Error(`Program link failed: ${info}`);
  }
  return program;
}

const COLOR_VERT = `
attribute vec2 a_position;
attribute vec4 a_color;
uniform vec2 u_resolution;
varying vec4 v_color;
void main() {
  vec2 clip = (a_position / u_resolution) * 2.0 - 1.0;
  gl_Position = vec4(clip.x, -clip.y, 0.0, 1.0);
  v_color = a_color;
}
`;

const COLOR_FRAG = `
precision mediump float;
varying vec4 v_color;
void main() {
  gl_FragColor = v_color;
}
`;

const TEXTURE_VERT = `
attribute vec2 a_position;
attribute vec2 a_texCoord;
uniform vec2 u_resolution;
varying vec2 v_texCoord;
void main() {
  vec2 clip = (a_position / u_resolution) * 2.0 - 1.0;
  gl_Position = vec4(clip.x, -clip.y, 0.0, 1.0);
  v_texCoord = a_texCoord;
}
`;

const TEXTURE_FRAG = `
precision mediump float;
uniform sampler2D u_image;
varying vec2 v_texCoord;
void main() {
  gl_FragColor = texture2D(u_image, v_texCoord);
}
`;

export type ColorProgram = {
  program: WebGLProgram;
  aPosition: number;
  aColor: number;
  uResolution: WebGLUniformLocation;
};

export type TextureProgram = {
  program: WebGLProgram;
  aPosition: number;
  aTexCoord: number;
  uResolution: WebGLUniformLocation;
  uImage: WebGLUniformLocation;
};

export function createColorProgram(gl: WebGLRenderingContext): ColorProgram {
  const program = createProgram(gl, COLOR_VERT, COLOR_FRAG);
  const uResolution = gl.getUniformLocation(program, "u_resolution");
  if (!uResolution) {
    throw new Error("Missing u_resolution");
  }
  return {
    program,
    aPosition: gl.getAttribLocation(program, "a_position"),
    aColor: gl.getAttribLocation(program, "a_color"),
    uResolution,
  };
}

export function createTextureProgram(gl: WebGLRenderingContext): TextureProgram {
  const program = createProgram(gl, TEXTURE_VERT, TEXTURE_FRAG);
  const uResolution = gl.getUniformLocation(program, "u_resolution");
  const uImage = gl.getUniformLocation(program, "u_image");
  if (!uResolution || !uImage) {
    throw new Error("Missing texture uniforms");
  }
  return {
    program,
    aPosition: gl.getAttribLocation(program, "a_position"),
    aTexCoord: gl.getAttribLocation(program, "a_texCoord"),
    uResolution,
    uImage,
  };
}

export function createTextureFromImage(
  gl: WebGLRenderingContext,
  source: TexImageSource,
  isPixel: boolean,
): WebGLTexture {
  const texture = gl.createTexture();
  if (!texture) {
    throw new Error("Unable to create texture");
  }
  gl.bindTexture(gl.TEXTURE_2D, texture);
  gl.pixelStorei(gl.UNPACK_PREMULTIPLY_ALPHA_WEBGL, 1);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
  const filter = isPixel ? gl.NEAREST : gl.LINEAR;
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, filter);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, filter);
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, source);
  gl.bindTexture(gl.TEXTURE_2D, null);
  return texture;
}
