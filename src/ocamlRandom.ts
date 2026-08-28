/**
 * OCaml / ReScript Random compatibility (lagged Fibonacci + MD5 seed).
 * Track decoration uses `Random.init(69)` from the original game.
 */

const addU32 = (x: number, y: number) => (x + y) >>> 0;
const rotl = (x: number, count: number) => (x << count) | (x >>> (32 - count));

function md5Bytes(bytes: Uint8Array): Uint8Array {
  const length = bytes.length;
  const bitLen = length * 8;
  const paddedLen = (((length + 8) >> 6) + 1) << 6;
  const buf = new Uint8Array(paddedLen);
  buf.set(bytes);
  buf[length] = 0x80;
  const view = new DataView(buf.buffer);
  view.setUint32(paddedLen - 8, bitLen >>> 0, true);
  view.setUint32(paddedLen - 4, Math.floor(bitLen / 0x100000000), true);

  let a = 0x67452301;
  let b = 0xefcdab89;
  let c = 0x98badcfe;
  let d = 0x10325476;

  const cmn = (q: number, a0: number, b0: number, x: number, s: number, t: number) =>
    addU32(rotl(addU32(addU32(a0, q), addU32(x, t)), s), b0);
  const ff = (a0: number, b0: number, c0: number, d0: number, x: number, s: number, t: number) =>
    cmn((b0 & c0) | (~b0 & d0), a0, b0, x, s, t);
  const gg = (a0: number, b0: number, c0: number, d0: number, x: number, s: number, t: number) =>
    cmn((b0 & d0) | (c0 & ~d0), a0, b0, x, s, t);
  const hh = (a0: number, b0: number, c0: number, d0: number, x: number, s: number, t: number) =>
    cmn(b0 ^ c0 ^ d0, a0, b0, x, s, t);
  const ii = (a0: number, b0: number, c0: number, d0: number, x: number, s: number, t: number) =>
    cmn(c0 ^ (b0 | ~d0), a0, b0, x, s, t);

  for (let i = 0; i < paddedLen; i += 64) {
    const x = new Int32Array(16);
    for (let j = 0; j < 16; j++) {
      x[j] = view.getInt32(i + j * 4, true);
    }
    let [aa, bb, cc, dd] = [a, b, c, d];
    aa = ff(aa, bb, cc, dd, x[0], 7, -680876936);
    dd = ff(dd, aa, bb, cc, x[1], 12, -389564586);
    cc = ff(cc, dd, aa, bb, x[2], 17, 606105819);
    bb = ff(bb, cc, dd, aa, x[3], 22, -1044525330);
    aa = ff(aa, bb, cc, dd, x[4], 7, -176418897);
    dd = ff(dd, aa, bb, cc, x[5], 12, 1200080426);
    cc = ff(cc, dd, aa, bb, x[6], 17, -1473231341);
    bb = ff(bb, cc, dd, aa, x[7], 22, -45705983);
    aa = ff(aa, bb, cc, dd, x[8], 7, 1770035416);
    dd = ff(dd, aa, bb, cc, x[9], 12, -1958414417);
    cc = ff(cc, dd, aa, bb, x[10], 17, -42063);
    bb = ff(bb, cc, dd, aa, x[11], 22, -1990404162);
    aa = ff(aa, bb, cc, dd, x[12], 7, 1804603682);
    dd = ff(dd, aa, bb, cc, x[13], 12, -40341101);
    cc = ff(cc, dd, aa, bb, x[14], 17, -1502002290);
    bb = ff(bb, cc, dd, aa, x[15], 22, 1236535329);
    aa = gg(aa, bb, cc, dd, x[1], 5, -165796510);
    dd = gg(dd, aa, bb, cc, x[6], 9, -1069501632);
    cc = gg(cc, dd, aa, bb, x[11], 14, 643717713);
    bb = gg(bb, cc, dd, aa, x[0], 20, -373897302);
    aa = gg(aa, bb, cc, dd, x[5], 5, -701558691);
    dd = gg(dd, aa, bb, cc, x[10], 9, 38016083);
    cc = gg(cc, dd, aa, bb, x[15], 14, -660478335);
    bb = gg(bb, cc, dd, aa, x[4], 20, -405537848);
    aa = gg(aa, bb, cc, dd, x[9], 5, 568446438);
    dd = gg(dd, aa, bb, cc, x[14], 9, -1019803690);
    cc = gg(cc, dd, aa, bb, x[3], 14, -187363961);
    bb = gg(bb, cc, dd, aa, x[8], 20, 1163531501);
    aa = gg(aa, bb, cc, dd, x[13], 5, -1444681467);
    dd = gg(dd, aa, bb, cc, x[2], 9, -51403784);
    cc = gg(cc, dd, aa, bb, x[7], 14, 1735328473);
    bb = gg(bb, cc, dd, aa, x[12], 20, -1926607734);
    aa = hh(aa, bb, cc, dd, x[5], 4, -378558);
    dd = hh(dd, aa, bb, cc, x[8], 11, -2022574463);
    cc = hh(cc, dd, aa, bb, x[11], 16, 1839030562);
    bb = hh(bb, cc, dd, aa, x[14], 23, -35309556);
    aa = hh(aa, bb, cc, dd, x[1], 4, -1530992060);
    dd = hh(dd, aa, bb, cc, x[4], 11, 1272893353);
    cc = hh(cc, dd, aa, bb, x[7], 16, -155497632);
    bb = hh(bb, cc, dd, aa, x[10], 23, -1094730640);
    aa = hh(aa, bb, cc, dd, x[13], 4, 681279174);
    dd = hh(dd, aa, bb, cc, x[0], 11, -358537222);
    cc = hh(cc, dd, aa, bb, x[3], 16, -722521979);
    bb = hh(bb, cc, dd, aa, x[6], 23, 76029189);
    aa = hh(aa, bb, cc, dd, x[9], 4, -640364487);
    dd = hh(dd, aa, bb, cc, x[12], 11, -421815835);
    cc = hh(cc, dd, aa, bb, x[15], 16, 530742520);
    bb = hh(bb, cc, dd, aa, x[2], 23, -995338651);
    aa = ii(aa, bb, cc, dd, x[0], 6, -198630844);
    dd = ii(dd, aa, bb, cc, x[7], 10, 1126891415);
    cc = ii(cc, dd, aa, bb, x[14], 15, -1416354905);
    bb = ii(bb, cc, dd, aa, x[5], 21, -57434055);
    aa = ii(aa, bb, cc, dd, x[12], 6, 1700485571);
    dd = ii(dd, aa, bb, cc, x[3], 10, -1894986606);
    cc = ii(cc, dd, aa, bb, x[10], 15, -1051523);
    bb = ii(bb, cc, dd, aa, x[1], 21, -2054922799);
    aa = ii(aa, bb, cc, dd, x[8], 6, 1873313359);
    dd = ii(dd, aa, bb, cc, x[15], 10, -30611744);
    cc = ii(cc, dd, aa, bb, x[6], 15, -1560198380);
    bb = ii(bb, cc, dd, aa, x[13], 21, 1309151649);
    aa = ii(aa, bb, cc, dd, x[4], 6, -145523070);
    dd = ii(dd, aa, bb, cc, x[11], 10, -1120210379);
    cc = ii(cc, dd, aa, bb, x[2], 15, 718787259);
    bb = ii(bb, cc, dd, aa, x[9], 21, -343485551);
    a = addU32(a, aa);
    b = addU32(b, bb);
    c = addU32(c, cc);
    d = addU32(d, dd);
  }

  const out = new Uint8Array(16);
  const outView = new DataView(out.buffer);
  outView.setUint32(0, a, true);
  outView.setUint32(4, b, true);
  outView.setUint32(8, c, true);
  outView.setUint32(12, d, true);
  return out;
}

function latin1Bytes(input: string): Uint8Array {
  const out = new Uint8Array(input.length);
  for (let i = 0; i < input.length; i++) {
    out[i] = input.charCodeAt(i) & 0xff;
  }
  return out;
}

function concatBytes(a: Uint8Array, b: Uint8Array): Uint8Array {
  const out = new Uint8Array(a.length + b.length);
  out.set(a, 0);
  out.set(b, a.length);
  return out;
}

function extractDigest(digest: Uint8Array): number {
  return (digest[0]! + (digest[1]! << 8) + (digest[2]! << 16) + (digest[3]! << 24)) | 0;
}

type RngState = {
  st: Int32Array;
  idx: number;
};

const MASK = 0x3fffffff;
const defaultState: RngState = {
  st: new Int32Array(55),
  idx: 0,
};

function fullInit(state: RngState, seed: number[]): void {
  const seeds = seed.length === 0 ? [0] : seed;
  for (let i = 0; i <= 54; i++) {
    state.st[i] = i;
  }
  let accu = latin1Bytes("x");
  const limit = 54 + Math.max(55, seeds.length);
  for (let i = 0; i <= limit; i++) {
    const j = i % 55;
    const k = i % seeds.length;
    accu = md5Bytes(concatBytes(accu, latin1Bytes(String(seeds[k]))));
    const extracted = extractDigest(accu);
    state.st[j] = (state.st[j]! ^ extracted) & MASK;
  }
  state.idx = 0;
}

function bits(state: RngState): number {
  state.idx = (state.idx + 1) % 55;
  const curval = state.st[state.idx]!;
  const mixed = curval ^ ((curval >>> 25) & 0x1f);
  const newval = (state.st[(state.idx + 24) % 55]! + mixed) | 0;
  const newval30 = newval & MASK;
  state.st[state.idx] = newval30;
  return newval30;
}

function intBound(state: RngState, bound: number): number {
  if (bound > MASK || bound <= 0) {
    throw new RangeError("Random.int");
  }
  for (;;) {
    const r = bits(state);
    const v = r % bound;
    if (r - v <= 1 + (MASK - bound)) {
      return v;
    }
  }
}

export const Random = {
  init(seed: number): void {
    fullInit(defaultState, [seed]);
  },
  int(bound: number): number {
    return intBound(defaultState, bound);
  },
};
