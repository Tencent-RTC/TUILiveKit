// Type shim for colorthief's prebuilt ESM bundle (dist/color-thief.mjs).
// The published package's "module" entry is empty and its bundled .d.ts
// describes a mismatched v3 async API, so we declare the real v2 surface
// (synchronous getPalette returning RGB triples) used at runtime.
declare module 'colorthief/dist/color-thief.mjs' {
  export default class ColorThief {
    getColor(img: HTMLImageElement, quality?: number): [number, number, number];
    getPalette(
      img: HTMLImageElement,
      colorCount?: number,
      quality?: number,
    ): [number, number, number][];
  }
}
