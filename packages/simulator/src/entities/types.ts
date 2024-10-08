export type Position = {
  x: number;
  y: number;
};

export type RectSize = {
  w: number;
  h: number;
};

export type CircleSize = { d: number };

export type Size<T extends "rect" | "circle"> = T extends "rect"
  ? RectSize
  : CircleSize;
