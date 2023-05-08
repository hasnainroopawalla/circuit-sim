export interface Position {
  x: number;
  y: number;
}

export interface Size {
  w: number;
  h: number;
}

export interface ChipRenderOptions {
  position: Position;
  size: Size;
  color: number;
  textColor: number;
  textPosition: Position;
  textSize: number;
}

export interface PinRenderOptions {
  position: Position;
  size: number;
  color: number;
}
