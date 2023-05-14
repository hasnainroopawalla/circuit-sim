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
  color: string;
  textColor: string;
  textPosition: Position;
  textSize: number;
}

export interface PinRenderOptions {
  position: Position;
  size: number;
  color: string;
}

export interface IORenderOptions {
  position: Position;
  size: number;
}
