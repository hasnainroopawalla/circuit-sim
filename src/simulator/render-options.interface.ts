export type IPosition = {
  x: number;
  y: number;
};

export type ISize = {
  w: number;
  h: number;
};

export type IChipRenderOptions = {
  position: IPosition;
  size: ISize;
  color: string;
  textColor: string;
  textPosition: IPosition;
};

export type IPinRenderOptions = {
  position: IPosition;
  size: number;
  color: string;
};

export type IORenderOptions = {
  position: IPosition;
  size: number;
};

export type ICircuitRenderOptions = {
  position: IPosition;
  size: ISize;
};

export type IButtonOptions = {
  position: IPosition;
  size: ISize;
  color: string;
};
