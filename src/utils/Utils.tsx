import { IPosition } from "../simulator/render-options.interface";

export const generateRandom = (start: number, end: number): number => {
  const diff = end - start;
  return Math.random() * diff + start;
};

export const initPosition = (): IPosition => {
  return {
    x: 0,
    y: 0,
  };
};
