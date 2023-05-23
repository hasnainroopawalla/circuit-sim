import { Position } from "../models/RenderOptions";

export const generateRandom = (start: number, end: number): number => {
  const diff = end - start;
  return Math.random() * diff + start;
};

export const initPosition = (): Position => {
  return {
    x: 0,
    y: 0,
  };
};
