import { Position, Size } from "../../types";

export const pinPosition = (
  chipPosition: Position,
  chipSize: number,
  isInput: boolean
): Position => {
  const position = isInput
    ? chipPosition.x + chipSize
    : chipPosition.x - chipSize;
  return { x: position, y: chipPosition.y };
};

export const sliderPosition = (
  sliderPadding: number,
  chipPosition: Position,
  chipSize: number,
  windowWidth: number,
  isInput: boolean
): { position: Position; size: Size<"rect"> } =>
  isInput
    ? {
        position: { x: sliderPadding, y: chipPosition.y - chipSize / 2 },
        size: { w: chipSize / 3, h: chipSize },
      }
    : {
        position: {
          x: windowWidth - chipSize / 3 - sliderPadding,
          y: chipPosition.y - chipSize / 2,
        },
        size: { w: chipSize / 3, h: chipSize },
      };
