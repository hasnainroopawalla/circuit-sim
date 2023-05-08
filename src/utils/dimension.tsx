import { Position, Size } from "../models/renderOptions";

export const computeChipTextPosition = (
  chipPosition: Position,
  chipSize: Size
): Position => {
  return {
    x: chipPosition.x + chipSize.w / 2,
    y: chipPosition.y + chipSize.h / 2,
  };
};

export const computeInputPinsPosition = (
  lineStartPosition: Position,
  lineEndPosition: Position,
  numPins: number
) => {
  if (numPins === 1) {
    return [
      {
        x: (lineStartPosition.x + lineEndPosition.x) / 2,
        y: (lineStartPosition.y + lineEndPosition.y) / 2,
      },
    ];
  }
  const dx = lineEndPosition.x - lineStartPosition.x;
  const dy = lineEndPosition.y - lineStartPosition.y;
  const length = Math.sqrt(dx * dx + dy * dy);
  const increment = length / (numPins - 1);
  const points = [];
  for (let i = 0; i < numPins; i++) {
    const x = lineStartPosition.x + (dx * (i * increment)) / length;
    const y = lineStartPosition.y + (dy * (i * increment)) / length;
    points.push({ x, y });
  }
  return points;
};
