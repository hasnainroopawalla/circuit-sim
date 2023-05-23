import { Position, Size } from "../models/RenderOptions";

export const textPositionInRect = (
  rectPosition: Position,
  rectSize: Size
): Position => {
  return {
    x: rectPosition.x + rectSize.w / 2,
    y: rectPosition.y + rectSize.h / 2,
  };
};

export const computeInputPinsPosition = (
  lineStartPosition: Position,
  lineEndPosition: Position,
  numPins: number
): Position[] => {
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
  var distance = Math.sqrt(dx * dx + dy * dy);
  const unitX = dx / distance;
  const unitY = dy / distance;
  const increment = distance / (numPins + 1);
  const points: Position[] = [];

  for (let i = 1; i <= numPins; i++) {
    var x = lineStartPosition.x + unitX * i * increment;
    var y = lineStartPosition.y + unitY * i * increment;
    points.push({ x, y });
  }
  return points;
};

export const computeIOPinPosition = (
  chipPosition: Position,
  chipSize: number,
  isInput: boolean
): Position => {
  const position = isInput
    ? chipPosition.x + chipSize
    : chipPosition.x - chipSize;
  return { x: position, y: chipPosition.y };
};

export const computeChipSize = (
  chipText: string,
  chipTextSize: number
): Size => {
  return {
    w: chipText.length * chipTextSize + 15,
    h: chipTextSize + chipTextSize,
  };
};

export const computeButtonSize = (
  buttonText: string,
  buttonTextSize: number
): Size => {
  return {
    w: buttonText.length * buttonTextSize,
    h: buttonTextSize + buttonTextSize / 1.5,
  };
};
