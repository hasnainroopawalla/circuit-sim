import { IPosition, ISize } from "../simulator/render-options.interface";

export const textPositionInRect = (
  rectPosition: IPosition,
  rectSize: ISize
): IPosition => {
  return {
    x: rectPosition.x + rectSize.w / 2,
    y: rectPosition.y + rectSize.h / 2,
  };
};

export const computeInputPinsPosition = (
  lineStartPosition: IPosition,
  lineEndPosition: IPosition,
  numPins: number
): IPosition[] => {
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
  const distance = Math.sqrt(dx * dx + dy * dy);
  const unitX = dx / distance;
  const unitY = dy / distance;
  const increment = distance / (numPins + 1);
  const points: IPosition[] = [];

  for (let i = 1; i <= numPins; i++) {
    const x = lineStartPosition.x + unitX * i * increment;
    const y = lineStartPosition.y + unitY * i * increment;
    points.push({ x, y });
  }
  return points;
};

export const computeIOPinPosition = (
  chipPosition: IPosition,
  chipSize: number,
  isInput: boolean
): IPosition => {
  const position = isInput
    ? chipPosition.x + chipSize
    : chipPosition.x - chipSize;
  return { x: position, y: chipPosition.y };
};

export const computeChipSize = (
  chipText: string,
  chipTextSize: number,
  numPins: number
): ISize => {
  return {
    w: chipText.length * chipTextSize + 15,
    h: chipTextSize * numPins + 15,
  };
};

export const computeButtonSize = (
  buttonText: string,
  buttonTextSize: number
): ISize => {
  return {
    w: buttonText.length * buttonTextSize,
    h: buttonTextSize + buttonTextSize / 1.5,
  };
};
