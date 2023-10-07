import { Position, Size } from "./shared.interface";

export default class Utils {
  public static textPositionInRect(
    rectPosition: Position,
    rectSize: Size
  ): Position {
    return {
      x: rectPosition.x + rectSize.w / 2,
      y: rectPosition.y + rectSize.h / 2,
    };
  }

  public static inputPinsPosition(
    lineStartPosition: Position,
    lineEndPosition: Position,
    numPins: number
  ): Position[] {
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
    const points: Position[] = [];

    for (let i = 1; i <= numPins; i++) {
      const x = lineStartPosition.x + unitX * i * increment;
      const y = lineStartPosition.y + unitY * i * increment;
      points.push({ x, y });
    }
    return points;
  }

  public static iOPinPosition(
    chipPosition: Position,
    chipSize: number,
    isInput: boolean
  ): Position {
    const position = isInput
      ? chipPosition.x + chipSize
      : chipPosition.x - chipSize;
    return { x: position, y: chipPosition.y };
  }

  public static chipSize(
    chipText: string,
    chipTextSize: number,
    numPins: number
  ): Size {
    return {
      w: chipText.length * chipTextSize + 15,
      h: chipTextSize * numPins + 15,
    };
  }

  public static entityHasConnectedWires(
    pins: string[],
    wires: string[][]
  ): boolean {
    return pins.some((pin) => {
      return wires.some((wire) => {
        return wire.some((id) => id === pin);
      });
    });
  }
}
