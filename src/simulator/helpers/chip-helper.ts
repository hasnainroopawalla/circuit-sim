import { Position, Size } from "../shared.interface";

// TODO: Test coverage
export class ChipHelper {
  public static textPositionInRect(
    rectPosition: Position,
    rectSize: Size
  ): Position {
    return {
      x: rectPosition.x + rectSize.w / 2,
      y: rectPosition.y + rectSize.h / 2,
    };
  }

  public static pinsPositions(
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

  public static chipSize(
    chipText: string,
    chipTextSize: number,
    numPins: number
  ): Size {
    return {
      w: chipText.length * chipTextSize + 15,
      h: chipTextSize * numPins + 10,
    };
  }
}
