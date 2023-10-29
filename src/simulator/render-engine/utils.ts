import { Position } from "../shared.interface";

export class Utils {
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
}
