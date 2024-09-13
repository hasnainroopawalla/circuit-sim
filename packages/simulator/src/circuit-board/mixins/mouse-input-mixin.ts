import p5 from "p5";
import { BaseMixin } from "power-mixin";
import {
  MouseInput,
  ICircuitBoard,
  CircuitBoardEntities,
} from "../circuit-board.interface";
import { IOChip, Chip, IOSlider } from "../../chips";
import { circuitBoardConfig } from "../circuit-board.config";
import { Pin } from "../../pin";

export type IMouseInputManager = {
  mouseClicked: () => void;
  mouseMoved: () => void;
  isMouseOver: () => boolean;
  isMouseOverIOChipPanel(kind: "input" | "output"): boolean;
  getMouseOverEntity(
    entities: CircuitBoardEntities
  ): IOChip | IOSlider | Pin | Chip | undefined;
};

class MouseInputManager implements IMouseInputManager {
  private p: p5;
  private circuitBoard: ICircuitBoard;

  private mouseReleaseAfterDrag: boolean;

  constructor(circuitBoard: ICircuitBoard, p: p5) {
    this.circuitBoard = circuitBoard;
    this.p = p;

    this.mouseReleaseAfterDrag = false;
  }

  public mouseClicked(): void {
    if (this.mouseReleaseAfterDrag) {
      this.mouseReleaseAfterDrag = false;
      return;
    }
    this.circuitBoard.getState().start(MouseInput.Click);
  }

  public mouseMoved(): void {
    this.circuitBoard.getState().start(MouseInput.Move);
  }

  public isMouseOver(): boolean {
    return (
      this.p.mouseX >= this.circuitBoard.position.x &&
      this.p.mouseX <=
        this.circuitBoard.position.x + this.circuitBoard.size.w &&
      this.p.mouseY >= this.circuitBoard.position.y &&
      this.p.mouseY <= this.circuitBoard.position.y + this.circuitBoard.size.h
    );
  }

  public isMouseOverIOChipPanel(kind: "input" | "output"): boolean {
    return kind === "input"
      ? this.p.mouseX >= 0 &&
          this.p.mouseX <= circuitBoardConfig.widthScale / 2 &&
          this.p.mouseY >= this.circuitBoard.position.y &&
          this.p.mouseY <=
            this.circuitBoard.position.y + this.circuitBoard.size.h
      : // !this.isMouseOverlapping(this.inputs)
        this.p.mouseX >=
          this.circuitBoard.position.x +
            this.circuitBoard.size.w +
            circuitBoardConfig.widthScale / 2 &&
          this.p.mouseY >= this.circuitBoard.position.y &&
          this.p.mouseY <=
            this.circuitBoard.position.y + this.circuitBoard.size.h;
    // !this.isMouseOverlapping(this.inputs)
  }

  public getMouseOverEntity(
    entities: CircuitBoardEntities
  ): IOChip | IOSlider | Pin | Chip | undefined {
    for (const entity of [
      ...entities.inputs,
      ...entities.outputs,
      ...entities.chips,
    ]) {
      const mouseOverEntity = entity.isMouseOverGetEntity();
      if (mouseOverEntity) {
        return mouseOverEntity;
      }
    }
  }
}

export class MouseInputMixin extends BaseMixin<
  ICircuitBoard,
  IMouseInputManager
> {
  constructor(p: p5) {
    super({
      methods: [
        "getMouseOverEntity",
        "isMouseOver",
        "isMouseOverIOChipPanel",
        "mouseClicked",
        "mouseMoved",
      ],
      props: [],
      initMixin: circuitBoard => new MouseInputManager(circuitBoard, p),
    });
  }
}
