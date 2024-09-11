import p5 from "p5";
import { BaseMixin } from "power-mixin";
import { ICircuitBoard } from "../circuit-board-mixin";
import { IOChip, Chip, IOSlider } from "../../chips";
import { circuitBoardConfig } from "../circuit-board.config";
import { Pin } from "../../pin";
import { CircuitBoardEntities } from "./entity-manager-mixin";
import { State } from "./state-manager-mixin";

export enum Interaction {
  Click = "Click",
  DoubleClick = "DoubleClick",
  Drag = "Drag",
  Move = "Move",
}

export type IMouseInputManager = {
  mouseClicked: () => void;
  isMouseOver: () => void;
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
    console.log("CLICk");
    this.handleMouseInteraction(Interaction.Click);
  }

  public isMouseOver() {
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

  private handleMouseInteraction(interaction: Interaction): void {
    switch (this.circuitBoard.state) {
      case State.Idle:
        this.idleModeController.start(interaction);
        break;
      // case Mode.Reposition:
      //   this.repositionController.start(interaction);
      //   break;
      // case Mode.SpawnChip:
      //   this.chipSpawnController.start(interaction);
      //   break;
      // case Mode.SpawnIOChipHover:
      //   this.iOChipSpawnController.start(interaction);
      //   break;
      // case Mode.Wiring:
      //   this.wiringController.start(interaction);
      //   break;
    }
  }
}

export class MouseInputManagerMixin extends BaseMixin<
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
      ],
      props: [],
      initMixin: circuitBoard => new MouseInputManager(circuitBoard, p),
    });
  }
}
