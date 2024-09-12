import p5 from "p5";
import { BaseMixin } from "power-mixin";
import { ICircuitBoard } from "../circuit-board-mixin";
import { circuitBoardConfig } from "../circuit-board.config";
import { AbstractRenderer, Position, Size } from "../../common";

export type IRenderer = {
  // TODO: should the name live here?
  name: string;
  render: () => void;
  position: Position;
  size: Size<"rect">;
};

class Renderer extends AbstractRenderer<Size<"rect">> implements IRenderer {
  public name: string;

  private circuitBoard: ICircuitBoard;

  constructor(
    circuitBoard: ICircuitBoard,
    p: p5,
    name: string,
    position: Position,
    size: Size<"rect">
  ) {
    super(p, position, size);
    this.name = name;
    this.circuitBoard = circuitBoard;
  }

  public render() {
    this.p.push();
    this.p.stroke(circuitBoardConfig.strokeColor);
    this.p.strokeWeight(2);
    this.p.fill(circuitBoardConfig.background);
    this.p.rect(this.position.x, this.position.y, this.size.w, this.size.h);
    // slider section
    this.p.strokeWeight(0);
    this.p.fill(circuitBoardConfig.sliderSectionColor);
    this.p.rect(
      0,
      this.position.y,
      circuitBoardConfig.widthScale / 2,
      this.size.h
    );
    this.p.rect(
      this.position.x + this.size.w + circuitBoardConfig.widthScale / 2,
      this.position.y,
      circuitBoardConfig.widthScale / 2,
      this.size.h
    );
    this.p.pop();

    this.renderWires();
    this.renderChips();
    this.renderIOChips();

    this.circuitBoard.getState().render();
  }

  private renderIOChips(): void {
    this.circuitBoard.entities.inputs.forEach(inputIOChip =>
      inputIOChip.render()
    );
    this.circuitBoard.entities.outputs.forEach(outputIOChip =>
      outputIOChip.render()
    );
  }

  private renderChips(): void {
    this.circuitBoard.entities.chips.forEach(chip => chip.render());
  }

  private renderWires(): void {
    this.circuitBoard.entities.wires.forEach(wire => wire.render());
  }
}

export class RendererMixin extends BaseMixin<ICircuitBoard, IRenderer> {
  constructor(p: p5, name: string, position: Position, size: Size<"rect">) {
    super({
      methods: ["render"],
      props: ["position", "size"],
      initMixin: circuitBoard =>
        new Renderer(circuitBoard, p, name, position, size),
    });
  }
}
