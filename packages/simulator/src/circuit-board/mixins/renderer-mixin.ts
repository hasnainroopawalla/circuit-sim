import p5 from "p5";
import { BaseMixin } from "power-mixin";
import { ICircuitBoard } from "../circuit-board-mixin";
import { IOChip, Chip } from "../../chips";
import { Wire } from "../../wire";
import { circuitBoardConfig } from "../circuit-board.config";
import { AbstractRenderer, Position, Size } from "../../common";

export type IRenderer = {
  render: () => void;
  position: Position;
  size: Size<"rect">;
};

class Renderer extends AbstractRenderer<Size<"rect">> implements IRenderer {
  private circuitBoard: ICircuitBoard;

  constructor(
    circuitBoard: ICircuitBoard,
    p: p5,
    position: Position,
    size: Size<"rect">
  ) {
    super(p, position, size);
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
  }

  private renderIOChips(inputIOChips: IOChip[], outputIOChips: IOChip[]): void {
    inputIOChips.forEach(inputIOChip => inputIOChip.render());
    outputIOChips.forEach(outputIOChip => outputIOChip.render());
  }

  private renderChips(chips: Chip[]): void {
    chips.forEach(chip => chip.render());
  }

  private renderWires(wires: Wire[]): void {
    wires.forEach(wire => wire.render());
  }
}

export class RendererMixin extends BaseMixin<ICircuitBoard, IRenderer> {
  constructor(p: p5, position: Position, size: Size<"rect">) {
    super({
      methods: ["render"],
      props: ["position", "size"],
      initMixin: circuitBoard => new Renderer(circuitBoard, p, position, size),
    });
  }
}
