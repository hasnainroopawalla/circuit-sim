import p5 from "p5";
import { type Position, type Size, AbstractRenderer } from "../common";
import type { Chip, IOChip, IOSlider } from "../chips";
import { Pin } from "../pin";
import { Wire } from "../wire";
import { circuitBoardConfig } from "./circuit-board.config";
import type { CircuitBoardEntities } from "./circuit-board.interface";

export class CircuitRenderer extends AbstractRenderer<Size<"rect">> {
  constructor(p: p5, position: Position, size: Size<"rect">) {
    super(p, position, size);
  }

  public isMouseOver() {
    return (
      this.p.mouseX >= this.position.x &&
      this.p.mouseX <= this.position.x + this.size.w &&
      this.p.mouseY >= this.position.y &&
      this.p.mouseY <= this.position.y + this.size.h
    );
  }

  public isMouseOverIOChipPanel(kind: "input" | "output"): boolean {
    return kind === "input"
      ? this.p.mouseX >= 0 &&
          this.p.mouseX <= circuitBoardConfig.widthScale / 2 &&
          this.p.mouseY >= this.position.y &&
          this.p.mouseY <= this.position.y + this.size.h
      : // !this.isMouseOverlapping(this.inputs)
        this.p.mouseX >=
          this.position.x + this.size.w + circuitBoardConfig.widthScale / 2 &&
          this.p.mouseY >= this.position.y &&
          this.p.mouseY <= this.position.y + this.size.h;
    // !this.isMouseOverlapping(this.inputs)
  }

  public mouseDragged(): void {}

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

  public renderIOChips(inputIOChips: IOChip[], outputIOChips: IOChip[]): void {
    inputIOChips.forEach((inputIOChip) => inputIOChip.render());
    outputIOChips.forEach((outputIOChip) => outputIOChip.render());
  }

  public renderChips(chips: Chip[]): void {
    chips.forEach((chip) => chip.render());
  }

  public renderWires(wires: Wire[]): void {
    wires.forEach((wire) => wire.render());
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
