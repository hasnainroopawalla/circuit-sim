import { Position, State } from "./shared.interface";

import { Chip, IOChip } from "./chip";
import { Wire } from "./wire";
import { config } from "../config";
import { RenderEngine } from "./render-engine";

type PinRenderOptions = {
  position: Position;
  size: number;
};

export class Pin {
  p: p5;
  name: string;
  id: number;
  state: State;
  isInput: boolean; // TODO: change to type: "input" | "output"
  isGhost: boolean;
  outgoingWires: Wire[];
  chip: Chip | IOChip;
  options: PinRenderOptions;
  renderEngine: RenderEngine;

  constructor(
    p5: p5,
    id: number,
    state: State,
    isInput: boolean,
    chip: Chip | IOChip,
    isGhost = false
  ) {
    this.p = p5;
    this.id = id;
    this.state = state;
    this.isInput = isInput;
    this.outgoingWires = [];
    this.chip = chip;
    this.options = {
      position: { x: 0, y: 0 },
      size: config.component.pin.size,
    };
    this.name = `${isInput ? "In" : "Out"} ${id}`;
    this.isGhost = isGhost;
    this.renderEngine = new RenderEngine(this.p);
  }

  public isMouseOver(): boolean {
    return (
      this.p.dist(
        this.p.mouseX,
        this.p.mouseY,
        this.options.position.x,
        this.options.position.y
      ) <=
      this.options.size / 2
    );
  }

  public propagate(): void {
    for (let i = 0; i < this.outgoingWires.length; i++) {
      this.outgoingWires[i].propagate();
    }
  }

  public setPosition(position: Position): void {
    this.options.position = position;
  }

  public mouseClicked(): boolean {
    return this.isMouseOver();
  }

  public render(): void {
    this.renderEngine.renderPin(
      this.name,
      this.options.position,
      this.options.size,
      this.isMouseOver(),
      this.isInput,
      this.isGhost
    );
  }
}
