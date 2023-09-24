import p5Types from "p5";
import { IButtonOptions } from "../simulator/render-options.interface";
import config from "../config";
import { computeButtonSize, textPositionInRect } from "../utils/Position";
import { initPosition } from "../utils/Utils";

class Button {
  p5: p5Types;
  text: string;
  options: IButtonOptions;
  onClickCb: () => void;

  constructor(p5: p5Types, text: string, onClickCb: () => void) {
    this.p5 = p5;
    this.text = text;
    this.onClickCb = onClickCb;
    this.options = {
      position: initPosition(),
      size: computeButtonSize(this.text, config.component.button.textSize),
      color: config.component.button.color,
    };
  }

  private renderText() {
    this.p5.fill(config.component.button.textColor);
    this.p5.textStyle(this.p5.BOLD);
    const textPosition = textPositionInRect(
      this.options.position,
      this.options.size
    );
    this.p5.textAlign(this.p5.CENTER, this.p5.CENTER);
    this.p5.textSize(config.component.button.textSize);
    this.p5.text(this.text, textPosition.x, textPosition.y);
  }

  public isMouseOver() {
    return (
      this.p5.mouseX >= this.options.position.x &&
      this.p5.mouseX <= this.options.position.x + this.options.size.w &&
      this.p5.mouseY >= this.options.position.y &&
      this.p5.mouseY <= this.options.position.y + this.options.size.h
    );
  }

  public mouseClicked = () => {
    if (this.isMouseOver()) {
      this.onClickCb();
    }
  };

  public render() {
    this.p5.strokeWeight(1);
    this.p5.stroke(config.document.strokeColor);
    this.p5.fill(this.options.color);
    this.p5.rect(
      this.options.position.x,
      this.options.position.y,
      this.options.size.w,
      this.options.size.h
    );
    this.renderText();
  }
}

export default Button;
