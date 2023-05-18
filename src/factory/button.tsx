import p5Types from "p5";
import { ButtonOptions } from "../models/RenderOptions";

class Button {
  p5: p5Types;
  text: string;
  options: ButtonOptions;
  onClickCb: () => void;

  constructor(
    p5: p5Types,
    text: string,
    onClickCb: () => void,
    options: ButtonOptions
  ) {
    this.p5 = p5;
    this.text = text;
    this.onClickCb = onClickCb;
    this.options = options;
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
    this.p5.fill(this.options.color);
    this.p5.stroke(1);
    this.p5.rect(
      this.options.position.x,
      this.options.position.y,
      this.options.size.w,
      this.options.size.h
    );
  }
}

export default Button;
