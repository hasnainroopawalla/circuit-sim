import { colors } from "../colors";

class ColorGenerator {
  colors: string[];

  constructor() {
    this.colors = colors;
  }

  public generate() {
    if (this.colors.length === 0) {
      this.colors = [...colors];
    }
    const index = Math.floor(Math.random() * this.colors.length);
    const item = this.colors[index];
    this.colors.splice(index, 1);
    return item;
  }
}

const colorGenerator = new ColorGenerator();
export default colorGenerator;
