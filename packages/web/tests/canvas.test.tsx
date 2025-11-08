import * as React from "react";
import { render } from "@testing-library/react";
import { Canvas } from "../src/components/canvas";

describe("Canvas component", () => {
  it("renders a canvas element", () => {
    const ref = React.createRef<HTMLCanvasElement>();
    const { container } = render(<Canvas canvasRef={ref} />);
    
    const canvas = container.querySelector("canvas");
    expect(canvas).toBeInTheDocument();
    expect(canvas?.id).toBe("simulator-canvas");
  });

  it("assigns the ref to the canvas element", () => {
    const ref = React.createRef<HTMLCanvasElement>();
    render(<Canvas canvasRef={ref} />);

    expect(ref.current).toBeInstanceOf(HTMLCanvasElement);
  });
});
