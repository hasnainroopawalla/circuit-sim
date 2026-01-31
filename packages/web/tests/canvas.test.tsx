import * as React from "react";
import { render } from "@testing-library/react";
import { vi } from "vitest";
import { SimulatorCanvas } from "../src/components/simulator-canvas";

describe("Canvas component", () => {
	it("renders a canvas element", () => {
		const ref = React.createRef<HTMLCanvasElement>();
		const { container } = render(
			<SimulatorCanvas canvasRef={ref} onCanvasReady={vi.fn()} />,
		);

		const canvas = container.querySelector("canvas");
		expect(canvas).toBeInTheDocument();
		expect(canvas?.id).toBe("simulator-canvas");
	});

	it("assigns the ref to the canvas element", () => {
		const ref = React.createRef<HTMLCanvasElement>();
		render(<SimulatorCanvas canvasRef={ref} onCanvasReady={vi.fn()} />);

		expect(ref.current).toBeInstanceOf(HTMLCanvasElement);
	});
});
