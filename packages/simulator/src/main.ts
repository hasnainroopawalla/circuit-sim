import { Simulator } from "./simulator";

const createCanvas = (): HTMLCanvasElement => {
	const canvas = <HTMLCanvasElement>document.getElementById("simulator-canvas");
	canvas.width = canvas.clientWidth * window.devicePixelRatio;
	canvas.height = canvas.clientHeight * window.devicePixelRatio;
	canvas.focus();
	return canvas;
};

export const runSimulator = (): void => {
	const canvas = createCanvas();

	const simulator = new Simulator({ canvas });
	simulator.start();
};
