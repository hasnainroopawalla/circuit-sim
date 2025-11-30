import * as React from "react";

const CANVAS_ID = "simulator-canvas"; // TODO: move to config

type SimulatorCanvasProps = {
	onCanvasReady: (canvas: HTMLCanvasElement) => void;
	canvasRef: React.RefObject<HTMLCanvasElement | null>;
};

export const SimulatorCanvas = ({
	onCanvasReady,
	canvasRef,
}: React.PropsWithChildren<SimulatorCanvasProps>) => {
	React.useLayoutEffect(() => {
		if (canvasRef.current) {
			onCanvasReady(canvasRef.current);
		}
	}, [canvasRef.current, onCanvasReady]);

	return (
		<canvas
			className="h-full w-full"
			id={CANVAS_ID}
			ref={canvasRef}
			tabIndex={0} // required for focus
		/>
	);
};
