import type * as React from "react";

const CANVAS_ID = "simulator-canvas"; // TODO: move to config

type CanvasProps = {
	canvasRef: React.RefObject<HTMLCanvasElement | null>;
};

export const Canvas: React.FC<CanvasProps> = ({ canvasRef }) => (
	<canvas id={CANVAS_ID} ref={canvasRef} />
);
