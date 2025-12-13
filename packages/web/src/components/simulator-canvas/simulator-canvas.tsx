import * as React from "react";

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

	const handleContextMenu = React.useCallback(
		(e: React.MouseEvent<HTMLCanvasElement>) => {
			e.preventDefault();
		},
		[],
	);

	return (
		<canvas
			id="simulator-canvas"
			ref={canvasRef}
			className="absolute inset-0 w-full h-full"
			tabIndex={0} // required for focus
			onContextMenu={handleContextMenu}
		/>
	);
};
