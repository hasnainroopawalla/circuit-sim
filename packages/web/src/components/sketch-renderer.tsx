import * as React from "react";
import { useSimulator } from "./simulator-context";
import { useEffectOnce } from "./utils/hooks";

export const Sketch: React.FC = () => {
	const containerRef = React.useRef(null);

	const simulator = useSimulator();

	useEffectOnce(() => {
		if (!containerRef.current) {
			throw new Error("Sketch not mounted");
		}

		const p5Instance = simulator.createSketch(containerRef.current);

		return () => p5Instance.remove();
	}, true /* run on mount */);

	return <div className="sketch-container" ref={containerRef} />;
};
