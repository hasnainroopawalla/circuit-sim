import { createP5Instance } from "@circuit-sim/simulator";
import * as React from "react";

export const Sketch = () => {
	const p5ContainerRef = React.useRef(null);

	React.useEffect(() => {
		if (!p5ContainerRef.current) {
			throw new Error("Sketch not mounted");
		}
		const p5Instance = createP5Instance(p5ContainerRef.current);
		return () => p5Instance.remove();
	}, []);

	return <div className="sketch-container" ref={p5ContainerRef} />;
};
