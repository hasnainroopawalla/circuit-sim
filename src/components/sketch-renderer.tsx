import React, { useEffect, useRef } from "react";
import { sketch } from "../simulator";
import p5 from "p5";

export const Sketch = () => {
  const p5ContainerRef = useRef();

  useEffect(() => {
    const p5Instance = new p5(sketch, p5ContainerRef.current);
    return () => p5Instance.remove();
  }, []);

  return <div className="sketch-container" ref={p5ContainerRef} />;
};
