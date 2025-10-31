import type * as React from "react";

const CANVAS_ID = "simulator-canvas"; // TODO: move to config

export const Canvas: React.FC = () => <canvas id={CANVAS_ID} />;
