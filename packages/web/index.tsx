import * as React from "react";
import { createRoot } from "react-dom/client";
import { App } from "./src/components";

createRoot(document.getElementById("root") as HTMLElement).render(<App />);
