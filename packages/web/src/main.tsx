import { createRoot } from "react-dom/client";
import { ContextualApp } from "./components";

createRoot(document.getElementById("root") as HTMLElement).render(
	<ContextualApp />,
);
