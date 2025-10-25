import type * as React from "react";
import { Dialog, DialogProvider } from "../dialog";
import { ButtonsPanel } from "./buttons-panel";

export const Toolbar: React.FC = () => (
	<DialogProvider>
		<ButtonsPanel />
		<Dialog />
	</DialogProvider>
);
