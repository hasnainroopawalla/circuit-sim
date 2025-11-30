import * as React from "react";
import { useSimulatorApp } from "../contexts/simulator-app-context";
import type { ChipSpec, SimulatorApp } from "@digital-logic-sim/simulator";

export const Toolbar: React.FC = () => {
	const simulatorApp = useSimulatorApp();

	return (
		<div className="absolute bottom-0 left-0 m-1 flex flex-row gap-2">
			{simulatorApp.sim.chipLibraryService.getAll().map((chipSpec) => (
				<ToolbarItem
					key={chipSpec.name}
					chipSpec={chipSpec}
					simulatorApp={simulatorApp}
				/>
			))}
		</div>
	);
};

type ToolbarItemProps = {
	chipSpec: ChipSpec;
	simulatorApp: Pick<SimulatorApp, "sim">;
};

const ToolbarItem: React.FC<ToolbarItemProps> = ({
	chipSpec,
	simulatorApp,
}) => {
	const onClick = React.useCallback(
		(e: React.MouseEvent<HTMLDivElement>) => {
			e.preventDefault();
			simulatorApp.sim.emit("chip.spawn", chipSpec);
		},
		[simulatorApp, chipSpec],
	);

	return (
		<div className="bg-amber-500 p-0.5" onMouseDown={onClick}>
			{chipSpec.name}
		</div>
	);
};
