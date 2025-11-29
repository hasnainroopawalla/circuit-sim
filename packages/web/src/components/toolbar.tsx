import type * as React from "react";
import { useSimulatorApp } from "../contexts/simulator-app-context";
import type { ChipSpec } from "@digital-logic-sim/simulator";

export const Toolbar: React.FC = () => {
	const { getChipSpecs } = useSimulatorApp();

	return (
		<div className="absolute bottom-0 left-0 m-1 flex flex-row gap-2">
			{/* TODO: add chip spawn logic */}
			{getChipSpecs().map((chipSpec) => (
				<ToolbarItem key={chipSpec.name} chipSpec={chipSpec} />
			))}
		</div>
	);
};

type ToolbarItemProps = {
	chipSpec: ChipSpec;
};

const ToolbarItem: React.FC<ToolbarItemProps> = ({ chipSpec }) => {
	return <div className="bg-amber-500 p-0.5">{chipSpec.name}</div>;
};
