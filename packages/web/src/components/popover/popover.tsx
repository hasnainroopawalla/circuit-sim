import * as React from "react";
import { useOnClickOutside } from "../../utils";
import { useSimulatorApp } from "../../contexts/simulator-app-context";
import type { IEntitySecondaryActionEvent } from "@digital-logic-sim/simulator";
import { ActionMenu } from "./action-menu";

export const Popover: React.FC = () => {
	const simulatorApp = useSimulatorApp();

	const divRef = React.useRef<HTMLDivElement>(null);

	const [popoverData, setPopoverData] =
		React.useState<IEntitySecondaryActionEvent | null>(null);

	const closePopover = React.useCallback(() => {
		setPopoverData(null);
	}, []);

	useOnClickOutside(divRef, closePopover);

	React.useEffect(() => {
		const unubscribe = simulatorApp.sim.on("entity.secondaryAction", (data) => {
			setPopoverData(data);
		});

		return () => {
			unubscribe();
		};
	}, [simulatorApp]);

	return (
		popoverData && (
			<div
				ref={divRef}
				className="fixed z-50 rounded-lg shadow-lg bg-neutral-900/90 backdrop-blur-xl ring-1 ring-white/10"
				style={{
					top: `${popoverData.mousePosition.y}px`,
					left: `${popoverData.mousePosition.x}px`,
				}}
			>
				<div className="overflow-hidden">
					<ActionMenu
						onView={() => {
							console.log("view", popoverData);
							setPopoverData(null);
						}}
						onDelete={() => {
							console.log("delete", popoverData);
							setPopoverData(null);
						}}
					/>
				</div>
			</div>
		)
	);
};

function renderActionComponent(action: unknown): React.ReactNode {
	return <div>{JSON.stringify(action)}</div>;
}
