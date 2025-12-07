import * as React from "react";
import { useSimulatorApp } from "../../contexts/simulator-app-context";

type SimulatorOverlayState = ChipLabelProps & {
	id: string;
};

export const SimulatorOverlayView: React.FC = () => {
	const [chipLabels, setChipLabels] = React.useState<SimulatorOverlayState[]>(
		[],
	);

	const simulatorApp = useSimulatorApp();

	React.useEffect(() => {
		const unsubscribe = simulatorApp.sim.on(
			"overlay.update",
			({ chipLabels }) => {
				setChipLabels(chipLabels);
			},
		);

		return () => unsubscribe();
	}, [simulatorApp]);

	return (
		<div className="absolute inset-0 pointer-events-none border-2 border-red-600">
			{chipLabels.map((chip) => (
				<ChipLabel key={chip.id} position={chip.position} text={chip.text} />
			))}
		</div>
	);
};

type ChipLabelProps = {
	text: string;
	position: { x: number; y: number };
};

const ChipLabel: React.FC<ChipLabelProps> = ({ text, position }) => {
	return (
		<div
			className="absolute -translate-x-1/2 -translate-y-1/2 text-white text-sm font-medium select-none"
			style={{
				left: position.x,
				top: position.y,
			}}
		>
			{text}
		</div>
	);
};
