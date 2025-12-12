import * as React from "react";
import { useSimulatorApp } from "../../contexts/simulator-app-context";
import { useEffectOnce } from "../../utils";

type Label = {
	id: string;
	text: string;
};

export const SimulatorOverlayView: React.FC = () => {
	const simulatorApp = useSimulatorApp();

	const [labels, setLabels] = React.useState<Label[]>([]);

	React.useEffect(() => {
		const unsubscribe = simulatorApp.sim.on(
			"entity.spawn.finish",
			({ entity }) => {
				setLabels((prev) => [
					...prev,
					{
						id: entity.id,
						text: entity.name,
					},
				]);
			},
		);

		return () => unsubscribe();
	}, [simulatorApp]);

	return (
		<div
			id="simulator-overlay-view"
			className="absolute inset-0 pointer-events-none"
		>
			{labels.map((label) => (
				<OverlayLabel key={label.id} id={label.id} text={label.text} />
			))}
		</div>
	);
};

export const OverlayLabel: React.FC<Label> = ({ id, text }) => {
	const simulatorApp = useSimulatorApp();

	const labelRef = React.useRef<HTMLDivElement>(null);

	useEffectOnce(() => {
		if (!labelRef.current) {
			return () => {};
		}

		simulatorApp.overlayManager.registerLabel(id, labelRef.current);

		return () => {
			// simulatorApp.overlayManager.unregisterLabel(id);
		};
	}, !!labelRef /* register the ref only after it has mounted */);

	return (
		<div
			id={`label-${id}`}
			ref={labelRef}
			className="absolute -translate-x-1/2 -translate-y-1/2 text-white text-sm font-medium select-none"
		>
			{text}
		</div>
	);
};
