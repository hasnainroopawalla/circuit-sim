import * as React from "react";
import { useSimulatorApp } from "../../contexts/simulator-app-context";
import { useEffectOnce } from "../../utils";
import type { OverlayElementKind } from "@digital-logic-sim/simulator";

type Label = {
	id: string;
	text: string;
	kind: OverlayElementKind;
};

export const SimulatorOverlayView: React.FC = () => {
	const simulatorApp = useSimulatorApp();

	const [labels, setLabels] = React.useState<Label[]>([]);

	React.useEffect(() => {
		const unsubscribe = simulatorApp.sim.on(
			"chip.spawn.finish",
			({ chipId, chipName, pins }) => {
				setLabels((prev) => {
					const newLabels = [
						{
							id: chipId,
							text: chipName,
							kind: "static" as const,
						},
						...pins.map((pin) => ({
							id: pin.id,
							text: pin.name,
							kind: "hover" as const,
						})),
					];

					return [...prev, ...newLabels];
				});
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
				<OverlayLabel
					key={label.id}
					id={label.id}
					text={label.text}
					kind={label.kind}
				/>
			))}
		</div>
	);
};

export const OverlayLabel: React.FC<Label> = ({ id, text, kind }) => {
	const simulatorApp = useSimulatorApp();

	const labelRef = React.useRef<HTMLDivElement>(null);

	useEffectOnce(() => {
		if (!labelRef.current) {
			return () => {};
		}

		simulatorApp.overlayManager.registerLabel(id, labelRef.current, kind);

		return () => {
			// simulatorApp.overlayManager.unregisterLabel(id, kind);
		};
	}, !!labelRef /* register the ref only after it has mounted */);

	return (
		<div
			id={`label-${id}`}
			ref={labelRef}
			className="absolute -translate-x-1/2 -translate-y-1/2 text-white text-sm font-medium select-none"
		>
			{getLabelElement(text, kind)}
		</div>
	);
};

const ChipName: React.FC<{ name: string }> = ({ name }) => {
	return <span>{name}</span>;
};

const PinName: React.FC<{ name: string }> = ({ name }) => {
	return (
		<span className="bg-black/80 py-1 px-2 backdrop-blur-md shadow-lg">
			{name}
		</span>
	);
};

// TODO: improve this method to correctly select which component to render
// switching on `kind` is incorrect
const getLabelElement = (text: string, kind: OverlayElementKind) => {
	switch (kind) {
		case "hover":
			return <PinName name={text} />;
		case "static":
			return <ChipName name={text} />;
	}
};
