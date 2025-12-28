import * as React from "react";
import { useSimulatorApp } from "../../../contexts/simulator-app-context";
import { useEffectOnce } from "../../../utils";
import { useOverlayLabels, type Label } from "../hooks";
import { ChipName } from "./chip-name";
import { PinName } from "./pin-name";

export const SimulatorOverlayView: React.FC = () => {
	const labels = useOverlayLabels();

	return (
		<div
			id="simulator-overlay-view"
			className="absolute inset-0 pointer-events-none"
		>
			{labels.map((label) => (
				<OverlayLabel key={label.id} {...label} />
			))}
		</div>
	);
};

export const OverlayLabel: React.FC<Label> = (label) => {
	const simulatorApp = useSimulatorApp();

	const labelRef = React.useRef<HTMLDivElement>(null);

	useEffectOnce(() => {
		if (!labelRef.current) {
			return () => {};
		}

		simulatorApp.overlayManager.registerLabel(
			label.id,
			labelRef.current,
			label.kind,
		);

		return () => {
			// simulatorApp.overlayManager.unregisterLabel(id, kind);
		};
	}, !!labelRef /* register the ref only after it has mounted */);

	switch (label.entityType) {
		case "pin":
			return (
				<PinName
					labelRef={labelRef}
					id={label.id}
					name={label.text}
					pinType={label.pinType}
				/>
			);
		case "chip":
			return <ChipName labelRef={labelRef} id={label.id} name={label.text} />;
	}
};
