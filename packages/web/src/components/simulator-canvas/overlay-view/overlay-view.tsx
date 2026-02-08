import * as React from "react";
import { useSimulatorApp } from "../../../contexts/simulator-app-context";
import { useEffectOnce } from "../../../utils";
import { useOverlayLabels } from "../hooks";
import { ChipName } from "./chip-name";
import { PinName } from "./pin-name";
import {
	EntityType,
	type OverlayLabelData,
} from "@digital-logic-sim/simulator";

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

export const OverlayLabel: React.FC<OverlayLabelData> = (label) => {
	const simulatorApp = useSimulatorApp();

	const labelRef = React.useRef<HTMLDivElement>(null);

	useEffectOnce(() => {
		if (labelRef.current) {
			simulatorApp.overlayManager.registerDomElement(
				label.id,
				labelRef.current,
			);
		}

		return () => {
			// simulatorApp.overlayManager.unregisterLabel(id, kind);
		};
	}, !!labelRef /* register the ref only after it has mounted */);

	switch (label.entityType) {
		case EntityType.Pin:
			return (
				<PinName
					labelRef={labelRef}
					id={label.id}
					name={label.text}
					pinType={label.pinType}
				/>
			);
		case EntityType.Chip:
			return <ChipName labelRef={labelRef} id={label.id} name={label.text} />;
	}
};
