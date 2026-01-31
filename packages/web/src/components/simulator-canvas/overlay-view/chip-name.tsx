import type * as React from "react";
import { ChipLabelUtils } from "@digital-logic-sim/simulator";

type ChipNameProps = {
	id: string;
	name: string;
	labelRef: React.RefObject<HTMLDivElement | null>;
};

export const ChipName: React.FC<ChipNameProps> = ({ id, labelRef, name }) => {
	const lines = ChipLabelUtils.splitChipName(name);

	return (
		<div
			id={`label-${id}`}
			ref={labelRef}
			className="absolute -translate-x-1/2 -translate-y-1/2 text-white select-none flex flex-col items-center justify-center px-2"
		>
			{lines.slice(0, 2).map((line) => (
				<span
					key={line}
					className="font-mono tracking-wide text-center leading-tight"
				>
					{line}
				</span>
			))}
		</div>
	);
};
