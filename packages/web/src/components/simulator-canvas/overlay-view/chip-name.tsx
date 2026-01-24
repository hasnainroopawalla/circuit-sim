import type * as React from "react";

type ChipNameProps = {
	id: string;
	name: string;
	labelRef: React.RefObject<HTMLDivElement | null>;
};

export const ChipName: React.FC<ChipNameProps> = ({ id, labelRef, name }) => {
	return (
		<div
			id={`label-${id}`}
			ref={labelRef}
			className="absolute -translate-x-1/2 -translate-y-1/2 text-white select-none flex items-center justify-center p-1"
		>
			<span className="font-mono tracking-wide text-center overflow-hidden text-ellipsis whitespace-nowrap">
				{name}
			</span>
		</div>
	);
};
