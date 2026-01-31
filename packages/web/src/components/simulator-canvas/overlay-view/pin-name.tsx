import type { PinType } from "@digital-logic-sim/simulator";
import type * as React from "react";
import { twMerge } from "tailwind-merge";

type PinNameProps = {
	id: string;
	name: string;
	labelRef: React.RefObject<HTMLDivElement | null>;
	pinType: PinType;
};

export const PinName: React.FC<PinNameProps> = ({
	id,
	labelRef,
	name,
	pinType,
}) => {
	return (
		<div
			id={`label-${id}`}
			ref={labelRef}
			className={twMerge(
				"text-white text-sm absolute pointer-events-none whitespace-nowrap -translate-y-1/2 justify-center items-center ",
				pinType === "in"
					? "-translate-x-[80%] text-left"
					: "-translate-x-[20%] ml-1.5 text-left",
			)}
		>
			<span className="bg-black/80 px-1 backdrop-blur-md shadow-lg">
				{name}
			</span>
		</div>
	);
};
