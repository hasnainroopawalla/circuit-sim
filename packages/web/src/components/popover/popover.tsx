import * as React from "react";
import { usePopover } from "./popover-context";
import { useClosePopoverShortcut, usePopoverPosition } from "./hooks";
import { useOnClickOutside } from "../../utils";

export const Popover: React.FC = () => {
	const { popoverProps, closePopover } = usePopover();

	useClosePopoverShortcut(closePopover);

	const divRef = React.useRef<HTMLDivElement>(null);
	const dimensionsRef = React.useRef({ width: 0, height: 0 });
	const [dimensions, setDimensions] = React.useState({ width: 0, height: 0 });

	useOnClickOutside(divRef, closePopover);

	React.useEffect(() => {
		if (!divRef.current) return;

		const resizeObserver = new ResizeObserver(() => {
			if (divRef.current) {
				const rect = divRef.current.getBoundingClientRect();
				const newDimensions = { width: rect.width, height: rect.height };

				// Only update if dimensions actually changed
				if (
					newDimensions.width !== dimensionsRef.current.width ||
					newDimensions.height !== dimensionsRef.current.height
				) {
					dimensionsRef.current = newDimensions;
					setDimensions(newDimensions);
				}
			}
		});

		resizeObserver.observe(divRef.current);

		return () => resizeObserver.disconnect();
	}, []);

	const { x, y } = popoverProps?.position ?? { x: 0, y: 0 }; // TODO

	const { top, left } = usePopoverPosition(
		x,
		y,
		dimensions.width,
		dimensions.height,
	);

	if (!popoverProps || !popoverProps.isOpen || !popoverProps.position) {
		return null;
	}

	return (
		<div
			ref={divRef}
			className="fixed z-100 shadow-lg rounded-lg bg-neutral-900/90 backdrop-blur-xl ring-1 ring-white/10"
			style={{
				top: `${top}px`,
				left: `${left}px`,
				pointerEvents: "auto",
			}}
		>
			<div className="overflow-hidden">{popoverProps.children}</div>
		</div>
	);
};
