import * as React from "react";

export const useClosePopoverShortcut = (closePopover: () => void) => {
	React.useEffect(() => {
		const handler = (e: KeyboardEvent) => {
			if (e.key === "Escape") {
				closePopover();
			}
		};

		window.addEventListener("keydown", handler);

		return () => window.removeEventListener("keydown", handler);
	}, [closePopover]);
};

export const usePopoverPosition = (
	x: number,
	y: number,
	width: number,
	height: number,
	offsetDistance = 8,
) => {
	const [position, setPosition] = React.useState<{
		top: number;
		left: number;
		placement: "top" | "bottom" | "left" | "right";
	}>({ top: y, left: x, placement: "bottom" });

	React.useEffect(() => {
		// Get viewport dimensions
		const viewportWidth = window.innerWidth;
		const viewportHeight = window.innerHeight;

		// Calculate distances to each edge
		const distanceToTop = y;
		const distanceToBottom = viewportHeight - y;
		const distanceToLeft = x;
		const distanceToRight = viewportWidth - x;

		let placement: "top" | "bottom" | "left" | "right" = "bottom";
		let finalX = x;
		let finalY = y + offsetDistance;

		// Determine best placement
		if (distanceToBottom > height + offsetDistance) {
			// Enough space below
			placement = "bottom";
			finalY = y + offsetDistance;
		} else if (distanceToTop > height + offsetDistance) {
			// Enough space above
			placement = "top";
			finalY = y - height - offsetDistance;
		} else if (distanceToRight > width + offsetDistance) {
			// Enough space to the right
			placement = "right";
			finalX = x + offsetDistance;
			finalY = y - height / 2;
		} else if (distanceToLeft > width + offsetDistance) {
			// Enough space to the left
			placement = "left";
			finalX = x - width - offsetDistance;
			finalY = y - height / 2;
		}

		// Clamp position to viewport with some padding
		const padding = 4;
		finalX = Math.max(
			padding,
			Math.min(finalX, viewportWidth - width - padding),
		);
		finalY = Math.max(
			padding,
			Math.min(finalY, viewportHeight - height - padding),
		);

		setPosition({ top: finalY, left: finalX, placement });
	}, [x, y, width, height, offsetDistance]);

	return position;
};
