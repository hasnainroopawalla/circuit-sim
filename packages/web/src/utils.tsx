import * as React from "react";
import type { Position } from "@digital-logic-sim/shared-types";

export const useEffectOnce = (callback: () => () => void, predicate = true) => {
	const hasRun = React.useRef(false);
	const dispose = React.useRef(() => {});

	React.useEffect(() => {
		if (hasRun.current && predicate) {
			return;
		}

		dispose.current = callback();
		hasRun.current = true;

		return () => dispose.current();
	}, [callback, predicate]);
};

export const useStateRef = <T,>(initial: T) => {
	const [state, setState] = React.useState(initial);
	const ref = React.useRef(state);

	const set = React.useCallback((value: T | ((prev: T) => T)) => {
		setState((prev) => {
			const next =
				typeof value === "function" ? (value as (p: T) => T)(prev) : value;
			ref.current = next;
			return next;
		});
	}, []);

	return [state, set, ref] as const;
};

export const useOnClickOutside = (
	divRef: React.RefObject<HTMLDivElement | null>,
	onClickOutside: () => void,
) => {
	React.useEffect(() => {
		function handleClick(e: MouseEvent) {
			if (!divRef.current) {
				return;
			}

			if (!divRef.current.contains(e.target as Node)) {
				onClickOutside();
			}
		}
		document.addEventListener("mousedown", handleClick);

		return () => {
			document.removeEventListener("mousedown", handleClick);
		};
	}, [divRef, onClickOutside]);

	React.useEffect(() => {
		function handleClick(e: KeyboardEvent) {
			if (e.key === "Escape") {
				onClickOutside();
			}
		}
		document.addEventListener("keydown", handleClick);

		return () => {
			document.removeEventListener("keydown", handleClick);
		};
	}, [onClickOutside]);
};

/**
 * Returns the current mouse position in the document.
 */
export const useGetMousePosition = (): (() => Position) => {
	const positionRef = React.useRef<Position>({ x: 0, y: 0 });

	const getMousePosition = React.useCallback(() => positionRef.current, []);

	React.useEffect(() => {
		const handler = (e: MouseEvent) => {
			positionRef.current = {
				x: e.clientX,
				y: e.clientY,
			};
		};

		document.addEventListener("mousemove", handler);

		return () => document.removeEventListener("mousemove", handler);
	}, []);

	return getMousePosition;
};
