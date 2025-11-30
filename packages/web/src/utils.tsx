import * as React from "react";

export const useEffectOnce = (callback: () => () => void) => {
	const hasRun = React.useRef(false);
	const dispose = React.useRef(() => {});

	React.useEffect(() => {
		if (hasRun.current) {
			return;
		}

		dispose.current = callback();
		hasRun.current = true;

		return () => dispose.current();
	}, [callback]);
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
