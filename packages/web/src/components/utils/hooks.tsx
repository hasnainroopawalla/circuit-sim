import * as React from "react";

/**
 * A modified useEffect that executes once only when the predicate is true.
 */
export const useEffectOnce = (
	callback: () => () => void,
	predicate: boolean,
) => {
	const isExecutedRef = React.useRef(false);

	const callbackRef = useSyncedRef(callback);
	const disposeRef = React.useRef(() => {});

	React.useEffect(() => {
		if (predicate && !isExecutedRef.current) {
			disposeRef.current = callbackRef.current();
			isExecutedRef.current = true;
		}

		return () => disposeRef.current();
	}, [predicate, callbackRef]);
};

const useSyncedRef = <T,>(value: T) => {
	const valueRef = React.useRef<T>(value);
	valueRef.current = value;
	return valueRef;
};
