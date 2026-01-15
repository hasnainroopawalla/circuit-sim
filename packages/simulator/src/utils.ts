import type OrderedMap from "orderedmap";

/**
 * Iterates through *all* elements of the array and
 * returns true if `fn` returns true for any element.
 */
export function didAnyChange<T>(
	array: T[],
	fn: (element: T, idx: number) => boolean,
) {
	let changed = false;

	array.forEach((element, idx) => {
		changed ||= fn(element, idx);
	});

	return changed;
}

/**
 * Strongly type the keys of an object while iterating through them.
 */
export function ObjectKeys<T extends object>(obj: T) {
	return Object.keys(obj) as Array<keyof T>;
}

export function orderedMapSome<T>(
	orderedmap: OrderedMap<T>,
	fn: (value: T) => boolean,
) {
	orderedmap.forEach((_key, value) => {
		const handled = fn(value);
		if (handled) {
			return;
		}
	});
}
