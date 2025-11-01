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
