/**
 * Strongly type the keys of an object while iterating through them.
 */
export const ObjectKeys = <T extends object>(obj: T) =>
	Object.keys(obj) as Array<keyof T>;
