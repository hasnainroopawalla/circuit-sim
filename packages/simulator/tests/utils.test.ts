import { didAnyChange } from "../src/utils";

describe("didAnyChange", () => {
	test("returns false for an empty array", () => {
		const result = didAnyChange([], () => true);
		expect(result).toBe(false);
	});

	test("returns false if callback never returns true", () => {
		const arr = [1, 2, 3];
		const result = didAnyChange(arr, (el) => el > 5);
		expect(result).toBe(false);
	});

	test("returns true if callback returns true for any element", () => {
		const arr = [1, 2, 3];
		const result = didAnyChange(arr, (el) => el === 2);
		expect(result).toBe(true);
	});

	test("callback receives correct element and index", () => {
		const arr = ["a", "b", "c"];
		const indices: number[] = [];
		didAnyChange(arr, (_, idx) => {
			indices.push(idx);
			return false;
		});
		expect(indices).toEqual([0, 1, 2]);
	});
});
