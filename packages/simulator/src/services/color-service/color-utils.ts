export const ColorUtils = {
	hslToRgb01(
		h: number,
		s: number,
		l: number,
	): { r: number; g: number; b: number } {
		const k = (n: number) => (n + h / 30) % 12;
		const a = s * Math.min(l, 1 - l);
		const f = (n: number) =>
			l - a * Math.max(-1, Math.min(k(n) - 3, Math.min(9 - k(n), 1)));

		return {
			r: clamp01(f(0)),
			g: clamp01(f(8)),
			b: clamp01(f(4)),
		};
	},
};
function clamp01(v: number): number {
	return Math.max(0, Math.min(1, v));
}
