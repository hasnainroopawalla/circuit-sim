import type { Simulator } from "../../simulator";
import { randomInRange } from "../../utils";
import { BaseService } from "../base-service";
import { ColorUtils } from "./color-utils";

export class ColorService extends BaseService {
	constructor(args: {
		sim: Simulator;
	}) {
		super(args.sim);
	}

	public static generateChipColor() {
		const hue = Math.random() * 360;

		// Visually rich but readable
		const saturation = randomInRange(0.55, 0.75);
		const lightness = randomInRange(0.3, 0.42);

		const { r, g, b } = ColorUtils.hslToRgb01(hue, saturation, lightness);
		return { r, g, b, a: 1 };
	}
}
