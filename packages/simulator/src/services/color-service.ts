import type { Simulator } from "../simulator";
import { BaseService } from "./base-service";

export class ColorService extends BaseService {
	constructor(args: {
		sim: Simulator;
	}) {
		super(args.sim);
	}

	public static getHighColor() {
		return {
			r: 0.87,
			g: 0.37,
			b: 0.33,
			a: 1.0,
		};
	}

	public static getLowColor() {
		return {
			r: 0.2,
			g: 0.1,
			b: 0.1,
			a: 1.0,
		};
	}
}
