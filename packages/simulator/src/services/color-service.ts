import type { Simulator } from "../simulator";
import { BaseService } from "./base-service";

export class ColorService extends BaseService {
	constructor(args: {
		sim: Simulator;
	}) {
		super(args.sim);
	}
}

// TODO: replace with actual color gen service
export const COLORS = {
	Ghost: { r: 0.59, g: 0.59, b: 0.59, a: 0.7 },
	LowSignal: {
		r: 0.2,
		g: 0.1,
		b: 0.1,
		a: 1.0,
	},
	HighSignal: {
		r: 0.87,
		g: 0.37,
		b: 0.33,
		a: 1.0,
	},
};
