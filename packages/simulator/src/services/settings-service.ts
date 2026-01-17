import type { Settings } from "@digital-logic-sim/shared-types";
import type { Simulator } from "../simulator";
import { BaseService } from "./base-service";

const DEFAULT_SETTINGS: Settings = {
	showGrid: true,
};

export class SettingsService extends BaseService {
	private settings: Settings = DEFAULT_SETTINGS;

	constructor(sim: Simulator) {
		super(sim);
	}

	public get(): Settings {
		return this.settings;
	}

	public set(newSettings: Settings): void {
		this.settings = newSettings;
	}
}
