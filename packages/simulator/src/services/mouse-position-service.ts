import type { Camera } from "../camera";
import type { InputManager } from "../managers/input-manager";
import type { Simulator } from "../simulator";
import type { MousePosition } from "../types";
import { BaseService } from "./base-service";

export class MousePositionService extends BaseService {
	private inputManager: InputManager;
	private camera: Camera;

	constructor(args: {
		sim: Simulator;
		inputManager: InputManager;
		camera: Camera;
	}) {
		super(args.sim);

		this.inputManager = args.inputManager;
		this.camera = args.camera;
	}

	public getMousePosition(): MousePosition {
		const screenSpaceMousePosition = this.inputManager.getMousePosition();

		return {
			screen: screenSpaceMousePosition,
			world: this.camera.toWorldPosition(screenSpaceMousePosition),
		};
	}
}
