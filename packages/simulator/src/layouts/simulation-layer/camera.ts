import type {
	CameraProjectionData,
	Position,
} from "@digital-logic-sim/render-engine";
import { mat4, vec3, vec4, type Vec3Arg } from "wgpu-matrix";
import type {
	KeyboardButtonType,
	ButtonEvent,
} from "../../managers/input-manager";
import type { Simulator } from "../../simulator";

type VelocityDelta = [number, number, number];

const cameraConfig = {
	speed: 5,
	zoomSpeed: 20,
	cameraFOV: 60,
	cameraUp: [0, 1, 0],
	keyToVelocityDeltaMap: new Map<KeyboardEvent["key"], VelocityDelta>([
		["w", [0, -1, 0]],
		["s", [0, 1, 0]],
		["a", [1, 0, 0]],
		["d", [-1, 0, 0]],
	]),
};

type CameraArgs = {
	canvas: HTMLCanvasElement;
	sim: Simulator;
};

export class Camera {
	private sim: Simulator;

	private eye: Float32Array;

	private screenDimensions: {
		height: number;
		width: number;
	};

	private inputVelocity: Float32Array;

	constructor(args: CameraArgs) {
		this.sim = args.sim;

		this.eye = new Float32Array([0, 0, -5]);
		this.inputVelocity = new Float32Array([0, 0, 0]);

		this.screenDimensions = {
			height: args.canvas.height,
			width: args.canvas.width,
		};
	}

	public update(deltaTime: number): void {
		const cameraVelocity = new Float32Array([
			this.inputVelocity[0] * cameraConfig.speed,
			this.inputVelocity[1] * cameraConfig.speed,
			this.inputVelocity[2] * cameraConfig.zoomSpeed,
		]);

		this.eye = vec3.add(this.eye, vec3.scale(cameraVelocity, deltaTime));
		this.eye[2] = clamp(this.eye[2], -10, -1);

		// reset the velocity to avoid zoom spikes
		this.inputVelocity.set([0, 0, 0]);
	}

	public getProjectionData(): CameraProjectionData {
		const viewProjectionMatrix = this.getViewProjectionMatrix();
		return {
			viewProjectionMatrix,
			viewProjectionInvMatrix:
				this.getViewProjectionInvMatrix(viewProjectionMatrix),
		};
	}

	public getViewProjectionMatrix(): Float32Array {
		const { height: screenHeight, width: screenWidth } = this.screenDimensions;

		const cameraTarget = mat4.add(
			this.eye,
			[0, 0, 1] /* only look long z-axis */,
		);
		const camMatrix = mat4.lookAt(
			this.eye,
			cameraTarget,
			cameraConfig.cameraUp,
		);
		const viewMatrix = mat4.inverse(camMatrix);

		const projectMatrix = mat4.perspective(
			(cameraConfig.cameraFOV * Math.PI) / 180,
			screenWidth / screenHeight,
			0.1,
			100,
		);

		return mat4.multiply(projectMatrix, viewMatrix);
	}

	public getViewProjectionInvMatrix(
		viewProjectionMatrix?: Float32Array,
	): Float32Array {
		return mat4.inverse(viewProjectionMatrix ?? this.getViewProjectionMatrix());
	}

	public getMouseWorldPosition(screenSpaceMousePosition: Position): Position {
		const { height: screenHeight, width: screenWidth } = this.screenDimensions;

		const normalizedPosition = {
			x: (2 * (screenSpaceMousePosition.x - screenWidth / 2)) / screenWidth,
			y: (2 * (-screenSpaceMousePosition.y + screenHeight / 2)) / screenHeight,
		};
		const worldPosition = mat4.multiply(
			this.getViewProjectionInvMatrix(),
			vec4.set(normalizedPosition.x, normalizedPosition.y, 0, 1),
		);

		const normWorldPosition = new Float32Array([
			worldPosition[0] / worldPosition[3],
			worldPosition[1] / worldPosition[3],
			worldPosition[2] / worldPosition[3],
		]);

		const cameraEye = new Float32Array(this.eye);
		cameraEye[1] *= -1;

		const dir = vec3.add(normWorldPosition, vec3.negate(cameraEye));
		const zScale = -normWorldPosition[2] / dir[2];

		return {
			x: zScale * dir[0] + normWorldPosition[0],
			y: zScale * dir[1] + normWorldPosition[1],
		};
	}

	public onResize(width: number, height: number): void {
		this.screenDimensions = { width: width, height: height };
	}

	public onKeyboardEvent(
		event: KeyboardButtonType,
		_nature: ButtonEvent,
	): boolean {
		const velocityDelta = cameraConfig.keyToVelocityDeltaMap.get(event);

		if (!velocityDelta) {
			return false;
		}

		this.setVelocity(velocityDelta);

		return true;
	}

	public onMouseInputEvent(event: "scrollUp" | "scrollDown"): boolean {
		switch (event) {
			case "scrollUp": {
				this.zoomIn();
				return true;
			}
			case "scrollDown": {
				this.zoomOut();
				return true;
			}
			default:
				return false;
		}
	}

	public computeScreenSpacePosition(chipWorldPosition: Position): Position {
		const { width: screenWidth, height: screenHeight } = this.screenDimensions;

		const { x: chipWorldX, y: chipWorldY } = chipWorldPosition;

		// 1. Transform world → clip space
		const clip = mat4.multiply(
			this.getViewProjectionMatrix(),
			vec4.set(chipWorldX, chipWorldY, 0, 1),
		);

		// 2. Perspective divide (clip → NDC)
		const ndcX = clip[0] / clip[3];
		const ndcY = clip[1] / clip[3];

		// 3. NDC → screen space
		const screenX = (ndcX * 0.5 + 0.5) * screenWidth;
		const screenY = (-ndcY * 0.5 + 0.5) * screenHeight;

		return {
			x: screenX,
			y: screenY,
		};
	}

	private setVelocity(velocityDelta: Vec3Arg): void {
		vec3.add(this.inputVelocity, velocityDelta, this.inputVelocity);
	}

	private zoomIn(): void {
		vec3.add(this.inputVelocity, [0, 0, 1], this.inputVelocity);
	}

	private zoomOut(): void {
		vec3.add(this.inputVelocity, [0, 0, -1], this.inputVelocity);
	}
}

const clamp = (value: number, min: number, max: number): number =>
	Math.min(Math.max(value, min), max);
