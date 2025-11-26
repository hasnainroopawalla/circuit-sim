import type { CameraEntity } from "@digital-logic-sim/render-engine";
import { vec3, type Vec3Arg } from "wgpu-matrix";

type VelocityDelta = [number, number, number];

const cameraConfig = {
	speed: 2,
	zoomSpeed: 5,
	keyToVelocityDeltaMap: new Map<KeyboardEvent["key"], VelocityDelta>([
		["w", [0, -1, 0]],
		["s", [0, 1, 0]],
		["a", [1, 0, 0]],
		["d", [-1, 0, 0]],
	]),
};

export class Camera {
	private eye: Float32Array;

	private inputVelocity: Float32Array;

	constructor() {
		this.eye = new Float32Array([0, 0, -5]);
		this.inputVelocity = new Float32Array([0, 0, 0]);
	}

	public update(deltaTime: number): void {
		const cameraVelocity = new Float32Array([
			this.inputVelocity[0] * cameraConfig.speed,
			this.inputVelocity[1] * cameraConfig.speed,
			this.inputVelocity[2] * cameraConfig.zoomSpeed,
		]);

		this.eye = vec3.add(this.eye, vec3.scale(cameraVelocity, deltaTime));
		this.eye[2] = clamp(this.eye[2], -10, -1);

		// TODO: required?
		this.inputVelocity.set([0, 0, 0]);
	}

	public getPosition(): CameraEntity {
		return {
			eye: this.eye,
		};
	}

	public onKeyboardInputEvent(event: KeyboardEvent): boolean {
		const velocityDelta = cameraConfig.keyToVelocityDeltaMap.get(event.key);

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
