const cameraConfig = {
	fov: 60,
	cameraUp: [0, 1, 0],
	speed: 2,
	zoomSpeed: 5,
	keyToVelocityDeltaMap: {
		w: [0, -1, 0],
		s: [0, 1, 0],
		a: [1, 0, 0],
		d: [-1, 0, 0],
	},
};

type CameraProps = {
	width: number;
	height: number;
};

export class Camera {
	private width: number;
	private height: number;

	private eye: Float32Array;
	private target: Float32Array;

	private viewProjMatrix: Float32Array;
	private viewProjInvMatrix: Float32Array;

	private inputVelocity: Float32Array;

	constructor(props: CameraProps) {
		this.width = props.width;
		this.height = props.height;
		this.eye = new Float32Array([0, 0, -5]);
		this.target = new Float32Array([0, 0, 0]);
		this.viewProjMatrix = this.viewProjectionMatrix();
		this.viewProjInvMatrix = mat4.inverse(this.viewProjMatrix);
		this.inputVelocity = new Float32Array([0, 0, 0]);

		this.registerSubscriptions();
	}

	public update(deltaTime: number): void {
		const cameraVelocity = new Float32Array([
			this.inputVelocity[0] * cameraConfig.speed,
			this.inputVelocity[1] * cameraConfig.speed,
			this.inputVelocity[2] * cameraConfig.zoomSpeed,
		]);

		this.eye = vec3.add(this.eye, vec3.scale(cameraVelocity, deltaTime));
		this.eye[2] = clamp(this.eye[2], -10, -1);

		// render engine
		this.target = vec3.add(this.target, vec3.scale(cameraVelocity, deltaTime));
		this.viewProjMatrix = this.viewProjectionMatrix();
		this.viewProjInvMatrix = mat4.inverse(this.viewProjMatrix);
		// render engine

		this.inputVelocity.set([0, 0, 0]);
	}

	private registerSubscriptions(): void {
		this.inputManager.onKeyboardEvent("w", "press", () =>
			this.setVelocity("w"),
		);
		this.inputManager.onKeyboardEvent("a", "press", () =>
			this.setVelocity("a"),
		);
		this.inputManager.onKeyboardEvent("s", "press", () =>
			this.setVelocity("s"),
		);
		this.inputManager.onKeyboardEvent("d", "press", () =>
			this.setVelocity("d"),
		);

		this.inputManager.onMouseScrollEvent("scrollUp", () => this.zoomIn());
		this.inputManager.onMouseScrollEvent("scrollDown", () => this.zoomOut());
	}

	private setVelocity(key: string): void {
		vec3.add(
			this.inputVelocity,
			cameraConfig.keyToVelocityDeltaMap[key as "w" | "a" | "s" | "d"],
			this.inputVelocity,
		);
	}

	private zoomIn(): void {
		vec3.add(this.inputVelocity, [0, 0, 1], this.inputVelocity);
	}

	private zoomOut(): void {
		vec3.add(this.inputVelocity, [0, 0, -1], this.inputVelocity);
	}
}
