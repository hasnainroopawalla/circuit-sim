import type { Position } from "@digital-logic-sim/render-engine";
import type {
	MouseButtonType,
	MouseScrollType,
	KeyboardButtonType,
	InputEventTopic,
	MouseMoveEventCallback,
} from "./input-manager.interface";
import { KeyboardManager } from "./keyboard-manager";
import { MouseManager } from "./mouse-manager";

type InputManagerArgs = {
	canvas: HTMLCanvasElement;
};

export class InputManager {
	private keyboardManager: KeyboardManager;
	private mouseManager: MouseManager;

	constructor(args: InputManagerArgs) {
		this.keyboardManager = new KeyboardManager(args.canvas);
		this.mouseManager = new MouseManager(args.canvas);
	}

	public destroy(): void {
		this.mouseManager.destroy();
		this.keyboardManager.destroy();
	}

	public update(deltaTime: number): void {
		this.mouseManager.update(deltaTime);
		this.keyboardManager.update(deltaTime);
	}

	public getMousePosition(): Position {
		return this.mouseManager.getMousePosition();
	}

	public onMouseButtonEvent(
		callback: InputEventTopic<MouseButtonType>["callback"],
	): void {
		(
			[
				{ event: "leftMouseButton", nature: "click" },
				{ event: "leftMouseButton", nature: "press" },
			] as const
		).forEach(({ event, nature }) => {
			this.mouseManager.onButtonHandler(event, nature, callback);
		});
	}

	public onMouseScrollEvent(
		callback: InputEventTopic<MouseScrollType>["callback"],
	): void {
		([{ event: "scrollUp" }, { event: "scrollDown" }] as const).forEach(
			({ event }) => {
				this.mouseManager.onScrollHandler(event, callback);
			},
		);
	}

	public onMouseMoveEvent(callback: MouseMoveEventCallback): void {
		this.mouseManager.onMoveHandler(callback);
	}

	public onKeyboardEvent(
		callback: InputEventTopic<KeyboardButtonType>["callback"],
	): void {
		(
			[
				{ event: "w", nature: "press" },
				{ event: "a", nature: "press" },
				{ event: "s", nature: "press" },
				{ event: "d", nature: "press" },
				{ event: "Escape", nature: "press" },
			] as const
		).forEach(({ event, nature }) => {
			this.keyboardManager.onButtonHandler(event, nature, callback);
		});
	}
}
