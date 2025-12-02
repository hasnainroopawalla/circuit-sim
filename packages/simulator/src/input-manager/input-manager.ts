import type { Position } from "@digital-logic-sim/render-engine";
import type {
	MouseButtonType,
	ButtonEvent,
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
		event: MouseButtonType,
		nature: ButtonEvent,
		callback: InputEventTopic<MouseButtonType>["callback"],
	): void {
		this.mouseManager.onButtonHandler(event, nature, callback);
	}

	public onMouseScrollEvent(
		event: MouseScrollType,
		callback: InputEventTopic<MouseScrollType>["callback"],
	): void {
		this.mouseManager.onScrollHandler(event, callback);
	}

	public onMouseMoveEvent(callback: MouseMoveEventCallback): void {
		this.mouseManager.onMoveHandler(callback);
	}

	public onKeyboardEvent(
		event: KeyboardButtonType,
		nature: ButtonEvent,
		callback: InputEventTopic<KeyboardButtonType>["callback"],
	): void {
		this.keyboardManager.onButtonHandler(event, nature, callback);
	}
}
