import type { Position } from "@digital-logic-sim/shared-types";
import {
	type MouseButtonType,
	type MouseScrollType,
	type KeyboardButtonType,
	type InputEventTopic,
	type MouseMoveEventCallback,
	ButtonEvent,
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
				{ event: "leftMouseButton", nature: ButtonEvent.Click },
				{ event: "leftMouseButton", nature: ButtonEvent.Press },
				{ event: "rightMouseButton", nature: ButtonEvent.Click },
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
				{ event: "w", nature: ButtonEvent.Press },
				{ event: "a", nature: ButtonEvent.Press },
				{ event: "s", nature: ButtonEvent.Press },
				{ event: "d", nature: ButtonEvent.Press },
				{ event: "Escape", nature: ButtonEvent.Press },
			] as const
		).forEach(({ event, nature }) => {
			this.keyboardManager.onButtonHandler(event, nature, callback);
		});
	}
}
