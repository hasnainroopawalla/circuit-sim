import type { Position } from "@digital-logic-sim/render-engine";
import {
	type MouseButtonType,
	type MouseScrollType,
	type ButtonEvent,
	type InputManagerState,
	type InputEventTopic,
	mouseButtonMap,
	MouseButton,
} from "./input-manager.interface";
import { ObjectKeys } from "../utils";

export class MouseManager {
	private scrollDirection: "up" | "down" | null;

	private mousePosition: Position;

	private timeoutId: number | undefined;

	private mouseButtonState: InputManagerState<MouseButtonType> =
		MouseButton.reduce<InputManagerState<MouseButtonType>>(
			(acc, entityType) => {
				acc[entityType] = {
					pollCount: 0,
					isDown: false,
					subscribers: new Set(),
				};
				return acc;
			},
			{} as InputManagerState<MouseButtonType>,
		);

	private scrollSubscriberMap: Record<
		MouseScrollType,
		Set<InputEventTopic<MouseScrollType>["callback"]>
	> = {
		scrollUp: new Set(),
		scrollDown: new Set(),
	};

	private abortController: AbortController;

	constructor(canvas: HTMLCanvasElement) {
		this.scrollDirection = null;
		this.mousePosition = { x: 0, y: 0 };

		this.abortController = new AbortController();

		this.registerSubscriptions(canvas);
	}

	public destroy(): void {
		this.abortController.abort();
	}

	public onButtonHandler(
		event: MouseButtonType,
		nature: ButtonEvent,
		callback: InputEventTopic<MouseButtonType>["callback"],
	): void {
		this.mouseButtonState[event].subscribers.add({ callback, nature });
	}

	public onScrollHandler(
		event: MouseScrollType,
		callback: InputEventTopic<MouseScrollType>["callback"],
	): void {
		this.scrollSubscriberMap[event].add(callback);
	}

	public update(_deltaTime: number): void {
		this.mouseButtonUpdate();
		this.mouseScrollUpdate();
	}

	public getMousePosition(): MouseManager["mousePosition"] {
		return this.mousePosition;
	}

	public isScrollingUp(): boolean {
		return this.scrollDirection === "up";
	}

	public isScrollingDown(): boolean {
		return this.scrollDirection === "down";
	}

	private setScrolling(e: WheelEvent): void {
		if (this.timeoutId) {
			clearTimeout(this.timeoutId);
		}

		this.timeoutId = setTimeout(() => {
			this.scrollDirection = null;
		}, 10);

		if (e.deltaY < 0) {
			this.scrollDirection = "up";
		} else if (e.deltaY > 0) {
			this.scrollDirection = "down";
		}
	}

	private mouseScrollUpdate(): void {
		this.scrollDirection === "up" &&
			this.scrollSubscriberMap.scrollUp.forEach((callback) => {
				callback("scrollUp", "click");
			});
		this.scrollDirection === "down" &&
			this.scrollSubscriberMap.scrollDown.forEach((callback) => {
				callback("scrollDown", "click");
			});
	}

	private mouseButtonUpdate(): void {
		ObjectKeys(this.mouseButtonState).forEach((mouseButton) => {
			if (this.mouseButtonState[mouseButton].isDown) {
				this.mouseButtonState[mouseButton].pollCount += 1;
			}

			this.mouseButtonState[mouseButton].subscribers.forEach(
				(subscriberTopic) => {
					this.performButtonCallback(mouseButton, subscriberTopic);
				},
			);
		});
	}

	private performButtonCallback(
		mouseButton: MouseButtonType,
		topic: InputEventTopic<MouseButtonType>,
	) {
		switch (topic.nature) {
			case "click":
				this.mouseButtonState[mouseButton].pollCount === 1 &&
					topic.callback(mouseButton, topic.nature);
				break;

			case "press":
				this.mouseButtonState[mouseButton].pollCount > 1 &&
					topic.callback(mouseButton, topic.nature);
				break;
		}
	}

	private setMousePosition(e: MouseEvent, canvas: HTMLCanvasElement) {
		const rect = canvas.getBoundingClientRect();

		const scaleX = canvas.width / rect.width;
		const scaleY = canvas.height / rect.height;

		this.mousePosition = {
			x: (e.clientX - rect.left) * scaleX,
			y: (e.clientY - rect.top) * scaleY,
		};
	}

	private mouseDownEventHandler(event: MouseEvent): void {
		if (!this.isMouseEventAllowed(event)) {
			return;
		}
		this.mouseButtonState[mouseButtonMap[event.button]].pollCount = 0;
		this.mouseButtonState[mouseButtonMap[event.button]].isDown = true;
	}

	private mouseUpEventHandler(event: MouseEvent): void {
		if (!this.isMouseEventAllowed(event)) {
			return;
		}
		this.mouseButtonState[mouseButtonMap[event.button]].isDown = false;
		this.mouseButtonState[mouseButtonMap[event.button]].pollCount = 0;
	}

	private isMouseEventAllowed(event: MouseEvent): boolean {
		return mouseButtonMap[event.button] in this.mouseButtonState;
	}

	private registerSubscriptions(canvas: HTMLCanvasElement): void {
		const signal = this.abortController.signal;

		canvas.addEventListener("wheel", (e) => this.setScrolling(e), { signal });
		canvas.addEventListener(
			"mousemove",
			(e) => this.setMousePosition(e, canvas),
			{ signal },
		);
		canvas.addEventListener("mousedown", (e) => this.mouseDownEventHandler(e), {
			signal,
		});
		canvas.addEventListener("mouseup", (e) => this.mouseUpEventHandler(e), {
			signal,
		});
	}
}
