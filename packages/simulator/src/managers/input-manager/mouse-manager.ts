import type { Position } from "@digital-logic-sim/shared-types";
import {
	ButtonEvent,
	type MouseButtonType,
	type MouseScrollType,
	type InputManagerState,
	type InputEventTopic,
	type MouseMoveEventCallback,
	mouseButtonMap,
	MouseButton,
} from "./input-manager.interface";
import { ObjectKeys } from "../../utils";

export class MouseManager {
	private scrollDirection: "up" | "down" | null;

	private mousePosition: Position;

	private timeoutId: ReturnType<typeof setTimeout> | undefined;

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

	private mouseMoveEventSubscribers = new Set<MouseMoveEventCallback>();

	private abortController: AbortController;

	constructor(canvas: HTMLCanvasElement) {
		this.scrollDirection = null;
		this.mousePosition = { x: 0, y: 0 };

		this.abortController = new AbortController();

		this.init(canvas);
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

	public onMoveHandler(callback: MouseMoveEventCallback): void {
		this.mouseMoveEventSubscribers.add(callback);
	}

	public update(_deltaTime: number): void {
		this.mouseButtonUpdate();
		this.mouseScrollUpdate();
	}

	public getMousePosition(): MouseManager["mousePosition"] {
		return this.mousePosition;
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
				callback("scrollUp", ButtonEvent.Click);
			});
		this.scrollDirection === "down" &&
			this.scrollSubscriberMap.scrollDown.forEach((callback) => {
				callback("scrollDown", ButtonEvent.Click);
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
			case ButtonEvent.Click:
				this.mouseButtonState[mouseButton].pollCount === 1 &&
					topic.callback(mouseButton, topic.nature);
				break;

			case ButtonEvent.Press:
				this.mouseButtonState[mouseButton].pollCount > 2 &&
					topic.callback(mouseButton, topic.nature);
				break;
		}
	}

	private setMousePosition(e: MouseEvent, canvas: HTMLCanvasElement): void {
		const rect = canvas.getBoundingClientRect();

		const scaleX = canvas.width / rect.width;
		const scaleY = canvas.height / rect.height;

		this.mousePosition = {
			x: (e.clientX - rect.left) * scaleX,
			y: (e.clientY - rect.top) * scaleY,
		};

		this.mouseMoveEventSubscribers.forEach((callback) => {
			callback(this.mousePosition);
		});
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
		this.mouseButtonState[mouseButtonMap[event.button]].pollCount = 0;
		this.mouseButtonState[mouseButtonMap[event.button]].isDown = false;
	}

	private isMouseEventAllowed(event: MouseEvent): boolean {
		return mouseButtonMap[event.button] in this.mouseButtonState;
	}

	private init(canvas: HTMLCanvasElement): void {
		const signal = this.abortController.signal;

		canvas.addEventListener("wheel", (e) => this.setScrolling(e), { signal });

		// `document` listener is intentionally added here to ensure pointer movements
		// are captured even if a UI menu is overlapping the canvas
		document.addEventListener(
			"pointermove",
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
