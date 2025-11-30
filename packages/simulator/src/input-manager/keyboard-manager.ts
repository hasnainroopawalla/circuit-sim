import { ObjectKeys } from "../utils";
import {
	type ButtonEvent,
	type InputEventTopic,
	type InputManagerState,
	type KeyboardButtonType,
	KeyboardButton,
} from "./input-manager.interface";

export class KeyboardManager {
	private keyboardButtonState: InputManagerState<KeyboardButtonType> =
		KeyboardButton.reduce(
			(accumulator, entityType) => ({
				...accumulator,
				[entityType]: { pollCount: 0, isDown: false, subscribers: new Set() },
			}),
			{},
		) as InputManagerState<KeyboardButtonType>;

	constructor(canvas: HTMLCanvasElement) {
		canvas.addEventListener("keydown", (e) => this.keyDownEventHandler(e));
		canvas.addEventListener("keyup", (e) => this.keyUpEventHandler(e));
	}

	public onButtonHandler(
		event: KeyboardButtonType,
		nature: ButtonEvent,
		callback: InputEventTopic<KeyboardButtonType>["callback"],
	): void {
		this.keyboardButtonState[event].subscribers.add({ callback, nature });
	}

	public update(_deltaTime: number): void {
		this.keyboardButtonUpdate();
	}

	private keyboardButtonUpdate(): void {
		ObjectKeys(this.keyboardButtonState).forEach((keyboardButton) => {
			if (this.keyboardButtonState[keyboardButton].isDown) {
				this.keyboardButtonState[keyboardButton].pollCount += 1;
			}

			this.keyboardButtonState[keyboardButton].subscribers.forEach(
				(subscriberTopic) => {
					this.performButtonCallback(keyboardButton, subscriberTopic);
				},
			);
		});
	}

	private performButtonCallback(
		keyboardButton: KeyboardButtonType,
		topic: InputEventTopic<KeyboardButtonType>,
	) {
		switch (topic.nature) {
			case "click":
				this.keyboardButtonState[keyboardButton].pollCount === 1 &&
					topic.callback(keyboardButton, topic.nature);
				break;
			case "press":
				this.keyboardButtonState[keyboardButton].pollCount > 1 &&
					topic.callback(keyboardButton, topic.nature);
				break;
			case "release":
				this.keyboardButtonState[keyboardButton].pollCount === -1 &&
					topic.callback(keyboardButton, topic.nature);
		}
	}

	private keyDownEventHandler(event: KeyboardEvent) {
		if (!this.isKeyEventAllowed(event)) {
			return;
		}
		this.keyboardButtonState[event.key as KeyboardButtonType].pollCount = 0;
		this.keyboardButtonState[event.key as KeyboardButtonType].isDown = true;
	}

	private keyUpEventHandler(event: KeyboardEvent) {
		if (!this.isKeyEventAllowed(event)) {
			return;
		}
		this.keyboardButtonState[event.key as KeyboardButtonType].pollCount = 0;
		this.keyboardButtonState[event.key as KeyboardButtonType].isDown = false;
	}

	private isKeyEventAllowed(event: KeyboardEvent): boolean {
		return event.key in this.keyboardButtonState;
	}
}
