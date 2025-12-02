export type ButtonEvent = "click" | "press" | "release";

type InputType = MouseButtonType | MouseScrollType | KeyboardButtonType;

export type InputEventTopic<TInputType extends InputType> = {
	callback: (event: TInputType, nature: ButtonEvent) => void;
	nature: ButtonEvent;
};

export type InputManagerState<TInputType extends InputType> = Record<
	TInputType,
	{
		pollCount: number;
		isDown: boolean;
		subscribers: Set<InputEventTopic<TInputType>>;
	}
>;

export const KeyboardButton = ["w", "a", "s", "d", "g"] as const;
export type KeyboardButtonType = ArrayToUnion<typeof KeyboardButton>;

export const MouseButton = [
	"leftMouseButton",
	"rightMouseButton",
	"middleMouseButton",
] as const;
export type MouseButtonType = ArrayToUnion<typeof MouseButton>;

export const MouseScroll = ["scrollUp", "scrollDown"] as const;
export type MouseScrollType = ArrayToUnion<typeof MouseScroll>;

export const mouseButtonMap: Record<MouseEvent["button"], MouseButtonType> = {
	0: "leftMouseButton",
	1: "middleMouseButton",
	2: "rightMouseButton",
};

/**
 * Converts a readonly string array to a string union type.
 * Example:
 * const array = ["apple", "orange"] as const;
 * ArrayToUnion<typeof array> => "apple" | "orange"
 */
type ArrayToUnion<TArray extends readonly string[]> = TArray[number];
