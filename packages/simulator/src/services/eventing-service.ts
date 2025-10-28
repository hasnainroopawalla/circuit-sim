import type { ChipSpec } from "../entities-new/chip";

export type IEvents = {
	// A spawn chip button is pressed
	"chip.spawn": ChipSpec;

	// New circuit chip is imported from a blueprint
	ImportChip: {
		chipName: string;
		blueprint: string;
	};

	// Save circuit button is pressed
	SaveCircuit: { name: string; color?: string };

	// Circuit chip blueprint string generated
	AddCircuitChipToToolbar: {
		name: string;
		blueprint: string;
	};

	// Notification message to be displayed to the user
	Notification: { text: string }; // TODO: Add error type, reason, etc.
};

export type Unsubscribe = () => void;

type Subscriptions<Data> = {
	[K in keyof Data]: ((data: Data[K]) => void)[];
};

export class EventingService {
	private readonly subscriptions: Partial<Subscriptions<IEvents>>;

	constructor() {
		this.subscriptions = {};
	}

	public subscribe<K extends keyof IEvents>(
		event: K,
		handler: (data: IEvents[K]) => void,
	): Unsubscribe {
		if (!this.subscriptions[event]) {
			this.subscriptions[event] = [];
		}

		this.subscriptions[event].push(handler);

		return () => {
			// unsubscribe
			const handlers = this.subscriptions[event];
			if (!handlers) {
				return;
			}

			const index = handlers.indexOf(handler);
			if (index !== -1) {
				handlers.splice(index, 1);
			}
		};
	}

	public publish<K extends keyof IEvents>(event: K, data: IEvents[K]): void {
		this.subscriptions[event]?.forEach((handler) => {
			handler(data);
		});
	}
}
