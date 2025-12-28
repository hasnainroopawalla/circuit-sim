import type { Pin, PinType } from "../entities/pin";
import type { ChipDefinition } from "./chip-library-service";

export type IEvents = {
	"sim.save-chip": undefined;
	"sim.reset": undefined;

	"wire.spawn.start": { startPin: Pin };
	"chip.spawn.start": { chipDefinition: ChipDefinition };

	"chip.spawn.finish": {
		chipId: string;
		chipName: string;
		pins: { id: string; name: string; pinType: PinType }[];
	};
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
