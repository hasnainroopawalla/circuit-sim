import { type EventData, type EventKey, pubsub } from "@circuit-sim/pubsub";
import * as React from "react";

export function useEventListener<T extends EventKey>(
	event: T,
): EventData[T] | undefined {
	const [eventData, setEventData] = React.useState<EventData[T]>();

	React.useEffect(() => {
		pubsub.subscribe(event, (error) => {
			setEventData(error);
		});

		return () => {
			pubsub.unsubscribe(event, () => {});
		};
	}, [event]);

	return eventData;
}
