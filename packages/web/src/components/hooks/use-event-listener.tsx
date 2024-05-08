import * as React from "react";
import { EventKey, EventData, pubsub } from "@circuit-sim/pubsub";

export function useEventListener<T extends EventKey>(
  event: T
): EventData[T] | undefined {
  const [eventData, setEventData] = React.useState<EventData[T]>();

  React.useEffect(() => {
    pubsub.subscribe(event, (error) => {
      setEventData(error);
    });

    return () => {
      pubsub.unsubscribe(event, () => {});
    };
  }, []);

  return eventData;
}
