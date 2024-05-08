import * as React from "react";
import { EventKey, EventData, emitter } from "@circuit-sim/events";

export function useEventListener<T extends EventKey>(
  event: T
): EventData[T] | undefined {
  const [eventData, setEventData] = React.useState<EventData[T]>();

  React.useEffect(() => {
    emitter.on(event, (error) => {
      setEventData(error);
    });

    return () => {
      emitter.off(event, () => {});
    };
  }, []);

  return eventData;
}
