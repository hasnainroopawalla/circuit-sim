import React from "react";
import { EmitterEvent, EmitterEventArgs, emitter } from "../../event-service";

export function useEventListener<T extends EmitterEvent>(
  event: T
): EmitterEventArgs[T] | undefined {
  const [eventData, setEventData] = React.useState<EmitterEventArgs[T]>();

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
