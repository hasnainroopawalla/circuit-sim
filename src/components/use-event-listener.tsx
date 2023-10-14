import { useEffect, useState } from "react";
import { EmitterEvent, EmitterEventArgs, emitter } from "../event-service";

export function useEventListener<T extends EmitterEvent>(
  event: T
): EmitterEventArgs[T] | undefined {
  const [eventData, setEventData] = useState<EmitterEventArgs[T]>();

  useEffect(() => {
    emitter.on(event, (error) => {
      setEventData(error);
    });

    return () => {
      emitter.off(event, () => {});
    };
  }, []);

  return eventData;
}
