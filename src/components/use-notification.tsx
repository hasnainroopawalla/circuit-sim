import { useEffect, useState } from "react";
import { EmitterEvent, EmitterEventArgs, emitter } from "../event-service";

export const useNotification = () => {
  const [notification, setNotification] = useState<
    EmitterEventArgs[EmitterEvent.Notification]
  >({
    message: "",
  });

  useEffect(() => {
    emitter.on(EmitterEvent.Notification, (error) => {
      setNotification(error);
    });

    return () => {
      emitter.off(EmitterEvent.Notification, () => {});
    };
  }, []);

  return notification;
};
