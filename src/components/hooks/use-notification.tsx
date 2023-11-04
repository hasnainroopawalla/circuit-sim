import * as React from "react";
import { EmitterEvent } from "../../event-service";
import { useEventListener } from "./use-event-listener";

export const useNotification = () => {
  const [notification, setNotification] = React.useState("");

  const notificationEvent = useEventListener(EmitterEvent.Notification);

  React.useEffect(() => {
    if (!notificationEvent) {
      return;
    }

    setNotification(notificationEvent.text);
  }, [notificationEvent]);

  return notification;
};
