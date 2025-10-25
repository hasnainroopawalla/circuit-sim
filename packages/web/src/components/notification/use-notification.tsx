import * as React from "react";
import { useEventListener } from "../hooks";

export const useNotification = () => {
	const [notification, setNotification] = React.useState("");

	const notificationEvent = useEventListener("Notification");

	React.useEffect(() => {
		if (!notificationEvent) {
			return;
		}

		setNotification(notificationEvent.text);
	}, [notificationEvent]);

	return notification;
};
