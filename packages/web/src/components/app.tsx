import styles from "./app.module.css";
import { NotificationBanner, useNotification } from "./notification";
import { Sketch } from "./sketch-renderer";
import { Toolbar } from "./toolbar";

export const App = () => {
	const notificationText = useNotification();

	return (
		<div className={styles.app}>
			<Sketch />
			<Toolbar />
			<NotificationBanner getText={() => notificationText} />
		</div>
	);
};
