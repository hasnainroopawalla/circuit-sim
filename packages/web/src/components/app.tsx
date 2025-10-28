import styles from "./app.module.css";
import { NotificationBanner, useNotification } from "./notification";
import { SimulatorProvider } from "./simulator-context";
import { Sketch } from "./sketch-renderer";
import { Toolbar } from "./toolbar";

export const App = () => {
	const notificationText = useNotification();

	return (
		<SimulatorProvider>
			<div className={styles.app}>
				<Sketch />
				<Toolbar />
				<NotificationBanner getText={() => notificationText} />
			</div>
		</SimulatorProvider>
	);
};
