import * as React from "react";
import styles from "./notification-banner.module.css";

const NOTIFICATION_BANNER_TIMEOUT = 4000;

type NotificationBannerProps = {
	getText: () => string; // TODO: no need for callback
};

export const NotificationBanner: React.FC<NotificationBannerProps> = (
	props,
) => {
	const [isVisible, setIsVisible] = React.useState(false);
	const { getText } = props;

	React.useEffect(() => {
		setIsVisible(true);
		const timeout = setTimeout(
			() => setIsVisible(false),
			NOTIFICATION_BANNER_TIMEOUT,
		);
		return () => clearTimeout(timeout);
	}, []);

	return (
		isVisible &&
		getText() && (
			<div className={styles.banner}>
				<div className={styles.content}>{getText()}</div>
			</div>
		)
	);
};
