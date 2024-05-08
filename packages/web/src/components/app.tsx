import * as React from "react";
import { Sketch } from "./sketch-renderer";
import { Toolbar } from "./toolbar/toolbar";
import { NotificationBanner } from "./toolbar/notification-banner";
import styles from "./app.module.css";
import { useNotification } from "./hooks";

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
