import * as React from "react";
import { Sketch } from "./sketch-renderer";
import { Toolbar } from "./toolbar";
import { NotificationBanner, useNotification } from "./notification";
import styles from "./app.module.css";

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
