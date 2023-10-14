import React from "react";
import { Sketch } from "./sketch-renderer";
import { Toolbar } from "./toolbar";
import { NotificationBanner } from "./notification-banner";
import styles from "./app.module.css";

export const App = () => {
  return (
    <div className={styles.app}>
      <Sketch />
      <Toolbar />
      <NotificationBanner />
    </div>
  );
};
