import React from "react";
import { useNotification } from "./use-notification";

const styles = {
  toolbarContainer: {
    color: "#fff",
    position: "absolute",
    top: "0",
    right: "0",
    margin: "5px 10px",
    display: "flex",
    flexDirection: "row",
    gap: "10px",
  },
} as const;

export const NotificationBanner = () => {
  const error = useNotification(); // TODO: This should come as a prop

  return (
    <div className="notification-banner" style={styles.toolbarContainer}>
      <p>{error.message}</p>
    </div>
  );
};
