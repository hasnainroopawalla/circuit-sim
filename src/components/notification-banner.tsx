import React from "react";
import { useEventListener } from "./hooks";
import { EmitterEvent } from "../event-service";

// TODO: Create CSS module
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
    MozUserSelect: "none" /* firefox */,
    WebkitUserSelect: "none" /* Safari */,
    msUserSelect: "none" /* IE*/,
    userSelect: "none",
  },
} as const;

export const NotificationBanner = () => {
  const notification = useEventListener(EmitterEvent.Notification); // TODO: This should be passed as a prop

  return (
    <div className="notification-banner" style={styles.toolbarContainer}>
      <p>{notification && notification.message}</p>
    </div>
  );
};
