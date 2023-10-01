import React from "react";
import { Sketch } from "./sketch-renderer";
import { Toolbar } from "./toolbar";
import { NotificationBanner } from "./notification-banner";

export const App = () => {
  return (
    <>
      <Sketch />
      <Toolbar />
      <NotificationBanner />
    </>
  );
};
