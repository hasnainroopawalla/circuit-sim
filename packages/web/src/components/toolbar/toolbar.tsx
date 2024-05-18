import * as React from "react";
import { DialogProvider, Dialog } from "../dialog";
import { ButtonsPanel } from "./buttons-panel";

export const Toolbar = () => (
  <>
    <DialogProvider>
      <ButtonsPanel />
      <Dialog />
    </DialogProvider>
  </>
);
