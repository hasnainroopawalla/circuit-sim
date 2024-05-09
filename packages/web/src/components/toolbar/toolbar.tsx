import * as React from "react";
import { MenuPanel } from "./menu-panel";
import { DialogProvider, Dialog } from "../dialog";
import { ChipsPanel } from "./chips-panel";

export const Toolbar = () => (
  <>
    <DialogProvider>
      <MenuPanel />
      <ChipsPanel />
      <Dialog />
    </DialogProvider>
  </>
);
