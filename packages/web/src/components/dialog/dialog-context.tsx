import * as React from "react";
import type { PanelKind } from "./dialog-panel-props";

type IDialogContext = {
  openDialog: (kind: PanelKind) => void;
  closeDialog: () => void;
  currentPanel: PanelKind | null;
};

const DialogContext = React.createContext<IDialogContext>({} as IDialogContext);

export const DialogProvider = (props: React.PropsWithChildren) => {
  const [currentPanel, setCurrentPanel] = React.useState<PanelKind | null>(
    null
  );

  const openDialog = React.useCallback(
    (kind: PanelKind) => setCurrentPanel(kind),
    []
  );

  const closeDialog = React.useCallback(() => setCurrentPanel(null), []);

  return (
    <DialogContext.Provider
      value={{
        openDialog,
        closeDialog,
        currentPanel,
      }}
    >
      {props.children}
    </DialogContext.Provider>
  );
};

export const useDialog = () => React.useContext(DialogContext);
