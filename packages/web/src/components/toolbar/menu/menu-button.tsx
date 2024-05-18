import * as React from "react";
import { useDialog } from "../../dialog";
import { Button } from "../../factory";

export const MenuButton: React.FC = () => {
  const { openDialog } = useDialog();

  const showSaveCircuitDialog = React.useCallback(
    () => openDialog("saveCircuit"),
    [openDialog]
  );

  return (
    <Button
      text="MENU"
      appearance="primary"
      size="large"
      onClick={showSaveCircuitDialog}
    />
  );
};
