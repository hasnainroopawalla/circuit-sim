import * as React from "react";
import styles from "./menu-panel.module.css";
import { useDialog } from "../../dialog";
import { Button } from "../../factory";
import {
  SaveIcon,
  ImportIcon,
  // OptionsIcon,
  type IconType,
  QuestionIcon,
} from "../../icons";

export const Menu: React.FC = () => {
  const [isMenuOpen, setIsMenuOpen] = React.useState(false);

  const { openDialog } = useDialog();

  const showSaveCircuitDialog = React.useCallback(() => {
    openDialog("saveCircuit");
    setIsMenuOpen(false);
  }, [openDialog]);

  const showImportChipDialog = React.useCallback(() => {
    openDialog("importChip");
    setIsMenuOpen(false);
  }, [openDialog]);

  return (
    <>
      <Button
        text="MENU"
        appearance="primary"
        size="large"
        onClick={() => setIsMenuOpen(!isMenuOpen)}
      />
      {isMenuOpen && (
        <div className={styles.menuPanel}>
          <MenuItem
            text="SAVE CHIP"
            Icon={SaveIcon}
            onClick={showSaveCircuitDialog}
          />
          <MenuItem
            text="IMPORT CHIP"
            Icon={ImportIcon}
            onClick={showImportChipDialog}
          />
          {/* <MenuItem
            text="OPTIONS"
            Icon={OptionsIcon}
            onClick={() => alert("In progress..")}
          /> */}
          <MenuItem
            text="HELP"
            Icon={QuestionIcon}
            onClick={() =>
              // eslint-disable-next-line no-alert
              alert(
                `https://github.com/hasnainroopawalla/circuit-sim\n\nv${APP_VERSION}`
              )
            }
          />
        </div>
      )}
    </>
  );
};

type MenuItemProps = {
  text: string;
  Icon: IconType;
  onClick: () => void;
};

const MenuItem: React.FC<MenuItemProps> = (props) => (
  <div className={styles.menuItemContainer} onClick={props.onClick}>
    {<props.Icon />} <span>{props.text.toUpperCase()}</span>
  </div>
);
