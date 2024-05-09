import * as React from "react";
import styles from "./menu-panel.module.css";
import { FaSave as SaveIcon, FaCog as OptionsIcon } from "react-icons/fa";
import type { IconType } from "react-icons";

export const MenuPanel: React.FC = () => (
  <div className={styles.menuPanel} style={{ backgroundColor: "white" }}>
    <MenuItem text="SAVE" Icon={SaveIcon} onClick={() => {}} />
    <MenuItem text="OPTIONS" Icon={OptionsIcon} onClick={() => {}} />
  </div>
);

type MenuItemProps = {
  text: string;
  Icon: IconType;
  onClick: () => void;
};

const MenuItem: React.FC<MenuItemProps> = (props) => (
  <div className={styles.menuItemContainer}>
    {<props.Icon />} <span>{props.text.toUpperCase()}</span>
  </div>
);
