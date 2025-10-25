import type * as React from "react";
import { Button } from "../../factory";
import { useChips } from "../../hooks";
import { Menu } from "../menu";
import styles from "./buttons-panel.module.css";

export const ButtonsPanel: React.FC = () => {
	const chips = useChips();

	return (
		<div className={styles.buttonsPanelContainer}>
			<Menu />
			{chips.map((chip) => (
				<Button
					key={chip.name}
					text={chip.name}
					appearance="secondary"
					size="large"
					onClick={chip.onClick}
				/>
			))}
		</div>
	);
};
