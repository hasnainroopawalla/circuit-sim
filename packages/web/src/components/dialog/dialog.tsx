import type * as React from "react";
import { CloseIcon } from "../icons";
import styles from "./dialog.module.css";
import { useDialog } from "./dialog-context";
import { getCurrentPanelProps } from "./dialog-panel-props";

export const Dialog: React.FC = () => {
	const { currentPanel, closeDialog } = useDialog();

	const { Component, Icon, title } = getCurrentPanelProps(
		currentPanel,
		closeDialog,
	);

	return currentPanel ? (
		<div className={styles.modalContainer}>
			<div className={styles.modalDialog}>
				<div className={styles.modalHeader}>
					<div>
						<Icon /> <span>{title.toUpperCase()}</span>
					</div>
					<CloseIcon className={styles.clickableIcon} onClick={closeDialog} />
				</div>
				<div className={styles.modalContent}>
					<Component />
				</div>
			</div>
		</div>
	) : null;
};
