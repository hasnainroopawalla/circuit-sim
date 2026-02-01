import * as React from "react";

export type PopoverPosition = "top" | "bottom" | "left" | "right";

export type PopoverProps = {
	isOpen: boolean;
	position?: { x: number; y: number };
	children: React.ReactNode;
	onClose: () => void;
};

type IPopoverContext = {
	openPopover: (props: PopoverProps) => void;
	closePopover: () => void;
	popoverProps: PopoverProps | null;
};

const PopoverContext = React.createContext<IPopoverContext>(
	{} as IPopoverContext,
);

export const PopoverProvider = (props: React.PropsWithChildren) => {
	const [popoverProps, setPopoverProps] = React.useState<PopoverProps | null>(
		null,
	);

	const openPopover = React.useCallback((props: PopoverProps) => {
		setPopoverProps(props);
	}, []);

	const closePopover = React.useCallback(() => {
		setPopoverProps(null);
	}, []);

	return (
		<PopoverContext.Provider
			value={{
				openPopover,
				closePopover,
				popoverProps,
			}}
		>
			{props.children}
		</PopoverContext.Provider>
	);
};

export const usePopover = () => React.useContext(PopoverContext);
