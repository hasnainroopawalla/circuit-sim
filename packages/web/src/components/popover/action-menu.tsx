import type * as React from "react";

export type ActionMenuItem = {
  label: string;
  handler: () => void;
};

type ActionMenuProps = {
  items: ActionMenuItem[];
};

export const ActionMenu: React.FC<ActionMenuProps> = ({ items }) => {
  return (
    <div className="py-1 text-sm text-white">
      {items.map((item) => (
        <button
          type="button"
          key={item.label}
          onClick={item.handler}
          className="flex items-center w-full gap-2 px-3 py-2 cursor-pointer hover:bg-white/5"
        >
          {item.label}
        </button>
      ))}
    </div>
  );
};
