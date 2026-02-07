import * as React from "react";
import { useOnClickOutside } from "../../utils";
import { useSimulatorApp } from "../../contexts/simulator-app-context";
import type {
  IEntitySecondaryActionEvent,
  SimulatorApp,
} from "@digital-logic-sim/simulator";
import { ActionMenu, type ActionMenuItem } from "./action-menu";

export const Popover: React.FC = () => {
  const simulatorApp = useSimulatorApp();

  const divRef = React.useRef<HTMLDivElement>(null);

  const [popoverData, setPopoverData] =
    React.useState<IEntitySecondaryActionEvent | null>(null);

  const closePopover = React.useCallback(() => {
    setPopoverData(null);
  }, []);

  useOnClickOutside(divRef, closePopover);

  React.useEffect(() => {
    const unubscribe = simulatorApp.sim.on("entity.secondaryAction", (data) => {
      setPopoverData(data);
    });

    return () => {
      unubscribe();
    };
  }, [simulatorApp]);

  const items = React.useMemo(() => {
    if (!popoverData) {
      return [];
    }

    return getActionsForEntity(popoverData, simulatorApp).map((action) => ({
      ...action,
      handler: () => {
        action.handler();
        setPopoverData(null);
      },
    }));
  }, [popoverData, simulatorApp]);

  return (
    popoverData && (
      <div
        ref={divRef}
        className="fixed z-50 rounded-lg shadow-lg bg-neutral-900/90 backdrop-blur-xl ring-1 ring-white/10"
        style={{
          top: `${popoverData.mousePosition.y}px`,
          left: `${popoverData.mousePosition.x}px`,
        }}
      >
        <div className="overflow-hidden">
          <ActionMenu items={items} />
        </div>
      </div>
    )
  );
};

function getActionsForEntity(
  data: IEntitySecondaryActionEvent,
  simulatorApp: SimulatorApp,
): ActionMenuItem[] {
  switch (data.entityType) {
    case "chip":
      return [
        {
          label: "View",
          handler: () => {
            simulatorApp.sim.emit("view.composite-chip", {
              compositeChipId: data.entityType,
            });
          },
        },
        {
          label: "Delete",
          handler: () => {},
        },
      ];

    case "wire":
      return [
        {
          label: "Delete",
          handler: () => {},
        },
      ];

    case "pin":
      return [];

    default:
      return [];
  }
}
