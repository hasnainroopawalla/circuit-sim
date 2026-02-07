import type { Position } from "@digital-logic-sim/shared-types";
import type { ChipType } from "../../entities/chips";
import type { Pin, PinType } from "../../entities/pin";
import type { ChipDefinition } from "../chip-library-service";
import type { EntityType } from "../../entities/entity";

export type IEntitySecondaryActionEvent = {
  entityType: EntityType;
  mousePosition: Position;
};

export type IEvents = {
  "sim.save-chip.start": { chipName: string };
  "sim.save-chip.finish": undefined;

  "sim.import-blueprint.start": { blueprintString: string };

  "sim.reset": undefined;

  "overlay.reset": undefined;

  "wire.spawn.start": { startPin: Pin };
  "chip.spawn.start": { chipDefinition: ChipDefinition };

  "chip.spawn.finish": {
    chipId: string;
    chipName: string;
    chipType: ChipType;
    pins: { id: string; name: string; pinType: PinType }[];
  };

  "entity.secondaryAction": IEntitySecondaryActionEvent;

  "view.composite-chip": {
    compositeChipId: string;
  };
};
