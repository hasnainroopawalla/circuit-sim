import type {
	IEntitySecondaryActionEvent,
	SimulatorApp,
} from "@digital-logic-sim/simulator";
import type { ActionMenuItem } from "./action-menu";

export type ActionContext = {
	data: IEntitySecondaryActionEvent;
	simulatorApp: SimulatorApp;
};

const Actions = {
	viewCompositeChip: ({
		data,
		simulatorApp,
	}: ActionContext): ActionMenuItem => ({
		label: "View",
		handler: () =>
			simulatorApp.sim.emit("composite-chip.view", {
				compositeChipId: data.entityId,
			}),
	}),

	deleteChip: ({ data, simulatorApp }: ActionContext): ActionMenuItem => ({
		label: "Delete",
		handler: () =>
			simulatorApp.sim.emit("chip.delete.start", {
				chipId: data.entityId,
			}),
	}),
};

export const ENTITY_ACTIONS = {
	compositeChip: (ctx: ActionContext): ActionMenuItem[] => [
		Actions.viewCompositeChip(ctx),
		Actions.deleteChip(ctx),
	],

	defaultChip: (ctx: ActionContext): ActionMenuItem[] => [
		Actions.deleteChip(ctx),
	],
};
