import { BaseMixin } from "ts-mxn";
import type { ICircuitBoard } from "../circuit-board.interface";

export type ICoreService = {
	name: string;
	isCircuitChip: boolean;
	execute: () => void;
};

type ICoreServiceArgs = {
	name: string;
	isCircuitChip: boolean;
	circuitBoard: ICircuitBoard;
};

class CoreService implements ICoreService {
	public name: string;
	public isCircuitChip: boolean;

	private circuitBoard: ICircuitBoard;

	constructor(args: ICoreServiceArgs) {
		this.circuitBoard = args.circuitBoard;

		this.name = args.name;
		this.isCircuitChip = args.isCircuitChip;
	}

	public execute(): void {
		for (const input of this.circuitBoard.entities.inputs) {
			input.execute();
		}
	}
}

type ICoreMixinArgs = Omit<ICoreServiceArgs, "circuitBoard">;

export class CoreMixin extends BaseMixin<ICircuitBoard, ICoreService> {
	constructor(args: ICoreMixinArgs) {
		super({
			methods: ["execute"],
			props: ["isCircuitChip", "name"],
			initMixin: (circuitBoard) => new CoreService({ circuitBoard, ...args }),
		});
	}
}
