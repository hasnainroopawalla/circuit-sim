import p5 from "p5";
import { sketchConfig } from "./sketch/sketch.config";
import { circuitBoardConfig } from "./entities";
import type { Position, Size } from "./entities/types";
import {
	EventingService,
	type IEvents,
	type Unsubscribe,
} from "./services/eventing-service";
import { EntityService } from "./services/entity-service";
import { ChipManager } from "./managers/chip-manager";
import { ChipLibraryService } from "./services/chip-library-service";
import { WireManager } from "./managers/wire-manager";

// let circuitBoard: ICircuitBoard;
let sketchInteractionEnabled = true;

// TODO Improve this
export const setSketchInteraction = (interactionEnabled: boolean) => {
	sketchInteractionEnabled = interactionEnabled;
};

export class Simulator {
	readonly chipLibraryService: ChipLibraryService;

	// services
	readonly eventingService: EventingService;
	readonly entityService: EntityService;

	// managers
	readonly chipManager: ChipManager;
	readonly wireManager: WireManager;

	constructor() {
		// services
		this.eventingService = new EventingService();
		this.entityService = new EntityService(this);
		this.chipLibraryService = new ChipLibraryService(this);

		// managers
		this.chipManager = new ChipManager(this);
		this.wireManager = new WireManager(this);

		this.setupNandGate();
	}

	public createSketch(container: HTMLDivElement): p5 {
		const sketch = (p: p5) => {
			const options = {
				position: {
					x: circuitBoardConfig.widthScale,
					y: circuitBoardConfig.heightScale,
				},
				size: {
					w: p.windowWidth - circuitBoardConfig.widthScale * 2,
					h: p.windowHeight - 65,
				},
			};

			p.setup = () => {
				p.createCanvas(p.windowWidth, p.windowHeight);

				// circuitBoard = createCircuitBoard({
				// 	p,
				// 	name: "main",
				// 	options: {
				// 		position: {
				// 			x: circuitBoardConfig.widthScale,
				// 			y: circuitBoardConfig.heightScale,
				// 		},
				// 		size: {
				// 			w: p.windowWidth - circuitBoardConfig.widthScale * 2,
				// 			h: p.windowHeight - 65,
				// 		},
				// 	},
				// 	isCircuitChip: false,
				// });
			};

			p.draw = () => {
				p.background(sketchConfig.background);
				this.run(p, options);
			};

			p.mouseClicked = this.mouseClicked.bind(this);
			p.mouseDragged = this.mouseDragged.bind(this);
			p.mouseReleased = this.mouseReleased.bind(this);
			p.doubleClicked = this.mouseDoubleClicked.bind(this);
			p.mouseMoved = this.mouseMoved.bind(this);
		};

		return new p5(sketch, container);
	}

	public on<K extends keyof IEvents>(
		event: K,
		handler: (data: IEvents[K]) => void,
	): Unsubscribe {
		return this.eventingService.subscribe(event, handler);
	}

	public emit<K extends keyof IEvents>(event: K, data: IEvents[K]): void {
		this.eventingService.publish(event, data);
	}

	private run(
		p: p5,
		options: {
			position: Position;
			size: Size<"rect">;
		},
	): void {
		this.render(p, options);
	}

	private render(
		p: p5,
		options: {
			position: Position;
			size: Size<"rect">;
		},
	): void {
		p.push();
		p.stroke(circuitBoardConfig.strokeColor);
		p.strokeWeight(2);
		p.fill(circuitBoardConfig.background);
		p.rect(
			options.position.x,
			options.position.y,
			options.size.w,
			options.size.h,
		);

		// slider section
		p.strokeWeight(0);
		p.fill(circuitBoardConfig.sliderSectionColor);
		p.rect(
			0,
			options.position.y,
			circuitBoardConfig.widthScale / 2,
			options.size.h,
		);
		p.rect(
			options.position.x + options.size.w + circuitBoardConfig.widthScale / 2,
			options.position.y,
			circuitBoardConfig.widthScale / 2,
			options.size.h,
		);
		p.pop();

		// this.circuitBoard.getState().render();

		// this.renderWires();
		// this.renderChips();
		// this.renderIOChips();
	}

	private mouseClicked() {
		if (!sketchInteractionEnabled) {
			return;
		}
	}

	private mouseDragged() {
		if (!sketchInteractionEnabled) {
			return;
		}
	}

	private mouseReleased() {
		if (!sketchInteractionEnabled) {
			return;
		}
	}

	private mouseDoubleClicked() {
		if (!sketchInteractionEnabled) {
			return;
		}
	}

	private mouseMoved() {
		if (!sketchInteractionEnabled) {
			return;
		}
	}

	// TODO: remove
	private setupNandGate(): void {
		const andChipSpec = this.chipLibraryService.getChipSpecByName("AND");
		const notChipSpec = this.chipLibraryService.getChipSpecByName("NOT");

		const inputChipSpec = this.chipLibraryService.getChipSpecByName("INPUT");
		const outputChipSpec = this.chipLibraryService.getChipSpecByName("OUTPUT");

		if (!inputChipSpec || !outputChipSpec || !andChipSpec || !notChipSpec) {
			return;
		}

		this.emit("chip.spawn", inputChipSpec);
		this.emit("chip.spawn", inputChipSpec);
		this.emit("chip.spawn", andChipSpec);
		this.emit("chip.spawn", notChipSpec);
		this.emit("chip.spawn", outputChipSpec);

		const inputChip = this.entityService.getEntityById("0");
		// const inputChip = this.entityService.getEntityById("0");
		const outputChip = this.entityService.getEntityById("2");
		const andChip = this.entityService.getEntityById("4");
		const notChip = this.entityService.getEntityById("8");

		if (!inputChip || !outputChip || !andChip || !notChip) {
			return;
		}

		this.wireManager.spawnWire("1", "5") // input 0 to AND in 0
		this.wireManager.spawnWire("2", "6") // input 1 to AND in 1

		this.wireManager.spawnWire("7", "9") // AND out to NOT in

		this.wireManager.spawnWire("10", "12") // NOT out to output

		console.log(this.entityService.entities);

	}
}
