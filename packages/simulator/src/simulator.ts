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

	constructor() {
		this.eventingService = new EventingService();
		this.entityService = new EntityService(this);
		this.chipLibraryService = new ChipLibraryService(this);

		this.chipManager = new ChipManager(this);

		this.registerDefaultChips();
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

	private registerDefaultChips(): void {
		this.chipLibraryService.add({
			label: "AND",
			pins: [
				{ direction: "in", label: "input-pin-0" },
				{ direction: "in", label: "input-pin-1" },
				{ direction: "out", label: "output-pin-0" },
			],
			execute: (inputs) => [inputs[0] && inputs[1]],
		});
	}
}
