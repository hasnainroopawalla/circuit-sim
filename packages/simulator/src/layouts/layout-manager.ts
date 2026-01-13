import { InteractionLayer } from "./interaction-layer";
import type { BaseLayer, BaseLayerArgs } from "./base-layer";
import { SimulationLayer } from "./simulation-layer";
import type { Renderable } from "@digital-logic-sim/render-engine";
import type {
	MouseButtonType,
	ButtonEvent,
	MouseScrollType,
	KeyboardButtonType,
} from "../managers/input-manager";
import type { MousePosition } from "../types";
import type { Entity } from "../entities/entity";
import type { Camera } from "../camera";
import type { MousePositionService } from "../services/mouse-position-service";

type LayoutManagerArgs = BaseLayerArgs & {
	camera: Camera;
	mousePositionService: MousePositionService;
};

enum Layer {
	Simulation,
	Interaction,
	Composite,
}

enum State {
	MainMenu,
	Workbench,
	Preview,
}

type LayerTransition = {
	layerId: Layer;
	transition: State;
};

type StateArgs =
	| {
			state: State.Workbench;
			args: { name: string };
	  }
	| {
			state: State.Preview;
			args: { age: number };
	  };

export class LayoutManager {
	private readonly layers: BaseLayer[];
	private hoveredEntity: Entity | null;
	// private state: StateArgs;
	private activeLayers: BaseLayer[];
	private suspendedLayers: BaseLayer[];

	private pendingTransitions: LayerTransition[];

	constructor(args: LayoutManagerArgs) {
		// this.state = {
		// 	state: State.Workbench,
		// 	args: { name: 3 },
		// };

		this.hoveredEntity = null;

		this.layers = [
			// layer 1
			new InteractionLayer({
				...args,
				mousePositionService: args.mousePositionService,
			}),
			// layer 0
			new SimulationLayer({
				...args,
				camera: args.camera,
			}),
		];

		this.activeLayers = [
			this.layers[Layer.Interaction],
			this.layers[Layer.Simulation],
		];
		this.pendingTransitions = [];
		this.suspendedLayers = [];
	}

	public getRenderables(): Renderable[] {
		return this.layers.flatMap((layer) => layer.getRenderables());
	}

	public update(hoveredEntity: Entity | null): void {
		this.hoveredEntity = hoveredEntity;
	}

	public onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		mousePosition: MousePosition,
	): void {
		this.layers.some((layer) =>
			layer.onMouseButtonEvent(
				event,
				nature,
				mousePosition,
				this.hoveredEntity,
			),
		);
	}

	public onMouseMoveEvent(mousePosition: MousePosition): void {
		this.layers.some((layer) =>
			layer.onMouseMoveEvent(mousePosition, this.hoveredEntity),
		);
	}

	public onMouseScrollEvent(event: MouseScrollType): void {
		this.layers.some((layer) => layer.onMouseScrollEvent(event));
	}

	public onKeyboardEvent(event: KeyboardButtonType, nature: ButtonEvent): void {
		this.layers.some((layer) => layer.onKeyboardEvent(event, nature));
	}

	public transitionLayer(transition: LayerTransition): void {
		this.pendingTransitions.push(transition);
	}

	public notificationFromLayer(from: Layer, args: unknown) {}

	private handleTransitions(): void {
		this.pendingTransitions.every((transition) => {});
	}

	// public get hoveredEntity(): Entity | null {
	// 	return this._hoveredEntity;
	// }

	// public set hoveredEntity(value: Entity | null) {
	// 	this._hoveredEntity = value;
	// }
}

// class HelloLayer {
// 	public ff() {
// 		this.layoutManager.notificationFromLayer(Layer.Hello);
// 	}

// 	public update(){
// 		for(layer: layers){
// 			switch(Layer.layerType){
// 				case Layer.Simulation:
// 					layer.isLoading // simulation layer
// 					layer.update(inParams, customArgs)
// 					if(customArgs.isLoading){
// 						layerTransistions.push({layer: Layer.Composite, state: State.active})
// 					}
// 			}
// 		}
// 	}
// }
