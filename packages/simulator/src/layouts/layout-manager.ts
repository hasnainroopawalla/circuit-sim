import { InteractionLayer } from "./interaction-layer";
import { type BaseLayer, Layer } from "./base-layer";
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
import { CompositeLayer } from "./composite-layer";
import type { Simulator } from "../simulator";
import type { Position } from "@digital-logic-sim/shared-types";
import { OverlayLayer } from "./overlay-layer";
import OrderedMap from "orderedmap";
import { orderedMapSome } from "../utils";

type LayoutManagerArgs = {
	sim: Simulator;
	camera: Camera;
	mousePositionService: MousePositionService;
};

enum State {
	MainMenu,
	Workbench,
	Preview,
}

type StateArgs =
	| {
			kind: State.Workbench;
			args: { compositeSelected: string };
	  }
	| {
			kind: State.Preview;
			args: { exitPreview: boolean; cameraPosition: Position };
	  }
	| {
			kind: State.MainMenu;
			args: {};
	  };

export class LayoutManager {
	private sim: Simulator;
	private camera: Camera;

	private hoveredEntity: Entity | null;
	private state: StateArgs;

	// TODO: create custom OrderedMap for enum key
	private activeLayers: OrderedMap<BaseLayer>;
	private suspendedLayers: OrderedMap<BaseLayer>;

	constructor(args: LayoutManagerArgs) {
		this.state = {
			kind: State.Workbench,
			args: { compositeSelected: "" },
		};
		this.sim = args.sim;
		this.camera = args.camera;

		this.hoveredEntity = null;

		this.activeLayers = OrderedMap.from({
			Interaction: new InteractionLayer({
				...args,
				layerType: Layer.Interaction,
				mousePositionService: args.mousePositionService,
			}),
			Simulation: new SimulationLayer({
				...args,
				layerType: Layer.Simulation,
				camera: args.camera,
			}),
			Overlay: new OverlayLayer({
				...args,
				layerType: Layer.Overlay,
				camera: args.camera,
			}),
		});

		this.suspendedLayers = OrderedMap.from({});
	}

	public getRenderables(): Renderable[] {
		let renderables: Renderable[] = [];

		this.activeLayers.forEach((_layerName, layer) => {
			renderables = [...renderables, ...layer.getRenderables(renderables)];
		});

		return renderables;
	}

	public update(hoveredEntity: Entity | null): void {
		this.hoveredEntity = hoveredEntity;
		this.notificationFromLayers();
	}

	public onMouseButtonEvent(
		event: MouseButtonType,
		nature: ButtonEvent,
		mousePosition: MousePosition,
	): void {
		orderedMapSome(this.activeLayers, (layer) =>
			layer.onMouseButtonEvent(
				event,
				nature,
				mousePosition,
				this.hoveredEntity,
			),
		);
	}

	public onMouseMoveEvent(mousePosition: MousePosition): void {
		orderedMapSome(this.activeLayers, (layer) =>
			layer.onMouseMoveEvent(mousePosition, this.hoveredEntity),
		);
	}

	public onMouseScrollEvent(event: MouseScrollType): void {
		orderedMapSome(this.activeLayers, (layer) =>
			layer.onMouseScrollEvent(event),
		);
	}

	public onKeyboardEvent(event: KeyboardButtonType, nature: ButtonEvent): void {
		orderedMapSome(this.activeLayers, (layer) =>
			layer.onKeyboardEvent(event, nature),
		);
	}

	public transitionState(): void {
		switch (this.state.kind) {
			case State.Workbench: {
				if (this.state.args.compositeSelected === "") {
					break;
				}
				const simulationLayer = this.activeLayers.get("Simulation");
				this.activeLayers = this.activeLayers.remove("Simulation");
				if (simulationLayer) {
					this.suspendedLayers = this.suspendedLayers.append({
						Simulation: simulationLayer,
					});
				}
				const interactionLayer = this.activeLayers.get("Interaction");
				this.activeLayers = this.activeLayers.remove("Interaction");
				if (interactionLayer) {
					this.suspendedLayers.append({ Interaction: interactionLayer });
				}
				const compositeArgs = {
					sim: this.sim,
					layerType: Layer.Composite,
					camera: this.camera,
					compositeId: this.state.args.compositeSelected,
				};
				const compositeLayer = new CompositeLayer(compositeArgs);
				this.activeLayers = this.activeLayers.prepend({
					Composite: compositeLayer,
				});

				this.state = {
					kind: State.Preview,
					args: {
						exitPreview: false,
						cameraPosition: this.camera.getCameraPosition(),
					},
				};
				this.camera.resetCamera();
				break;
			}
			case State.Preview: {
				if (!this.state.args.exitPreview) {
					break;
				}
				this.activeLayers = this.activeLayers.remove("Composite");
				const simulationLayer = this.suspendedLayers.get("Simulation");
				this.suspendedLayers = this.suspendedLayers.remove("Simulation");
				if (simulationLayer !== undefined) {
					this.activeLayers = this.activeLayers.prepend({
						Simulation: simulationLayer,
					});
				}
				const interactionLayer = this.suspendedLayers.get("Interaction");
				this.suspendedLayers = this.suspendedLayers.remove("Interaction");
				if (interactionLayer) {
					this.activeLayers = this.activeLayers.prepend({
						Interaction: interactionLayer,
					});
				}
				this.camera.resetCamera(this.state.args.cameraPosition);
				this.state = {
					kind: State.Workbench,
					args: { compositeSelected: "" },
				};
				break;
			}
		}
	}

	public notificationFromLayers() {
		switch (this.state.kind) {
			case State.Workbench:
				{
					this.activeLayers.forEach((_layerName, layer) => {
						// TODO: strongly type layer here
						if (layer.getLayerType() === Layer.Simulation) {
							this.state.args.compositeSelected = (
								layer as SimulationLayer
							).notifyManager();
						}
					});
				}
				break;
			case State.Preview:
				{
					this.activeLayers.forEach((_layerName, layer) => {
						if (layer.getLayerType() === Layer.Composite) {
							this.state.args.exitPreview = (
								layer as CompositeLayer
							).notifyManager();
						}
					});
				}
				break;
		}
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
