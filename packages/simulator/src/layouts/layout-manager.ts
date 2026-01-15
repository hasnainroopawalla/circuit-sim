import { InteractionLayer } from "./interaction-layer";
import { BaseLayer, Layer } from "./base-layer";
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
import { Camera } from "../camera";
import type { MousePositionService } from "../services/mouse-position-service";
import { CompositeLayer } from "./composite-layer";
import { Simulator } from "../simulator";
import { Position } from "@digital-logic-sim/shared-types";
import { OverlayLayer } from "./overlay-layer";

type LayoutManagerArgs = {
	sim: Simulator
	camera: Camera;
	mousePositionService: MousePositionService;
};

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
			kind: State.Workbench;
			args: {compositeSelected: string}
	  }
	| {
			kind: State.Preview;
			args: {exitPreview:boolean, cameraPosition: Position};
	  }
	  | {
			kind: State.MainMenu;
			args: {};
	  };

export class LayoutManager {
	private sim: Simulator;
	private camera: Camera;
	private readonly layers: BaseLayer[];
	private hoveredEntity: Entity | null;
	private state: StateArgs;
	private activeLayers: BaseLayer[];
	private suspendedLayers: BaseLayer[];

	private pendingTransitions: LayerTransition[];

	constructor(args: LayoutManagerArgs) {
		 this.state = {
		 	kind: State.Workbench,
			args: {compositeSelected: ""}
		 };
		 this.sim = args.sim;
		 this.camera = args.camera;

		this.hoveredEntity = null;

		this.layers = [
			// layer 1
			new InteractionLayer({
				...args,
				layerType: Layer.Interaction,
				mousePositionService: args.mousePositionService,
			}),
			// layer 0
			new SimulationLayer({
				...args,
				layerType: Layer.Simulation,
				camera: args.camera,
			}),
			new OverlayLayer({
				...args,
				layerType: Layer.Overlay,
				camera: args.camera,
			}),
		];

		this.activeLayers = [
			this.layers[0],
			this.layers[1],
			this.layers[2]
		];
		this.pendingTransitions = [];
		this.suspendedLayers = [];
	}

	public getRenderables(): Renderable[] {
		let renderables: Renderable[] = []
		for(const layer of this.activeLayers) {			
			renderables = [...renderables, ...layer.getRenderables(renderables)]
		}
		return renderables;
		// return this.activeLayers.flatMap((layer) => layer.getRenderables());
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
		this.activeLayers.some((layer) =>
			layer.onMouseButtonEvent(
				event,
				nature,
				mousePosition,
				this.hoveredEntity,
			),
		);
	}

	public onMouseMoveEvent(mousePosition: MousePosition): void {
		this.activeLayers.some((layer) =>
			layer.onMouseMoveEvent(mousePosition, this.hoveredEntity),
		);
	}

	public onMouseScrollEvent(event: MouseScrollType): void {
		this.activeLayers.some((layer) => layer.onMouseScrollEvent(event));
	}

	public onKeyboardEvent(event: KeyboardButtonType, nature: ButtonEvent): void {
		this.activeLayers.some((layer) => layer.onKeyboardEvent(event, nature));
	}

	public transitionState(): void {
		switch(this.state.kind){
			case State.Workbench:
				if(this.state.args.compositeSelected!==""){
					const simulationLayer = this.activeLayers.pop();
					if(simulationLayer!==undefined){
						this.suspendedLayers.push(simulationLayer);
					}
					const interactionLayer = this.activeLayers.pop();
					if(interactionLayer!==undefined){
						this.suspendedLayers.push(interactionLayer);
					}
					const compositeArgs = {sim: this.sim, layerType: Layer.Composite, camera: this.camera, compositeId: this.state.args.compositeSelected};	
					const compositeLayer = new CompositeLayer(compositeArgs);
					this.layers.push(compositeLayer);
					this.activeLayers.push(compositeLayer);
					
					this.state = {kind: State.Preview, args: {exitPreview: false, cameraPosition: this.camera.getCameraPosition()}};
					this.camera.resetCamera();
				}
				break;
			case State.Preview:
				if(this.state.args.exitPreview){
					this.layers.pop();
					this.activeLayers.pop();
					const interactionLayer = this.suspendedLayers.pop();
					if(interactionLayer!==undefined){
						this.activeLayers.push(interactionLayer);
					}
					const simulationLayer = this.suspendedLayers.pop();
					if(simulationLayer!==undefined){
						this.activeLayers.push(simulationLayer);
					}
					this.camera.resetCamera(this.state.args.cameraPosition);
					this.state = {kind: State.Workbench, args: {compositeSelected: ""}};
				}
				break;
		}
	}

	public notificationFromLayers() {
		switch(this.state.kind){
			case State.Workbench:
				for(const layer of this.activeLayers){
					 if(layer.getLayerType()===Layer.Simulation){
						// TODO: strongly type layer here
					 	this.state.args.compositeSelected = (layer as SimulationLayer).notifyManager();
					 }	
				}
				break;
			case State.Preview:
				for(const layer of this.activeLayers){
				 	if(layer.getLayerType()===Layer.Composite){
				 		this.state.args.exitPreview = (layer as CompositeLayer).notifyManager();
				 	}
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
