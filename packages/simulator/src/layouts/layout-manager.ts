import { InteractionLayer } from "./interaction-layer";
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
import { LayerType } from "./layout.interface";
import { LayerUtils } from "./layer.utils";
import { LayerStack } from "./layer-stack";

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
	  };

export class LayoutManager {
	private sim: Simulator;
	private camera: Camera;

	private hoveredEntity: Entity | null;
	private state: StateArgs;

	private layerStack: LayerStack;

	// private activeLayers: OrderedMap<BaseLayer<LayerType>>;
	// private suspendedLayers: OrderedMap<BaseLayer<LayerType>>;

	constructor(args: LayoutManagerArgs) {
		this.state = {
			kind: State.Workbench,
			args: { compositeSelected: "" },
		};
		this.sim = args.sim;
		this.camera = args.camera;

		this.hoveredEntity = null;

		this.layerStack = new LayerStack();

		this.layerStack.register(
			LayerType.Interaction,
			new InteractionLayer({
				...args,
				mousePositionService: args.mousePositionService,
			}),
			2,
		);
		this.layerStack.register(
			LayerType.Simulation,
			new SimulationLayer({
				...args,
				camera: args.camera,
			}),
			1,
		);
		this.layerStack.register(
			LayerType.Overlay,
			new OverlayLayer({
				...args,
				camera: args.camera,
			}),
			0,
		);

		// this.activeLayers = OrderedMap.from({
		// 	Interaction: new InteractionLayer({
		// 		...args,
		// 		mousePositionService: args.mousePositionService,
		// 	}),
		// 	Simulation: new SimulationLayer({
		// 		...args,
		// 		camera: args.camera,
		// 	}),
		// 	Overlay: new OverlayLayer({
		// 		...args,
		// 		camera: args.camera,
		// 	}),
		// });

		// this.suspendedLayers = OrderedMap.from({});
	}

	public getRenderables(): Renderable[] {
		let renderables: Renderable[] = [];

		this.layerStack.getActiveLayers().forEach((layer) => {
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
		this.layerStack
			.getActiveLayers()
			.some((layer) =>
				layer.onMouseButtonEvent(
					event,
					nature,
					mousePosition,
					this.hoveredEntity,
				),
			);
	}

	public onMouseMoveEvent(mousePosition: MousePosition): void {
		this.layerStack
			.getActiveLayers()
			.some((layer) =>
				layer.onMouseMoveEvent(mousePosition, this.hoveredEntity),
			);
	}

	public onMouseScrollEvent(event: MouseScrollType): void {
		this.layerStack
			.getActiveLayers()
			.some((layer) => layer.onMouseScrollEvent(event));
	}

	public onKeyboardEvent(event: KeyboardButtonType, nature: ButtonEvent): void {
		this.layerStack
			.getActiveLayers()
			.some((layer) => layer.onKeyboardEvent(event, nature));
	}

	public transitionState(): void {
		switch (this.state.kind) {
			case State.Workbench: {
				if (this.state.args.compositeSelected) {
					this.transitionToPreviewState(this.state.args.compositeSelected);
				}
				break;
			}
			case State.Preview: {
				if (this.state.args.exitPreview) {
					this.transitionToWorkbenchState(this.state.args.cameraPosition);
				}
				break;
			}
		}
	}

	public notificationFromLayers() {
		this.layerStack.getActiveLayers().forEach((layer) => {
			switch (this.state.kind) {
				case State.Workbench:
					{
						if (LayerUtils.isSimulationLayer(layer)) {
							this.state.args.compositeSelected = layer.notifyManager();
						}
					}
					break;
				case State.Preview:
					{
						if (LayerUtils.isCompositeLayer(layer)) {
							this.state.args.exitPreview = layer.notifyManager();
						}
					}
					break;
			}
		});
	}

	private transitionToWorkbenchState(cameraPosition: Position): void {
		this.layerStack.deactivate(LayerType.Composite);

		this.layerStack.activate(LayerType.Simulation);
		this.layerStack.activate(LayerType.Interaction);

		this.camera.resetCamera(cameraPosition);

		this.state = {
			kind: State.Workbench,
			args: { compositeSelected: "" },
		};
	}

	private transitionToPreviewState(compositeId: string): void {
		this.layerStack.deactivate(LayerType.Simulation);
		this.layerStack.deactivate(LayerType.Interaction);

		this.layerStack.register(
			LayerType.Composite,
			new CompositeLayer({
				sim: this.sim,
				camera: this.camera,
				compositeId,
			}),
			4, // TODO: dont hardcode for multiple composite layers
		);

		this.state = {
			kind: State.Preview,
			args: {
				exitPreview: false,
				cameraPosition: this.camera.getCameraPosition(),
			},
		};

		this.camera.resetCamera();
	}
}
