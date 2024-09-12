import p5 from "p5";
import { BaseMixin } from "power-mixin";
import { ICircuitBoard } from "../circuit-board-mixin";
import { Chip, IOChip } from "../../chips";
import { Pin } from "../../pin";
import {
  type AbstractState,
  IdleState,
  RepositionState,
  SpawnChipState,
  SpawnIOChipState,
  WiringState,
} from "./controllers";

export enum State {
  Idle = "Idle",
  Reposition = "Reposition",
  Wiring = "Wiring",
  SpawnChip = "SpawnChip",
  SpawnIOChip = "SpawnIOChip",
}

export type StateProps =
  | { state: State.Idle; deps?: object }
  | { state: State.SpawnChip; deps: { chip: Chip } }
  | { state: State.SpawnIOChip; deps: { kind: "input" | "output" } }
  | { state: State.Reposition; deps: { chip: Chip | IOChip } }
  | { state: State.Wiring; deps: { startPin: Pin } };

export type IStateManager = {
  currentState: State;
  setState: (props: StateProps) => void;
  getState: () => AbstractState;
};

class StateManager implements IStateManager {
  public currentState: State;
  private circuitBoard: ICircuitBoard;

  // private readonly states: Record<State, AbstractState>;
  private readonly states: {
    Idle: IdleState;
    Reposition: RepositionState;
    SpawnChip: SpawnChipState;
    SpawnIOChip: SpawnIOChipState;
    Wiring: WiringState;
  };

  constructor(circuitBoard: ICircuitBoard, p: p5) {
    this.circuitBoard = circuitBoard;

    this.states = {
      [State.Idle]: new IdleState(p, circuitBoard),
      [State.Reposition]: new RepositionState(p, circuitBoard),
      [State.SpawnChip]: new SpawnChipState(p, circuitBoard),
      [State.SpawnIOChip]: new SpawnIOChipState(p, circuitBoard),
      [State.Wiring]: new WiringState(p, circuitBoard),
    };

    this.currentState = State.Idle;
  }

  public setState(props: StateProps) {
    const { state, deps } = props;

    switch (state) {
      case State.Idle:
        // this.chipSpawnController.stop();
        // this.repositionController.stop();
        // this.wiringController.stop();
        // this.iOChipSpawnController.stop();
        break;
      case State.Reposition:
        this.states[state].setup(deps.chip);
        break;
      case State.SpawnChip:
        this.states[state].setup(deps.chip);
        break;
      case State.SpawnIOChip:
        console.log("YA");
        this.states[state].setup(
          this.circuitBoard.createIOChip(deps.kind, true, false)
        );
        break;
      case State.Wiring:
        this.states[state].setup(deps.startPin);
        break;
      default:
        throw new Error("Invalid mode");
    }

    this.currentState = state;
  }

  public getState(): AbstractState {
    return this.states[this.currentState];
  }
}

export class StateManagerMixin extends BaseMixin<ICircuitBoard, IStateManager> {
  constructor(p: p5) {
    super({
      methods: ["setState", "getState"],
      props: ["currentState"],
      initMixin: circuitBoard => new StateManager(circuitBoard, p),
    });
  }
}
