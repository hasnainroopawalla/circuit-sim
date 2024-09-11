import p5 from "p5";
import { BaseMixin } from "power-mixin";
import { ICircuitBoard } from "../circuit-board-mixin";
import { Chip, IOChip } from "../../chips";
import { Pin } from "../../pin";
import { IdleState } from "./controllers/idle-state";

export enum State {
  Idle = "Idle",
  Reposition = "Reposition",
  Wiring = "Wiring",
  SpawnChip = "SpawnChip",
  SpawnIOChipHover = "SpawnIOChipHover",
}

export type StateProps =
  | { state: State.Idle; deps?: object }
  | { state: State.SpawnChip; deps: { chip: Chip } }
  | { state: State.Reposition; deps: { chip: Chip | IOChip } }
  | { state: State.Wiring; deps: { startPin: Pin } }
  | { state: State.SpawnIOChipHover; deps: { kind: "input" | "output" } };

export type IStateManager = {
  state: State;
  setState: (props: StateProps) => void;
};

class StateManager implements IStateManager {
  public state: State;
  private circuitBoard: ICircuitBoard;

  private readonly states: {
    idleState: IdleState;
  };

  constructor(circuitBoard: ICircuitBoard, p: p5) {
    this.circuitBoard = circuitBoard;
    this.state = State.Idle;

    this.states = {
      idleState: new IdleState(p, circuitBoard),
    };
  }

  public setState(props: StateProps) {
    const { state, deps } = props;

    switch (state) {
      case State.Idle:
        this.chipSpawnController.stop();
        this.repositionController.stop();
        this.wiringController.stop();
        this.iOChipSpawnController.stop();
        break;
      case State.Reposition:
        this.repositionController.setChip(deps.chip);
        break;
      case State.SpawnChip:
        this.chipSpawnController.setGhostChip(deps.chip);
        break;
      case State.SpawnIOChipHover:
        this.iOChipSpawnController.setGhostIOChip(
          this.createIOChip(deps.kind, true, false)
        );
        break;
      case State.Wiring:
        this.wiringController.setStartPin(deps.startPin);
        break;
      default:
        throw new Error("Invalid mode");
    }
    this.state = state;
  }
}

export class StateManagerMixin extends BaseMixin<ICircuitBoard, IStateManager> {
  constructor(p: p5) {
    super({
      methods: ["setState"],
      props: ["state"],
      initMixin: circuitBoard => new StateManager(circuitBoard, p),
    });
  }
}
