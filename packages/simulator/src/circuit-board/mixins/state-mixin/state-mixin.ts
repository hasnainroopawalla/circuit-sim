import p5 from "p5";
import { BaseMixin } from "power-mixin";
import { ICircuitBoard, State } from "../../circuit-board.interface";
import { Chip, IOChip } from "../../../chips";
import { Pin } from "../../../pin";
import { AbstractState } from "./abstract-state";
import { IdleState } from "./idle-state";
import { RepositionState } from "./reposition-state";
import { SpawnChipState } from "./spawn-chip-state";
import { SpawnIOChipState } from "./spawn-io-chip-state";
import { WiringState } from "./wiring-state";

type StateProps =
  | { state: State.Idle; props?: object }
  | { state: State.SpawnChip; props: { chip: Chip } }
  | { state: State.SpawnIOChip; props: { kind: "input" | "output" } }
  | { state: State.Reposition; props: { chip: Chip | IOChip } }
  | { state: State.Wiring; props: { startPin: Pin } };

export type ICircuitBoardState = {
  currentState: State;
  setState: (props: StateProps) => void;
  getState: () => AbstractState;
};

class CircuitBoardState implements ICircuitBoardState {
  public currentState: State;
  private circuitBoard: ICircuitBoard;

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

  public setState(stateProps: StateProps) {
    const { state, props } = stateProps;

    switch (state) {
      case State.Idle:
        // this.chipSpawnController.stop();
        // this.repositionController.stop();
        // this.wiringController.stop();
        // this.iOChipSpawnController.stop();
        break;
      case State.Reposition:
        this.states[state].setup(props.chip);
        break;
      case State.SpawnChip:
        this.states[state].setup(props.chip);
        break;
      case State.SpawnIOChip:
        this.states[state].setup(
          this.circuitBoard.createIOChip(props.kind, true, false)
        );
        break;
      case State.Wiring:
        this.states[state].setup(props.startPin);
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

export class CircuitBoardStateMixin extends BaseMixin<
  ICircuitBoard,
  ICircuitBoardState
> {
  constructor(p: p5) {
    super({
      methods: ["setState", "getState"],
      props: ["currentState"],
      initMixin: circuitBoard => new CircuitBoardState(circuitBoard, p),
    });
  }
}
