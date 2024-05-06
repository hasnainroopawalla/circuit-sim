import {
  EmitterEvent,
  EmitterEventArgs,
  emitter,
} from "../../../event-service";
import type { Circuit } from "../circuit";
import { Mode } from "../circuit.interface";

export class ButtonService {
  p: p5;
  circuit: Circuit;

  constructor(p: p5, circuit: Circuit) {
    this.p = p;
    this.circuit = circuit;
    !this.circuit.isCustomChip && this.registerSubscriptions();
  }

  // TODO: rename
  private customChipButtonOnClick(
    args: EmitterEventArgs[EmitterEvent.SpawnCustomChip]
  ): void {
    const customChipCircuit =
      this.circuit.blueprintService.createCircuitFromBlueprint(
        args.name,
        args.blueprint,
        "main"
      );

    const customChip = this.circuit.createCustomChip(
      customChipCircuit,
      args.color,
      false
    );
    this.circuit.setMode({ mode: Mode.SpawnChip, deps: { chip: customChip } });
  }

  // TODO: rename
  private coreChipButtonOnClick(
    args: EmitterEventArgs[EmitterEvent.SpawnCoreChip]
  ): void {
    const chip = this.circuit.createCoreChip(args.coreChip, false);
    this.circuit.setMode({ mode: Mode.SpawnChip, deps: { chip } });
  }

  private importCustomChip(
    args: EmitterEventArgs[EmitterEvent.ImportCustomChip]
  ): void {
    emitter.emit(EmitterEvent.AddCustomChipToToolbar, {
      name: args.customChipName,
      blueprint: args.blueprint,
    });
  }

  private registerSubscriptions() {
    emitter.on(EmitterEvent.SaveCircuit, (eventData) =>
      this.circuit.blueprintService.saveCircuit(eventData)
    );
    emitter.on(EmitterEvent.SpawnCoreChip, (eventData) =>
      this.coreChipButtonOnClick(eventData)
    );
    emitter.on(EmitterEvent.SpawnCustomChip, (eventData) =>
      this.customChipButtonOnClick(eventData)
    );
    emitter.on(EmitterEvent.ImportCustomChip, (eventData) =>
      this.importCustomChip(eventData)
    );
  }
}
