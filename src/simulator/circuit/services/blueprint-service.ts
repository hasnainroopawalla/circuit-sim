import {
  EmitterEvent,
  EmitterEventArgs,
  EmitterHelper,
  emitter,
} from "../../../event-service";
import type { Circuit } from "../circuit";
import {
  blueprintToCircuit,
  circuitToBlueprint,
} from "./blueprint-service-utils";

export class BlueprintService {
  p: p5;
  circuit: Circuit;

  constructor(p: p5, circuit: Circuit) {
    this.p = p;
    this.circuit = circuit;
    !this.circuit.isCustomChip && this.registerSubscriptions();
  }

  public saveCircuit(
    eventData: EmitterEventArgs[EmitterEvent.SaveCircuit]
  ): void {
    // Create the custom chip only if inputs and outputs exist
    if (
      this.circuit.entities.inputs.length === 0 ||
      this.circuit.entities.outputs.length === 0
    ) {
      return EmitterHelper.notification(
        "Custom chip not created due to missing inputs/outputs"
      );
    }

    const blueprint = circuitToBlueprint("main", this.circuit);

    emitter.emit(EmitterEvent.AddCustomChipToToolbar, {
      name: eventData.name,
      blueprint: JSON.stringify(blueprint),
    });

    this.circuit.initEntities();
  }

  private createCircuitFromBlueprint(
    eventData: EmitterEventArgs[EmitterEvent.SpawnCustomChip]
  ): void {
    const circuit = blueprintToCircuit(
      this.p,
      eventData.name,
      eventData.blueprint,
      "main"
    );

    this.circuit.createCustomChip(circuit, eventData.color);
  }

  private registerSubscriptions() {
    emitter.on(EmitterEvent.SaveCircuit, (eventData) =>
      this.saveCircuit(eventData)
    );
    emitter.on(EmitterEvent.SpawnCustomChip, (eventData) =>
      this.createCircuitFromBlueprint(eventData)
    );
  }
}
