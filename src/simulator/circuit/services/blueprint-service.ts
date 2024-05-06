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

  public createCircuitFromBlueprint(
    name: string,
    blueprint: string,
    _color: string
  ): Circuit {
    return blueprintToCircuit(this.p, name, blueprint, "main");
  }
}
