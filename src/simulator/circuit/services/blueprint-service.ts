import {
    EmitterEvent,
    EmitterHelper,
    emitter,
  } from "../../../event-service";
  import { BlueprintHelper } from "../../helpers/blueprint-helper";
  import type { Circuit } from "../circuit";
  
  export class BlueprintService {
    circuit: Circuit;
  
    constructor(circuit: Circuit) {
      this.circuit = circuit;
    }
  
    public saveCircuit(
      name: string, _color?: string
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
  
      const blueprint = BlueprintHelper.circuitToBlueprint("main", this.circuit);
  
      emitter.emit(EmitterEvent.AddCustomChipToToolbar, {
        name,
        blueprint: JSON.stringify(blueprint),
      });
  
      this.circuit.initEntities();
    }
  }
  