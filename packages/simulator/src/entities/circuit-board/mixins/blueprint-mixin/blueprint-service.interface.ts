export type CircuitChipSchema = {
  inputs: { id: string }[];
  outputs: { id: string }[];
  chips: {
    id: string;
    name: string;
  }[];
  wires: string[][];
};

export type CircuitChipBlueprint = {
  [chipName: string]: CircuitChipSchema;
};
