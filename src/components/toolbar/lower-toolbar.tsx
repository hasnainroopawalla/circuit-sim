import React from "react";
import { Button } from "../factory";
import { EmitterEvent, emitter } from "../../event-service";
import type { IUseCustomChips } from "../hooks";

import styles from "./toolbar.module.css";

type LowerToolbarProps = {
  useCustomChips: IUseCustomChips;
  importChipButtonOnClick: () => void;
};

export const LowerToolbar: React.FC<LowerToolbarProps> = (props) => {
  const { useCustomChips, importChipButtonOnClick } = props;

  const customChips = useCustomChips();

  // const blueprint = JSON.stringify({
  //   main: {
  //     inputs: [
  //       {
  //         id: "input.1",
  //         pin: "input.1-pin.0",
  //       },
  //       {
  //         id: "input.2",
  //         pin: "input.2-pin.0",
  //       },
  //     ],
  //     outputs: [
  //       {
  //         id: "output.4",
  //         pin: "output.4-pin.0",
  //       },
  //     ],
  //     chips: [
  //       {
  //         id: "chip.AND.0",
  //         name: "AND",
  //         inputPins: ["chip.AND.0-inputPin.0", "chip.AND.0-inputPin.1"],
  //         outputPins: ["chip.AND.0-outputPin.0"],
  //       },
  //       {
  //         id: "chip.NOT.3",
  //         name: "NOT",
  //         inputPins: ["chip.NOT.3-inputPin.0"],
  //         outputPins: ["chip.NOT.3-outputPin.0"],
  //       },
  //     ],
  //     wires: [
  //       ["input.2-pin.0", "chip.AND.0-inputPin.1"],
  //       ["input.1-pin.0", "chip.AND.0-inputPin.0"],
  //       ["chip.AND.0-outputPin.0", "chip.NOT.3-inputPin.0"],
  //       ["chip.NOT.3-outputPin.0", "output.4-pin.0"],
  //     ],
  //   },
  // });

  // const blueprintNANDNOT = JSON.stringify({
  //   main: {
  //     inputs: [
  //       {
  //         id: "input-0",
  //         pin: "input-0_pin-0",
  //       },
  //       {
  //         id: "input-1",
  //         pin: "input-1_pin-0",
  //       },
  //     ],
  //     outputs: [
  //       {
  //         id: "output-0",
  //         pin: "output-0_pin-0",
  //       },
  //     ],
  //     chips: [
  //       {
  //         id: "chip-0",
  //         name: "NAND",
  //         inputPins: ["chip-0_input-pin-0", "chip-0_input-pin-1"],
  //         outputPins: ["chip-0_output-pin-0"],
  //       },
  //       {
  //         id: "chip-1",
  //         name: "NOT",
  //         inputPins: ["chip-1_input-pin-0"],
  //         outputPins: ["chip-1_output-pin-0"],
  //       },
  //     ],
  //     wires: [
  //       ["input-1_pin-0", "chip-0_input-pin-1"],
  //       ["input-0_pin-0", "chip-0_input-pin-0"],
  //       ["chip-0_output-pin-0", "chip-1_input-pin-0"],
  //       ["chip-1_output-pin-0", "output-0_pin-0"],
  //     ],
  //   },
  //   NAND: {
  //     inputs: [
  //       {
  //         id: "input-0",
  //         pin: "input-00_pin-0",
  //       },
  //       {
  //         id: "input-1",
  //         pin: "input-1_pin-0",
  //       },
  //     ],
  //     outputs: [
  //       {
  //         id: "output-0",
  //         pin: "output-0_pin-0",
  //       },
  //     ],
  //     chips: [
  //       {
  //         id: "chip-0",
  //         name: "AND",
  //         inputPins: ["chip-0_input-pin-0", "chip-0_input-pin-1"],
  //         outputPins: ["chip-0_output-pin-0"],
  //       },
  //       {
  //         id: "chip-1",
  //         name: "NOT",
  //         inputPins: ["chip-1_input-pin-0"],
  //         outputPins: ["chip-1_output-pin-0"],
  //       },
  //     ],
  //     wires: [
  //       ["input-1_pin-0", "chip-0_input-pin-1"],
  //       ["input-0_pin-0", "chip-0_input-pin-0"],
  //       ["chip-0_output-pin-0", "chip-1_input-pin-0"],
  //       ["chip-1_output-pin-0", "output-0_pin-0"],
  //     ],
  //   },
  // });

  const blueprintNAND = JSON.stringify({
    main: {
      inputs: [
        {
          id: "chip.input.2",
        },
        {
          id: "chip.input.3",
        },
      ],
      outputs: [
        {
          id: "chip.output.4",
        },
      ],
      chips: [
        {
          id: "chip.AND.0",
          name: "AND",
        },
        {
          id: "chip.NOT.1",
          name: "NOT",
        },
      ],
      wires: [
        ["chip.input.2/output.0", "chip.AND.0/input.0"],
        ["chip.input.3/output.0", "chip.AND.0/input.1"],
        ["chip.AND.0/output.0", "chip.NOT.1/input.0"],
        ["chip.NOT.1/output.0", "chip.output.4/input.0"],
      ],
    },
  });

  const blueprintNANDNOR = JSON.stringify({
    NAND: {
      inputs: [
        {
          id: "chip.NAND.0.input.0",
        },
        {
          id: "chip.NAND.0.input.1",
        },
      ],
      outputs: [
        {
          id: "chip.NAND.0.output.0",
        },
      ],
      chips: [
        {
          id: "chip.AND.4",
          name: "AND",
        },
        {
          id: "chip.NOT.5",
          name: "NOT",
        },
      ],
      wires: [
        ["chip.NAND.0.input.0/input.0", "chip.AND.4/input.0"],
        ["chip.NAND.0.input.1/input.0", "chip.AND.4/input.1"],
        ["chip.AND.4/output.0", "chip.NOT.5/input.0"],
        ["chip.NOT.5/output.0", "chip.NAND.0.output.0/output.0"],
      ],
    },
    NOR: {
      inputs: [
        {
          id: "chip.NOR.6.input.0",
        },
        {
          id: "chip.NOR.6.input.1",
        },
      ],
      outputs: [
        {
          id: "chip.NOR.6.output.0",
        },
      ],
      chips: [
        {
          id: "chip.OR.10",
          name: "OR",
        },
        {
          id: "chip.NOT.11",
          name: "NOT",
        },
      ],
      wires: [
        ["chip.OR.10/output.0", "chip.NOT.11/input.0"],
        ["chip.NOR.6.input.0/input.0", "chip.OR.10/input.0"],
        ["chip.NOR.6.input.1/input.0", "chip.OR.10/input.1"],
        ["chip.NOT.11/output.0", "chip.NOR.6.output.0/output.0"],
      ],
    },
    main: {
      inputs: [
        {
          id: "chip.input.12",
        },
        {
          id: "chip.input.13",
        },
      ],
      outputs: [
        {
          id: "chip.output.14",
        },
      ],
      chips: [
        {
          id: "chip.NAND.0",
          name: "NAND",
        },
        {
          id: "chip.NOR.6",
          name: "NOR",
        },
      ],
      wires: [
        ["chip.NAND.0.output.0/output.0", "chip.NOR.6.input.0/input.0"],
        ["chip.NAND.0.output.0/output.0", "chip.NOR.6.input.1/input.0"],
        ["chip.input.12/output.0", "chip.NAND.0.input.0/input.0"],
        ["chip.input.13/output.0", "chip.NAND.0.input.1/input.0"],
        ["chip.NOR.6.output.0/output.0", "chip.output.14/input.0"],
      ],
    },
  });

  const blueprintNOR = JSON.stringify({
    main: {
      inputs: [
        {
          id: "chip.input.2",
        },
        {
          id: "chip.input.3",
        },
      ],
      outputs: [
        {
          id: "chip.output.4",
        },
      ],
      chips: [
        {
          id: "chip.OR.0",
          name: "OR",
        },
        {
          id: "chip.NOT.1",
          name: "NOT",
        },
      ],
      wires: [
        ["chip.OR.0/output.0", "chip.NOT.1/input.0"],
        ["chip.input.2/output.0", "chip.OR.0/input.0"],
        ["chip.input.3/output.0", "chip.OR.0/input.1"],
        ["chip.NOT.1/output.0", "chip.output.4/input.0"],
      ],
    },
  });

  return (
    <div className={`${styles.toolbar} ${styles.lowerToolbar}`}>
      <Button
        text="AND"
        appearance="secondary"
        size="large"
        onClick={() =>
          emitter.emit(EmitterEvent.SpawnCoreChip, {
            coreChip: "AND",
          })
        }
      />
      <Button
        text="OR"
        appearance="secondary"
        size="large"
        onClick={() =>
          emitter.emit(EmitterEvent.SpawnCoreChip, {
            coreChip: "OR",
          })
        }
      />
      <Button
        text="NOT"
        appearance="secondary"
        size="large"
        onClick={() =>
          emitter.emit(EmitterEvent.SpawnCoreChip, {
            coreChip: "NOT",
          })
        }
      />
      <Button
        text="NAND"
        appearance="secondary"
        size="large"
        onClick={() =>
          emitter.emit(EmitterEvent.SpawnCustomChip, {
            name: "NAND",
            blueprint: blueprintNAND,
            color: "blue",
          })
        }
      />
      <Button
        text="NOR"
        appearance="secondary"
        size="large"
        onClick={() =>
          emitter.emit(EmitterEvent.SpawnCustomChip, {
            name: "NOR",
            blueprint: blueprintNOR,
            color: "blue",
          })
        }
      />{" "}
      <Button
        text="NAND-NOR"
        appearance="secondary"
        size="large"
        onClick={() =>
          emitter.emit(EmitterEvent.SpawnCustomChip, {
            name: "NAND-NOR",
            blueprint: blueprintNANDNOR,
            color: "blue",
          })
        }
      />
      {customChips.map((customChip) => (
        <Button
          key={customChip.name}
          text={customChip.name}
          appearance="secondary"
          size="large"
          onClick={customChip.onClick}
        />
      ))}
      <Button
        text="+"
        appearance="primary"
        size="large"
        onClick={importChipButtonOnClick}
      />
    </div>
  );
};
