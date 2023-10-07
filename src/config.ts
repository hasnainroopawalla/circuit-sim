export const config = {
  document: {
    color: {
      background: "#3D3D3D",
    },
    strokeWeight: 1,
    strokeColor: "#121212",
  },
  component: {
    board: {
      spacingBetweenButtons: 5,
    },
    button: {
      color: "#525151",
      textSize: 20,
      textColor: "#FFFFFF",
    },
    circuit: {
      // TODO: Change name
      widthScale: 50,
      background: "#525151",
    },
    chip: {
      strokeWeight: 0,
      color: {
        andChip: "#FC60A8",
        orChip: "",
        notChip: "",
        chipName: "",
      },
      size: {
        cornerRadius: 5,
      },
      text: {
        size: 20,
        color: "#FFFFFF",
        font: "Helvetica",
      },
    },
    iOChip: {
      strokeWeight: 2,
      size: 30,
      color: {
        stateOff: "#152C40",
        stateOn: "#3083DC",
      },
      innerWire: {
        color: "#121212",
        strokeWeight: 3,
      },
    },
    pin: {
      strokeWeight: 2,
      color: "#121212",
      size: 15,
      mouse: {
        hitRange: 0,
      },
    },
    wire: {
      color: {
        stateOff: "#152C40",
        stateOn: "#3083DC",
      },
      strokeWeight: 4,
    },
  },
};
