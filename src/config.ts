export const config = {
  document: {
    color: {
      background: "#3D3D3D",
    },
    strokeWeight: 1,
    strokeColor: "#121212",
  },
  component: {
    circuit: {
      widthScale: 50,
      background: "#525151",
    },
    chip: {
      strokeWeight: 0,
      size: {
        cornerRadius: 5,
      },
      text: {
        size: 20,
        color: "#FFFFFF",
      },
    },
    iOChip: {
      strokeWeight: 2,
      size: 35,
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
