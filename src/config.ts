const config = {
  document: {
    color: {
      background: "#3D3D3D",
    },
    strokeWeight: 0,
  },
  component: {
    chip: {
      color: {
        andChip: "#FC60A8",
        orChip: "",
        notChip: "",
        chipName: "",
      },
      size: {
        w: 50,
        cornerRadius: 5,
      },
      text: {
        color: "#FFFFFF",
        font: "Helvetica",
      },
    },
    iOChip: {
      color: {
        stateOff: "#152C40",
        stateOn: "#3083DC",
      },
      innerWire: {
        color: "#121212",
        strokeWeight: 2,
      },
    },
    pin: {
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
      strokeWeight: 3,
    },
  },
};

export default config;
