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
        w: 30,
        h: 30,
        cornerRadius: 7,
      },
      text: {
        size: 30,
        color: "#FFFFFF",
        font: "Helvetica",
      },
    },
    iOChip: {},
    pin: {
      color: "#121212",
      size: 10,
      mouse: {
        hitRange: 10,
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
