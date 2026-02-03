const MATRIX_FLOAT_SIZE = 16;
const COLOR_FLOAT_SIZE = 4;

export const renderEngineConfig = {
	chunkSize: 500,
	matrixFloatSize: MATRIX_FLOAT_SIZE,
	colorFloatSize: COLOR_FLOAT_SIZE,
	radiusFloatSize: 4,
	modelFloatSize: MATRIX_FLOAT_SIZE + COLOR_FLOAT_SIZE,

	lineDataFloatSize: 4,
	lineThickness: 0.04,
	pinSize: 0.1,
};
