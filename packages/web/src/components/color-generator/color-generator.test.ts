import { colorGenerator } from "./color-generator";
import { colors } from "./colors";

const generatedColors: string[] = [];

describe("ColorGenerator", () => {
	test.each(Array.from(Array(colors.length).keys()))(
		"generated color is not repeated",
		() => {
			const generatedColor = colorGenerator.generate();
			expect(generatedColors).not.toContain(generatedColor);
			generatedColors.push(generatedColor);
		},
	);
});
