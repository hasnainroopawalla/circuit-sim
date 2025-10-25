import type { Chip, IOChip, IOSlider } from "./chips";
import type { Pin } from "./pin";
import type { Wire } from "./wire";

export type Entity = IOChip | IOSlider | Pin | Chip | Wire;
