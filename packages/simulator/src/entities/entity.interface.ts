import { IOChip, IOSlider, Chip } from "./chips";
import { Pin } from "./pin";
import { Wire } from "./wire";

export type Entity = IOChip | IOSlider | Pin | Chip | Wire;
