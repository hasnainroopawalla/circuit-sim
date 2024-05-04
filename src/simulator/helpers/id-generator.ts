class IdGenerator {
  current: number;

  constructor() {
    this.current = -1;
  }

  public reset(): void {
    this.current = -1;
  }

  public inputChipId(): string {
    return `chip.input.${this.generate()}`;
  }

  public outputChipId(): string {
    return `chip.output.${this.generate()}`;
  }

  public chipId(chipName: string): string {
    return `chip.${chipName}.${this.generate()}`;
  }

  private generate(): number {
    this.current += 1;
    return this.current;
  }
}

export const idGenerator = new IdGenerator();
