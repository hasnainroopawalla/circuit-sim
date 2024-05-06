const ENTITY_ID_SEPARATOR = ".";
const ENTITY_ID_START_INDEX = -1;

class EntityIdService {
  current: number;

  constructor() {
    this.current = ENTITY_ID_START_INDEX;
  }

  public reset(): void {
    this.current = ENTITY_ID_START_INDEX;
  }

  public inputChipId(): string {
    return this.generate(["chip", "input"]);
  }

  public outputChipId(): string {
    return this.generate(["chip", "output"]);
  }

  public chipId(chipName: string): string {
    return this.generate(["chip", chipName]);
  }

  private getId(): number {
    this.current += 1;
    return this.current;
  }

  private generate(args: string[]): string {
    return [...args, this.getId()].join(ENTITY_ID_SEPARATOR);
  }
}

// Export a singleton instance of the service
export const entityIdService = new EntityIdService();
