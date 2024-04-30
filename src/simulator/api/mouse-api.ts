export class MouseApi {
  private p: p5;
  private clickEvents: (() => void)[];

  constructor(p5: p5) {
    this.p = p5;
    this.clickEvents = [];
  }

  public get mouseX() {
    return this.p.mouseX;
  }

  public get mouseY() {
    return this.p.mouseY;
  }

  public attachClicked(callback: () => void) {
    this.clickEvents.push(callback);
  }

  public triggerClickEvents() {
    this.clickEvents.forEach((clickEvent) => {
      console.log(clickEvent);
      clickEvent();
    });
  }
}
