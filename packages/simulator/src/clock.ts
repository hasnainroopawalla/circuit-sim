const FRAME_TIME_ELEMENT_ID = "frametime";
const SHOW_FRAME_TIME_TICKS = 10;

type ClockProps = { showFrameTime: boolean };

export class Clock {
	public tickCount: number;
	private lastFrameMs: number;
	private showFrameTime: boolean;

	constructor({ showFrameTime }: ClockProps) {
		this.showFrameTime = showFrameTime;
		this.lastFrameMs = Date.now();
		this.tickCount = 0;
	}

	/**
	 * Returns the deltaTime in seconds.
	 */
	public tick(): number {
		const now = Date.now();
		const deltaTime = (now - this.lastFrameMs) / 1000;
		this.lastFrameMs = now;
		this.tickCount++;

		this.showFrameTime && this.displayFrameTime(deltaTime);

		return deltaTime;
	}

	public displayFrameTime(deltaTime: number): void {
		if (this.tickCount % SHOW_FRAME_TIME_TICKS === 0) {
			(
				document.getElementById(FRAME_TIME_ELEMENT_ID) as HTMLElement
			).innerHTML = (deltaTime * 1000).toString();
		}
	}
}
