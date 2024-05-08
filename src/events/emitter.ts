import type { EventData } from "./emitter.interface";
import EventEmitter from "events";

class EventEmitterWrapper<TEvents> {
  private emitter: EventEmitter;

  constructor() {
    this.emitter = new EventEmitter();
  }

  public emit<TEventName extends keyof TEvents & string>(
    eventName: TEventName,
    eventArgs: TEvents[TEventName]
  ) {
    this.emitter.emit(eventName, eventArgs);
  }

  public on<TEventName extends keyof TEvents & string>(
    eventName: TEventName,
    callback: (args: TEvents[TEventName]) => void
  ) {
    this.emitter.on(eventName, callback);
  }

  public off<TEventName extends keyof TEvents & string>(
    eventName: TEventName,
    callback: (args: TEvents[TEventName]) => void
  ) {
    this.emitter.off(eventName, callback);
  }
}

export const emitter = new EventEmitterWrapper<EventData>();
