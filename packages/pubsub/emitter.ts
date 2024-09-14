import type { EventData } from "./events";
import EventEmitter from "events";

class EventEmitterWrapper<TEvents> {
  private emitter: EventEmitter;

  constructor() {
    this.emitter = new EventEmitter();
  }

  public publish<TEventName extends keyof TEvents & string>(
    eventName: TEventName,
    eventArgs: TEvents[TEventName]
  ) {
    this.emitter.emit(eventName, eventArgs);
  }

  public subscribe<TEventName extends keyof TEvents & string>(
    eventName: TEventName,
    callback: (args: TEvents[TEventName]) => void
  ) {
    this.emitter.on(eventName, callback);
  }

  public unsubscribe<TEventName extends keyof TEvents & string>(
    eventName: TEventName,
    callback: (args: TEvents[TEventName]) => void
  ) {
    this.emitter.off(eventName, callback);
  }
}

export const pubsub = new EventEmitterWrapper<EventData>();
