import { EmitterEventArgs } from "./emitter.interface";
import EventEmitter from "events";

class CustomEventEmitter<TEvents> {
  private emitter = new EventEmitter();

  public emit<TEventName extends keyof TEvents & string>(
    eventName: TEventName,
    eventArgs?: TEvents[TEventName]
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

export const emitter = new CustomEventEmitter<EmitterEventArgs>();
