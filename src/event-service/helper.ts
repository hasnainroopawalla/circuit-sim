import { emitter } from "./emitter";
import { EmitterEvent } from "./emitter.interface";

export class EmitterHelper {
  public static notification(text: string) {
    emitter.emit(EmitterEvent.Notification, {
      text,
    });
  }
}
