import 'dart:ui';

import 'package:flutter/gestures.dart';

class GestureMock {
  static void mockTapEvent() {
    //Mock a tap event to close the current popupWidget
    const Offset tapPosition = Offset(1, 1);
    const PointerAddedEvent addEvent = PointerAddedEvent(pointer: 0, position: tapPosition);
    const PointerDownEvent downEvent = PointerDownEvent(pointer: 0, position: tapPosition);
    const PointerUpEvent upEvent = PointerUpEvent(pointer: 0, position: tapPosition);
    GestureBinding.instance.handlePointerEvent(addEvent);
    GestureBinding.instance.handlePointerEvent(downEvent);
    GestureBinding.instance.handlePointerEvent(upEvent);
  }

  static void mockDoubleTapEvent() async {
    const Offset tapPosition = Offset(1, 1);
    const PointerAddedEvent addEvent = PointerAddedEvent(pointer: 0, position: tapPosition);
    const PointerDownEvent downEvent = PointerDownEvent(pointer: 0, position: tapPosition);
    const PointerUpEvent upEvent = PointerUpEvent(pointer: 0, position: tapPosition);
    GestureBinding.instance.handlePointerEvent(addEvent);
    GestureBinding.instance.handlePointerEvent(downEvent);
    GestureBinding.instance.handlePointerEvent(upEvent);
    await Future.delayed(const Duration(milliseconds: 50));
    GestureBinding.instance.handlePointerEvent(addEvent);
    GestureBinding.instance.handlePointerEvent(downEvent);
    GestureBinding.instance.handlePointerEvent(upEvent);
  }
}
