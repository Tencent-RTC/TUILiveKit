import 'package:flutter/foundation.dart';
import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';

import '../../common/index.dart';
import 'barrage_input_panel_widget.dart';
import 'barrage_send_controller.dart';

class BarrageSendWidget extends StatefulWidget {
  final BarrageSendController controller;
  final BuildContext? parentContext;

  const BarrageSendWidget({super.key, required this.controller, this.parentContext});

  @override
  State<BarrageSendWidget> createState() => _BarrageSendWidgetState();
}

class _BarrageSendWidgetState extends State<BarrageSendWidget> {
  bool _isShowingInputWidget = false;
  late final VoidCallback _floatWindowModeChangedListener = _onFloatWindowModeChanged;

  @override
  void initState() {
    super.initState();
    widget.controller.isFloatWindowMode.addListener(_floatWindowModeChangedListener);
  }

  @override
  void dispose() {
    _autoCloseInputWidget();
    widget.controller.isFloatWindowMode.removeListener(_floatWindowModeChangedListener);
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return SizedBox(
      height: 36,
      child: ElevatedButton(
        onPressed: () async {
          showInputWidget(widget.parentContext ?? context, widget.controller);
        },
        style: ButtonStyle(
          padding: WidgetStateProperty.all(const EdgeInsets.all(0)),
          backgroundColor:
          WidgetStateProperty.all<Color>(BarrageColors.barrageLightGrey),
          shape: WidgetStateProperty.all(
            RoundedRectangleBorder(
              borderRadius: BorderRadius.circular(18),
            ),
          ),
        ),
        child: Row(
          mainAxisAlignment: MainAxisAlignment.spaceAround,
          children: [
            Text(
              BarrageLocalizations.of(context)!.barrage_let_us_chat,
              style: const TextStyle(
                fontSize: 12,
                fontWeight: FontWeight.w400,
                color: BarrageColors.barrageTextGrey,
                overflow: TextOverflow.ellipsis,
              ),
            ),
            Image.asset(
              BarrageImages.emojiIcon,
              package: Constants.pluginName,
              width: 18.33,
              height: 18.33,
            ),
          ],
        ),
      ),
    );
  }

  void showInputWidget(BuildContext context, BarrageSendController controller) {
    _isShowingInputWidget = true;
    showModalBottomSheet(
      useRootNavigator: true,
      context: context,
      barrierColor: Colors.transparent,
      builder: (context) => BarrageInputPanelWidget(controller: controller),
    ).then((value) => _isShowingInputWidget = false);
  }

  void _autoCloseInputWidget() {
    // Mock a tap event
    final Offset tapPosition = Offset(1, 1);
    final PointerAddedEvent addEvent = PointerAddedEvent(pointer: 0, position: tapPosition);
    final PointerDownEvent downEvent = PointerDownEvent(pointer: 0, position: tapPosition);
    final PointerUpEvent upEvent = PointerUpEvent(pointer: 0, position: tapPosition);
    GestureBinding.instance.handlePointerEvent(addEvent);
    GestureBinding.instance.handlePointerEvent(downEvent);
    GestureBinding.instance.handlePointerEvent(upEvent);
  }

  void _onFloatWindowModeChanged() {
    if (widget.controller.isFloatWindowMode.value) {
      _autoCloseInputWidget();
    }
  }
}