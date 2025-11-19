import 'package:flutter/foundation.dart';
import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';
import 'package:live_uikit_gift/state/tui_gift_store.dart';

import '../../../common/index.dart';
import 'gift_list_controller.dart';
import 'gift_list_widget.dart';

class GiftSendWidget extends StatefulWidget {
  final GiftListController controller;
  final BuildContext? parentContext;

  const GiftSendWidget({super.key, required this.controller, this.parentContext});

  @override
  State<GiftSendWidget> createState() => _GiftSendWidgetState();
}

class _GiftSendWidgetState extends State<GiftSendWidget> {
  bool _isShowingGiftPanelWidget = false;
  late final VoidCallback _floatWindowModeChangedListener = _onFloatWindowModeChanged;

  @override
  void initState() {
    super.initState();
    widget.controller.isFloatWindowMode.addListener(_floatWindowModeChangedListener);
  }

  @override
  void dispose() {
    _autoCloseGiftPanelWidget();
    widget.controller.isFloatWindowMode.removeListener(_floatWindowModeChangedListener);
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return IconButton(onPressed: () => showGiftPanelWidget(widget.parentContext ?? context, widget.controller),
        padding: EdgeInsets.zero,
        icon: Image.asset(
          GiftImages.giftSendIcon,
          package: Constants.pluginName,
          fit: BoxFit.fill,
        ));
  }

  void showGiftPanelWidget(BuildContext context, GiftListController controller) {
    _isShowingGiftPanelWidget = true;
    showModalBottomSheet(
      context: context,
      useRootNavigator: true,
      barrierColor: Colors.transparent,
      builder: (context) => GiftListWidget(giftListController: controller),
    ).then((value) => _isShowingGiftPanelWidget = false);
  }

  void _autoCloseGiftPanelWidget() {
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
      _autoCloseGiftPanelWidget();
    }
  }
}
