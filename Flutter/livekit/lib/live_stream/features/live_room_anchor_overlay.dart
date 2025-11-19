import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/common/widget/float_window/float_window_widget.dart';

import '../../component/float_window/index.dart';
import '../../tencent_live_uikit.dart';

class TUILiveRoomAnchorOverlay extends StatefulWidget {
  final String roomId;
  final bool needPrepare;
  final TUILiveInfo? liveInfo;
  final VoidCallback? onStartLive;

  const TUILiveRoomAnchorOverlay(
      {super.key, required this.roomId, this.needPrepare = true, this.liveInfo, this.onStartLive});

  @override
  State<StatefulWidget> createState() {
    return TUILiveRoomAnchorOverlayState();
  }
}

class TUILiveRoomAnchorOverlayState extends State<TUILiveRoomAnchorOverlay> {
  @override
  void initState() {
    super.initState();
    WidgetsBinding.instance.addPostFrameCallback((_) {
      if (GlobalFloatWindowManager.instance.isFloating()) {
        GlobalFloatWindowState state = GlobalFloatWindowManager.instance.state;
        if (state.ownerId.value == TUIRoomEngine.getSelfInfo().userId) {
          makeToast(msg: LiveKitLocalizations.of(Global.appContext())!.livelist_exit_float_window_tip);
          Navigator.pop(context);
          return;
        } else {
          GlobalFloatWindowManager.instance.overlayManager.closeOverlay();
        }
      }
      if (Global.secondaryNavigatorKey.currentState == null) {
        Navigator.pop(context);
        LiveKitLogger.error("TUILiveRoomAnchorOverlay error: Global.secondaryNavigatorKey is invalid!");
        return;
      }
      final overlayEntry = OverlayEntry(builder: (context) => buildOverlayContent());
      Global.secondaryNavigatorKey.currentState!.overlay!.insert(overlayEntry);
      GlobalFloatWindowManager.instance.setRoomId(widget.roomId);
      GlobalFloatWindowManager.instance.setOwnerId(TUIRoomEngine.getSelfInfo().userId);
      GlobalFloatWindowManager.instance.overlayManager.showOverlayEntry(overlayEntry);
      Navigator.pop(context);
    });
  }

  Widget buildOverlayContent() {
    return FloatWindowWidget(builder: (context, controller) {
      switchToFullScreenMode() {controller.onTapSwitchFloatWindowInApp(false);}
      GlobalFloatWindowManager.instance.overlayManager.setSwitchToFullScreenCallback(switchToFullScreenMode);
      return TUILiveRoomAnchorWidget(
          roomId: widget.roomId,
          needPrepare: widget.needPrepare,
          liveInfo: widget.liveInfo,
          onStartLive: widget.onStartLive,
          floatWindowController: controller);
    });
  }

  @override
  Widget build(BuildContext context) {
    return const SizedBox.shrink();
  }
}
