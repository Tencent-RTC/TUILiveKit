import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/live_core_controller.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/live_stream/manager/live_stream_manager.dart';
import 'package:tencent_live_uikit/live_stream/state/co_guest_state.dart';

import '../../../../tencent_live_uikit.dart';
import '../panel/co_guest_type_select_panel_widget.dart';

class AudienceEmptySeatWidget extends StatelessWidget {
  final SeatFullInfo seatFullInfo;
  final LiveCoreController liveCoreController;
  final LiveStreamManager liveStreamManager;

  const AudienceEmptySeatWidget(
      {super.key, required this.seatFullInfo, required this.liveCoreController, required this.liveStreamManager});

  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
        valueListenable: liveStreamManager.floatWindowState.isFloatWindowMode,
        builder: (context, isFloatWindowMode, child) {
          return Visibility(
            visible: !isFloatWindowMode,
            child: GestureDetector(
              onTap: () {
                _showCoGuestPanelWidget(seatFullInfo.seatIndex);
              },
              child: Container(
                alignment: Alignment.center,
                decoration: BoxDecoration(
                    color: LiveColors.grayDark2, border: Border.all(color: LiveColors.black6, width: 0.5)),
                child: Column(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    const Spacer(flex: 1),
                    Image.asset(
                      LiveImages.add,
                      package: Constants.pluginName,
                      width: 18.radius,
                      height: 18.radius,
                    ),
                    SizedBox(height: 4.height),
                    Text(
                      LiveKitLocalizations.of(context)!.common_apply_connection,
                      style: const TextStyle(color: LiveColors.designStandardFlowkitWhite, fontSize: 14),
                    ),
                    const Spacer(flex: 1),
                  ],
                ),
              ),
            ),
          );
        });
  }

  void _showCoGuestPanelWidget(int seatIndex) {
    final coGuestStatus = liveStreamManager.coGuestState.coGuestStatus.value;
    if (coGuestStatus == CoGuestStatus.applying || coGuestStatus == CoGuestStatus.linking) {
      LiveKitLogger.info("can't link, coGuestStatus:$coGuestStatus");
    } else {
      popupWidget(CoGuestTypeSelectPanelWidget(
        liveCoreController: liveCoreController,
        seatIndex: seatIndex,
      ));
    }
  }
}
