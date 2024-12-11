import 'package:barrage/barrage.dart';
import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/state/live_define.dart';

import 'link/select_link_mic_type_panel_widget.dart';

class AudienceFunctionWidget extends BasicWidget {
  const AudienceFunctionWidget({super.key, required super.liveController});

  @override
  AudienceFunctionWidgetState getState() {
    return AudienceFunctionWidgetState();
  }
}

class AudienceFunctionWidgetState extends BasicState<AudienceFunctionWidget> {
  late BarrageSendController _sendController;

  @override
  Widget build(BuildContext context) {
    return Stack(children: [
      _initBarrageSendWidget(),
      _initLinkMicWidget(),
    ]);
  }

  _initBarrageSendWidget() {
    return Positioned(
      left: 15,
      top: 0,
      width: 130,
      height: 36,
      child: ValueListenableBuilder(
          valueListenable: liveController.getRoomSate().enterRoomSuccess,
          builder: (BuildContext context, bool value, Widget? child) {
            if (liveController.getRoomSate().enterRoomSuccess.value) {
              _sendController = BarrageSendController(
                  roomId: liveController.getRoomSate().roomId,
                  ownerId: liveController.getRoomSate().ownerInfo.userId,
                  selfUserId: liveController.getUserState().selfInfo.userId,
                  selfName: liveController.getUserState().selfInfo.name.value);
              return BarrageSendWidget(controller: _sendController);
            } else {
              return Container();
            }
          },
      )
    );
  }

  _initLinkMicWidget() {
    return ValueListenableBuilder(
        valueListenable: liveController.getViewState().linkStatus,
        builder: (BuildContext context, LinkStatus value, Widget? child) {
          return Positioned(
              right: 20,
              top: 2,
              width: 32,
              height: 32,
              child: GestureDetector(
                onTap: () {
                  switch (liveController.getViewState().linkStatus.value) {
                    case LinkStatus.none:
                      _showLinkMicPanelWidget();
                      break;
                    case LinkStatus.applying:
                      _showCancelRequestPanelWidget();
                      break;
                    case LinkStatus.linking:
                      _showCloseLinkPanelWidget();
                      break;
                  }
                },
                child: Image.asset(
                  _getImageByLinkStatus(),
                  package: Constants.pluginName,
                ),
              ));
        });
  }
}

extension AudienceFunctionWidgetStateLogicExtension on AudienceFunctionWidgetState {
  _showBarrageSendWidget() {}

  _showLinkMicPanelWidget() {
    showWidget(SelectLinkMicTypePanelWidget(liveController: liveController));
  }

  void _showCancelRequestPanelWidget() {
    List<ActionSheetModel> list = [
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.livekit_text_cancel_link_mic_apply,
          textStyle: const TextStyle(
            color: LivekitColors.livekitDesignStandardFlowkitRed,
            fontSize: 16,
            fontWeight: FontWeight.w700,
          ),
          lineHeight: 3,
          bingData: 1),
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.livekit_cancel,
          isShowBottomLine: false,
          bingData: 2),
    ];
    ActionSheet.show(list, (ActionSheetModel model) async {
      if (model.bingData == 1) {
        liveController.seatController.cancelTakeSeatApplication();
      }
    });
  }

  void _showCloseLinkPanelWidget() {
    List<ActionSheetModel> list = [
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.livekit_text_close_link_mic,
          textStyle: const TextStyle(
            color: LivekitColors.livekitDesignStandardFlowkitRed,
            fontSize: 16,
            fontWeight: FontWeight.w700,
          ),
          lineHeight: 3,
          bingData: 1),
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.livekit_cancel,
          isShowBottomLine: false,
          bingData: 2),
    ];
    ActionSheet.show(list, (ActionSheetModel model) async {
      if (model.bingData == 1) {
        liveController.seatController.leaveSeat();
      }
    });
  }

  String _getImageByLinkStatus() {
    switch (liveController.getViewState().linkStatus.value) {
      case LinkStatus.none:
        return LivekitImages.livekitFunctionLinkDefault;
      case LinkStatus.applying:
        return LivekitImages.livekitFunctionRequest;
      case LinkStatus.linking:
        return LivekitImages.livekitFunctionLinked;
    }
  }
}
