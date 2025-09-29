import 'package:live_stream_core/live_core_widget/live_core_controller.dart';
import 'package:live_stream_core/live_core_widget/state/co_guest_state.dart';
import 'package:live_uikit_barrage/live_uikit_barrage.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/live_stream/features/audience/panel/co_guest_type_select_panel_widget.dart';
import 'package:tencent_live_uikit/live_stream/manager/live_stream_manager.dart';

class AudienceBottomMenuWidget extends StatefulWidget {
  final LiveCoreController liveCoreController;
  final LiveStreamManager liveStreamManager;

  const AudienceBottomMenuWidget({
    super.key,
    required this.liveCoreController,
    required this.liveStreamManager,
  });

  @override
  State<StatefulWidget> createState() => _AudienceBottomMenuWidgetState();
}

class _AudienceBottomMenuWidgetState extends State<AudienceBottomMenuWidget> {
  BarrageSendController? _barrageSendController;
  GiftListController? _giftListController;
  LikeSendController? _likeSendController;

  @override
  void dispose() {
    _likeSendController?.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Stack(children: [
      _buildBarrageSendWidget(),
      Positioned(
        top: 2.height,
        right: 20.width,
        height: 32.height,
        width: 112.width,
        child: Row(
          mainAxisAlignment: MainAxisAlignment.end,
          mainAxisSize: MainAxisSize.min,
          children: [
            _buildGiftSendWidget(),
            SizedBox(width: 8.width),
            ValueListenableBuilder(
              valueListenable: widget.liveStreamManager.roomState.roomVideoStreamIsLandscape,
              builder: (BuildContext context, isLandScape, Widget? child) {
                return Visibility(
                    visible: !isLandScape,
                    child: Row(
                      children: [
                        _buildCoGuestWidget(),
                        SizedBox(width: 8.width),
                      ],
                    ));
              },
            ),
            _buildLikeSendWidget(),
          ],
        ),
      ),
    ]);
  }

  Widget _buildBarrageSendWidget() {
    _initBarrageSendController();
    return Positioned(
      left: 15.width,
      top: 0,
      width: 130.width,
      height: 36.height,
      child: BarrageSendWidget(controller: _barrageSendController!),
    );
  }

  Widget _buildCoGuestWidget() {
    return ListenableBuilder(
      listenable: Listenable.merge([
        widget.liveCoreController.coGuestState.coGuestStatus,
        widget.liveCoreController.coHostState.connectedUserList,
      ]),
      builder: (context, _) {
        bool isDisable = widget.liveCoreController.coHostState.connectedUserList.value.isNotEmpty;
        return SizedBox(
          width: 32.radius,
          height: 32.radius,
          child: GestureDetector(
            onTap: () => _handleCoGuestTap(widget.liveCoreController.coGuestState.coGuestStatus.value, isDisable),
            child: Image.asset(
              _getImageByCoGuestStatus(isDisable),
              package: Constants.pluginName,
            ),
          ),
        );
      },
    );
  }

  Widget _buildGiftSendWidget() {
    _initGiftSendController();
    return SizedBox(
      width: 32.radius,
      height: 32.radius,
      child: GiftSendWidget(controller: _giftListController!),
    );
  }

  Widget _buildLikeSendWidget() {
    _initLikeSendController();
    return SizedBox(
      width: 32.radius,
      height: 32.radius,
      child: LikeSendWidget(controller: _likeSendController!),
    );
  }

  void _handleCoGuestTap(CoGuestStatus status, bool isCoGuestDisable) {
    if (isCoGuestDisable) {
      return;
    }
    switch (status) {
      case CoGuestStatus.none:
        _showCoGuestPanelWidget();
        break;
      case CoGuestStatus.applying:
        _showCancelRequestPanelWidget();
        break;
      case CoGuestStatus.linking:
        _showCloseCoGuestPanelWidget();
        break;
    }
  }

  void _showCoGuestPanelWidget() {
    popupWidget(CoGuestTypeSelectPanelWidget(
      liveCoreController: widget.liveCoreController,
      seatIndex: -1,
    ));
  }
}

extension on _AudienceBottomMenuWidgetState {
  void _initBarrageSendController() {
    _barrageSendController ??= BarrageSendController(
      roomId: widget.liveCoreController.roomState.roomId,
      ownerId: widget.liveCoreController.roomState.ownerInfo.userId,
      selfUserId: widget.liveCoreController.userState.selfInfo.userId,
      selfName: widget.liveCoreController.userState.selfInfo.userName,
    );
  }

  void _initGiftSendController() {
    final language = DeviceLanguage.getCurrentLanguageCode(context);
    _giftListController ??= GiftListController(roomId: widget.liveCoreController.roomState.roomId, language: language);
  }

  void _initLikeSendController() {
    _likeSendController ??= LikeSendController(roomId: widget.liveCoreController.roomState.roomId);
  }

  void _showCancelRequestPanelWidget() {
    List<ActionSheetModel> list = [
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.common_text_cancel_link_mic_apply,
          textStyle: const TextStyle(
            color: LiveColors.designStandardFlowkitRed,
            fontSize: 16,
            fontWeight: FontWeight.w700,
          ),
          lineHeight: 3,
          bingData: 1),
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.common_cancel,
          isShowBottomLine: false,
          bingData: 2),
    ];
    ActionSheet.show(list, (ActionSheetModel model) async {
      if (model.bingData == 1) {
        widget.liveCoreController.cancelIntraRoomConnection(widget.liveCoreController.roomState.ownerInfo.userId);
      }
    });
  }

  void _showCloseCoGuestPanelWidget() {
    List<ActionSheetModel> list = [
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.common_text_close_link_mic,
          textStyle: const TextStyle(
            color: LiveColors.designStandardFlowkitRed,
            fontSize: 16,
            fontWeight: FontWeight.w700,
          ),
          lineHeight: 3,
          bingData: 1),
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.common_cancel,
          isShowBottomLine: false,
          bingData: 2),
    ];
    ActionSheet.show(list, (ActionSheetModel model) async {
      if (model.bingData == 1) {
        widget.liveCoreController.terminateIntraRoomConnection();
      }
    });
  }

  String _getImageByCoGuestStatus(bool isCoGuestDisabled) {
    if (isCoGuestDisabled) {
      return LiveImages.functionLinkDisable;
    }
    switch (widget.liveCoreController.coGuestState.coGuestStatus.value) {
      case CoGuestStatus.none:
        return LiveImages.functionLinkDefault;
      case CoGuestStatus.applying:
        return LiveImages.functionRequest;
      case CoGuestStatus.linking:
        return LiveImages.functionLinked;
    }
  }
}
