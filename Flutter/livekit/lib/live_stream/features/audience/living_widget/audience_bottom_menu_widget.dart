import 'package:live_stream_core/live_core_widget/live_core_controller.dart';
import 'package:live_stream_core/live_core_widget/state/co_guest_state.dart';
import 'package:live_uikit_barrage/live_uikit_barrage.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
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
  GiftSendController? _giftSendController;
  LikeSendController? _likeSendController;

  @override
  Widget build(BuildContext context) {
    return Stack(children: [
      _buildBarrageSendWidget(),
      _buildCoGuestWidget(),
      _buildGiftSendWidget(),
      _buildLikeSendWidget(),
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
        bool isDisable = widget
            .liveCoreController.coHostState.connectedUserList.value.isNotEmpty;
        return Positioned(
          right: 60.width,
          top: 2.height,
          width: 32.width,
          height: 32.height,
          child: GestureDetector(
            onTap: () => _handleCoGuestTap(
                widget.liveCoreController.coGuestState.coGuestStatus.value,
                isDisable),
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
    return Positioned(
      right: 100.width,
      top: 2.height,
      width: 32.width,
      height: 32.height,
      child: GiftSendWidget(controller: _giftSendController!),
    );
  }

  Widget _buildLikeSendWidget() {
    _initLikeSendController();
    return Positioned(
      right: 20.width,
      top: 2.height,
      width: 32.width,
      height: 32.height,
      child: LikeSendWidget(controller: _likeSendController!),
    );
  }

  GiftUser _createGiftUser(TUIUserInfo userInfo, String level) {
    return GiftUser(
      userId: userInfo.userId,
      avatarUrl: userInfo.avatarUrl,
      userName: userInfo.userName,
      level: level,
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
      liveStreamManager: widget.liveStreamManager,
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
    _giftSendController ??= GiftSendController(
      roomId: widget.liveCoreController.roomState.roomId,
      owner:
          _createGiftUser(widget.liveCoreController.roomState.ownerInfo, "66"),
      self: _createGiftUser(widget.liveCoreController.userState.selfInfo, "32"),
    );
  }

  void _initLikeSendController() {
    _likeSendController ??= LikeSendController(
      roomId: widget.liveCoreController.roomState.roomId,
      owner:
          _createGiftUser(widget.liveCoreController.roomState.ownerInfo, "66"),
      self: _createGiftUser(widget.liveCoreController.userState.selfInfo, "32"),
    );
  }

  void _showCancelRequestPanelWidget() {
    List<ActionSheetModel> list = [
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!
              .common_text_cancel_link_mic_apply,
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
        widget.liveCoreController.cancelIntraRoomConnection(
            widget.liveCoreController.roomState.ownerInfo.userId);
      }
    });
  }

  void _showCloseCoGuestPanelWidget() {
    List<ActionSheetModel> list = [
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!
              .common_text_close_link_mic,
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
