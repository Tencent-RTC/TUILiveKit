import 'package:live_stream_core/live_core_widget/index.dart';
import 'package:live_uikit_barrage/live_uikit_barrage.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/constants/constants.dart';
import 'package:tencent_live_uikit/common/language/gen/livekit_localizations.dart';
import 'package:tencent_live_uikit/common/resources/colors.dart';
import 'package:tencent_live_uikit/common/resources/images.dart';
import 'package:tencent_live_uikit/common/screen/index.dart';
import 'package:tencent_live_uikit/common/widget/action_sheet.dart';
import 'package:tencent_live_uikit/common/widget/global.dart';
import 'package:tencent_live_uikit/component/gift_access/gift_barrage_item_builder.dart';
import 'package:tencent_live_uikit/live_stream/features/audience/living_widget/audience_bottom_menu_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/decorations/co_guest/co_guest_waiting_agree_widget.dart';
import 'package:tencent_live_uikit/live_stream/manager/live_stream_manager.dart';

import '../../../../common/widget/toast.dart';
import '../../../../component/audience_list/index.dart';
import '../../../../component/live_info/index.dart';
import '../../../../common/error/index.dart';

class AudienceLivingWidget extends StatefulWidget {
  final LiveCoreController liveCoreController;
  final LiveStreamManager liveStreamManager;

  const AudienceLivingWidget({
    super.key,
    required this.liveCoreController,
    required this.liveStreamManager,
  });

  @override
  State<StatefulWidget> createState() => _AudienceLivingWidgetState();
}

class _AudienceLivingWidgetState extends State<AudienceLivingWidget> {
  BarrageDisplayController? _barrageDisplayController;
  GiftDisplayController? _giftDisplayController;

  @override
  void initState() {
    super.initState();
    widget.liveStreamManager.userState.enterUser
        .addListener(_onRemoteUserEnterRoom);
  }

  @override
  void dispose() {
    widget.liveStreamManager.userState.enterUser
        .removeListener(_onRemoteUserEnterRoom);
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final screenWidth = 1.screenWidth;
    final screenHeight = 1.screenHeight;
    return Stack(
      children: [
        _buildTopMeanWidget(),
        _buildCoGuestWaitingAgreeWidget(),
        _buildBarrageDisplayWidget(screenWidth),
        _buildGiftDisplayWidget(screenWidth, screenHeight),
        _buildBottomMenuWidget(screenWidth),
      ],
    );
  }

  Widget _buildTopMeanWidget() {
    return Positioned(
      left: 16.width,
      top: 54.height,
      right: 16.width,
      child: SizedBox(
        height: 40.height,
        child: Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: [
            LiveInfoWidget(
              roomId: widget.liveCoreController.roomState.roomId,
            ),
            Row(
              children: [
                AudienceListWidget(
                  roomId: widget.liveCoreController.roomState.roomId,
                ),
                SizedBox(width: 8.width), // Add spacing between widgets
                SizedBox(
                  width: 24.width,
                  height: 24.height,
                  child: GestureDetector(
                    onTap: () {
                      _onCloseIconTap();
                    },
                    child: Image.asset(
                      LiveImages.audienceClose,
                      package: Constants.pluginName,
                      fit: BoxFit.contain,
                    ),
                  ),
                ),
              ],
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildCoGuestWaitingAgreeWidget() {
    return Positioned(
        right: 8.width,
        top: 116.height,
        child: CoGuestWaitingAgreeWidget(
          liveCoreController: widget.liveCoreController,
          liveStreamManager: widget.liveStreamManager,
        ));
  }

  Widget _buildBarrageDisplayWidget(double screenWidth) {
    return Positioned(
      left: 16.width,
      bottom: 80.height,
      height: 182.height,
      width: screenWidth - 72.width,
      child: ValueListenableBuilder(
        valueListenable: widget.liveCoreController.roomState.liveStatus,
        builder: (BuildContext context, value, Widget? child) {
          if (widget.liveCoreController.roomState.liveStatus.value !=
              LiveStatus.playing) {
            return const SizedBox.shrink();
          }

          _initBarrageDisPlayController();
          return BarrageDisplayWidget(controller: _barrageDisplayController!);
        },
      ),
    );
  }

  Widget _buildGiftDisplayWidget(double screenWidth, double screenHeight) {
    return Positioned(
      left: 0,
      top: 0,
      width: screenWidth,
      height: screenHeight,
      child: ValueListenableBuilder(
        valueListenable: widget.liveCoreController.roomState.liveStatus,
        builder: (BuildContext context, value, Widget? child) {
          if (widget.liveCoreController.roomState.liveStatus.value !=
              LiveStatus.playing) {
            return const SizedBox.shrink();
          }

          _initGiftDisPlayController();
          return GiftDisplayWidget(controller: _giftDisplayController!);
        },
      ),
    );
  }

  Widget _buildBottomMenuWidget(double screenWidth) {
    return Positioned(
      left: 0,
      bottom: 34.height,
      height: 36.height,
      width: screenWidth,
      child: AudienceBottomMenuWidget(
        liveCoreController: widget.liveCoreController,
        liveStreamManager: widget.liveStreamManager,
      ),
    );
  }
}

extension on _AudienceLivingWidgetState {
  void _initBarrageDisPlayController() {
    _barrageDisplayController ??= BarrageDisplayController(
        roomId: widget.liveCoreController.roomState.roomId,
        ownerId: widget.liveCoreController.roomState.ownerInfo.userId,
        selfUserId: widget.liveCoreController.userState.selfInfo.userId,
        selfName: widget.liveCoreController.userState.selfInfo.userName,
        onError: (code, message) {
          makeToast(msg: ErrorHandler.convertToErrorMessage(code, message) ?? '');
        });
  }

  void _initGiftDisPlayController() {
    if (_giftDisplayController != null) {
      return;
    }

    _initBarrageDisPlayController();

    _barrageDisplayController?.setCustomBarrageBuilder(GiftBarrageItemBuilder(
      selfUserId: widget.liveCoreController.userState.selfInfo.userId,
    ));

    final ownerInfo = GiftUser(
      userId: widget.liveCoreController.roomState.ownerInfo.userId,
      avatarUrl: widget.liveCoreController.roomState.ownerInfo.avatarUrl,
      userName: widget.liveCoreController.roomState.ownerInfo.userName,
      level: "66",
    );

    final selfInfo = GiftUser(
      userId: widget.liveCoreController.userState.selfInfo.userId,
      avatarUrl: widget.liveCoreController.userState.selfInfo.avatarUrl,
      userName: widget.liveCoreController.userState.selfInfo.userName,
      level: "32",
    );

    _giftDisplayController = GiftDisplayController(
      roomId: widget.liveCoreController.roomState.roomId,
      owner: ownerInfo,
      self: selfInfo,
    );
    _giftDisplayController?.setGiftCallback(
      onReceiveGiftCallback: _insertToBarrageMessage,
      onSendGiftCallback: _insertToBarrageMessage,
    );
  }

  void _onRemoteUserEnterRoom() {
    final userInfo = widget.liveStreamManager.userState.enterUser.value;
    BarrageUser barrageUser = BarrageUser();
    barrageUser.userId = userInfo.userId;
    barrageUser.userName =
        userInfo.userName.isNotEmpty ? userInfo.userName : userInfo.userId;
    barrageUser.avatarUrl = userInfo.avatarUrl;
    barrageUser.level = "66";

    Barrage barrage = Barrage();
    barrage.user = barrageUser;
    barrage.content =
        LiveKitLocalizations.of(Global.appContext())!.common_entered_room;
    _barrageDisplayController?.insertMessage(barrage);
  }

  void _insertToBarrageMessage(GiftMessage message) {
    Barrage barrage = Barrage();
    barrage.content = "gift";
    barrage.user.userId = message.sender?.userId ?? "";
    barrage.user.userName =
        message.sender?.userName ?? message.sender?.userId ?? "";
    barrage.user.avatarUrl = message.sender?.avatarUrl ?? "";
    barrage.user.level = message.sender?.level ?? "66";
    barrage.extInfo[Constants.keyGiftViewType] = Constants.valueGiftViewType;
    barrage.extInfo[Constants.keyGiftName] = message.gift?.giftName;
    barrage.extInfo[Constants.keyGiftCount] = message.giftCount;
    barrage.extInfo[Constants.keyGiftImage] = message.gift?.imageUrl;
    barrage.extInfo[Constants.keyGiftReceiverUserId] =
        message.receiver?.userId ?? "";
    barrage.extInfo[Constants.keyGiftReceiverUsername] =
        message.receiver?.userName ?? message.receiver?.userId ?? "";
    _barrageDisplayController?.insertMessage(barrage);
  }

  void _onCloseIconTap() {
    if (widget.liveCoreController.coGuestState.coGuestStatus.value !=
        CoGuestStatus.linking) {
      widget.liveCoreController.leaveLiveStream();
      Navigator.pop(context);
      return;
    }

    final actionSheetItems = [
      ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(Global.appContext())!
            .common_text_terminate_connection_tips,
        textStyle: const TextStyle(
          color: LiveColors.notStandardWhite30Transparency,
          fontSize: 12,
          fontWeight: FontWeight.w400,
        ),
        lineHeight: 1.height,
        bingData: 1,
      ),
      ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(Global.appContext())!
            .common_text_terminate_connection,
        textStyle: const TextStyle(
          color: LiveColors.designStandardFlowkitRed,
          fontSize: 16,
          fontWeight: FontWeight.w500,
        ),
        lineHeight: 1.height,
        bingData: 2,
      ),
      ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(Global.appContext())!
            .common_text_leave_room,
        textStyle: const TextStyle(
          color: LiveColors.designStandardFlowkitWhite,
          fontSize: 16,
          fontWeight: FontWeight.w500,
        ),
        lineHeight: 7.height,
        bingData: 3,
      ),
      ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(Global.appContext())!.common_cancel,
        isShowBottomLine: false,
        bingData: 4,
      ),
    ];

    ActionSheet.show(actionSheetItems, (ActionSheetModel model) async {
      switch (model.bingData) {
        case 2:
          widget.liveCoreController.terminateIntraRoomConnection();
          break;
        case 3:
          widget.liveCoreController.leaveLiveStream();
          Navigator.pop(context);
          break;
        default:
          break;
      }
    });
  }
}
