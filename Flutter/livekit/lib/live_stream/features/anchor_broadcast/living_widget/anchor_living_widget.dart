import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/index.dart' hide LiveStatus;
import 'package:live_stream_core/live_core_widget/live_core_controller.dart';
import 'package:live_uikit_barrage/live_uikit_barrage.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/live_stream/features/anchor_broadcast/co_guest/anchor_co_guest_float_widget.dart';

import '../../../../common/error/index.dart';
import '../../../../common/language/index.dart';
import '../../../../common/screen/index.dart';
import '../../../../common/constants/constants.dart';
import '../../../../common/resources/index.dart';
import '../../../../common/widget/index.dart';
import '../../../../component/audience_list/index.dart';
import '../../../../component/beauty/index.dart';
import '../../../../component/gift_access/gift_barrage_item_builder.dart';
import '../../../../component/live_info/index.dart';
import '../../../manager/live_stream_manager.dart';
import '../../../live_define.dart';
import 'anchor_bottom_menu_widget.dart';

class AnchorLivingWidget extends StatefulWidget {
  final LiveStreamManager liveStreamManager;
  final LiveCoreController liveCoreController;

  const AnchorLivingWidget(
      {super.key,
      required this.liveStreamManager,
      required this.liveCoreController});

  @override
  State<AnchorLivingWidget> createState() => _AnchorLivingWidgetState();
}

class _AnchorLivingWidgetState extends State<AnchorLivingWidget> {
  late final LiveStreamManager liveStreamManager;
  late final LiveCoreController liveCoreController;
  BarrageDisplayController? _barrageDisplayController;
  GiftDisplayController? _giftDisplayController;

  @override
  void initState() {
    super.initState();
    liveStreamManager = widget.liveStreamManager;
    liveCoreController = widget.liveCoreController;
    _addObserver();
  }

  @override
  void dispose() {
    _removeObserver();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Stack(children: [
      _buildCloseWidget(),
      _buildAudienceListWidget(),
      _buildLiveInfoWidget(),
      _buildBarrageDisplayWidget(),
      _buildGiftDisplayWidget(),
      _buildAnchorBottomMenuWidget(),
      _buildApplyLinkAudienceWidget()
    ]);
  }

  Widget _buildCloseWidget() {
    return Positioned(
      right: 10.width,
      top: 68.height,
      width: 24.width,
      height: 24.width,
      child: GestureDetector(
        onTap: () {
          _closeButtonClick();
        },
        child: Image.asset(
          LiveImages.close,
          package: Constants.pluginName,
        ),
      ),
    );
  }

  Widget _buildAudienceListWidget() {
    return Positioned(
        right: 38.width,
        top: 68.height,
        child: Container(
          constraints: BoxConstraints(maxWidth: 107.width),
          child: ValueListenableBuilder(
              valueListenable: liveStreamManager.roomState.liveStatus,
              builder: (context, liveStatus, _) {
                return Visibility(
                  visible: liveStatus == LiveStatus.pushing,
                  child: AudienceListWidget(
                    roomId: liveStreamManager.roomState.roomId,
                  ),
                );
              }),
        ));
  }

  Widget _buildLiveInfoWidget() {
    return Positioned(
        left: 16.width,
        top: 60.height,
        child: Container(
          constraints:
              BoxConstraints(maxHeight: 40.height, maxWidth: 200.width),
          child: ValueListenableBuilder(
              valueListenable: liveStreamManager.roomState.liveStatus,
              builder: (context, liveStatus, _) {
                return Visibility(
                  visible: liveStatus == LiveStatus.pushing,
                  child: LiveInfoWidget(
                    roomId: liveStreamManager.roomState.roomId,
                  ),
                );
              }),
        ));
  }

  Widget _buildBarrageDisplayWidget() {
    return Positioned(
        left: 16.height,
        bottom: 84.height,
        height: 224.height,
        width: 305.width,
        child: ValueListenableBuilder(
          valueListenable: liveStreamManager.roomState.liveStatus,
          builder: (context, liveStatus, _) {
            if (liveStatus != LiveStatus.pushing) {
              return Container();
            }
            if (_barrageDisplayController == null) {
              _barrageDisplayController = BarrageDisplayController(
                  roomId: liveStreamManager.roomState.roomId,
                  ownerId: liveStreamManager.coreRoomState.ownerInfo.userId,
                  selfUserId: liveStreamManager.coreUserState.selfInfo.userId,
                  selfName: liveStreamManager.coreUserState.selfInfo.userName);
              _barrageDisplayController?.setCustomBarrageBuilder(
                  GiftBarrageItemBuilder(
                      selfUserId:
                          liveStreamManager.coreUserState.selfInfo.userId));
            }
            return BarrageDisplayWidget(controller: _barrageDisplayController!);
          },
        ));
  }

  Widget _buildGiftDisplayWidget() {
    return Positioned(
        width: 1.screenWidth,
        height: 1.screenHeight,
        child: ValueListenableBuilder(
          valueListenable: liveStreamManager.roomState.liveStatus,
          builder: (context, liveStatus, _) {
            if (liveStatus != LiveStatus.pushing) {
              return Container();
            }
            if (_giftDisplayController == null) {
              GiftUser ownerInfo = GiftUser(
                  userId: liveStreamManager.coreRoomState.ownerInfo.userId,
                  avatarUrl:
                      liveStreamManager.coreRoomState.ownerInfo.avatarUrl,
                  userName: liveStreamManager.coreRoomState.ownerInfo.userName,
                  level: "66");

              GiftUser selfInfo = GiftUser(
                  userId: liveStreamManager.coreUserState.selfInfo.userId,
                  avatarUrl: liveStreamManager.coreUserState.selfInfo.avatarUrl,
                  userName: liveStreamManager.coreUserState.selfInfo.userName,
                  level: "32");

              _giftDisplayController = GiftDisplayController(
                  roomId: liveStreamManager.coreRoomState.roomId,
                  owner: ownerInfo,
                  self: selfInfo);
              _giftDisplayController?.setGiftCallback(
                  onReceiveGiftCallback: _insertToBarrageMessage,
                  onSendGiftCallback: _insertToBarrageMessage);
            }
            return GiftDisplayWidget(controller: _giftDisplayController!);
          },
        ));
  }

  Widget _buildAnchorBottomMenuWidget() {
    return Positioned(
        left: 0,
        bottom: 36.height,
        child: SizedBox(
            width: 1.screenWidth,
            height: 46.height,
            child: AnchorBottomMenuWidget(
                liveStreamManager: liveStreamManager,
                liveCoreController: liveCoreController)));
  }

  Widget _buildApplyLinkAudienceWidget() {
    return Positioned(
      right: 8.width,
      top: 116.height,
      height: 86.height,
      width: 114.width,
      child: AnchorCoGuestFloatWidget(
        liveStreamManager: liveStreamManager,
        liveCoreController: liveCoreController,
      ),
    );
  }
}

extension on _AnchorLivingWidgetState {
  void _addObserver() {
    liveStreamManager.userState.enterUser.addListener(_onRemoteUserEnterRoom);
  }

  void _removeObserver() {
    liveStreamManager.userState.enterUser
        .removeListener(_onRemoteUserEnterRoom);
  }

  void _onRemoteUserEnterRoom() {
    final userInfo = liveStreamManager.userState.enterUser.value;
    BarrageUser barrageUser = BarrageUser();
    barrageUser.userId = userInfo.userId;
    barrageUser.userName = userInfo.userName;
    barrageUser.avatarUrl = userInfo.avatarUrl;

    Barrage barrage = Barrage();
    barrage.user = barrageUser;
    barrage.content =
        LiveKitLocalizations.of(Global.appContext())!.common_entered_room;
    _barrageDisplayController?.insertMessage(barrage);
  }

  void _closeButtonClick() {
    String title = '';
    final selfUserId = liveStreamManager.coreUserState.selfInfo.userId;
    final isSelfInBattle = liveStreamManager.battleState.battleUsers.value
        .any((user) => user.userId == selfUserId);
    final isSelfInCoHost =
        liveStreamManager.coHostState.connectedUsers.value.length > 1;
    final isSelfInCoGuest = liveStreamManager.coreCoGuestState.seatList.value
        .where((user) => user.userId.isNotEmpty && user.userId != selfUserId)
        .toList()
        .isNotEmpty;

    const endBattleNumber = 1;
    const endCoHostNumber = 2;
    const endLiveNumber = 3;
    const cancelNumber = 4;
    final List<ActionSheetModel> menuData = List.empty(growable: true);

    const lineColor = LiveColors.designStandardG8;
    if (isSelfInBattle) {
      title = LiveKitLocalizations.of(context)!.common_end_pk_tips;
      final endBattle = ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(context)!.common_end_pk,
          textStyle:
              const TextStyle(color: LiveColors.notStandardRed, fontSize: 16),
          lineColor: lineColor,
          bingData: endBattleNumber);
      menuData.add(endBattle);
    } else if (isSelfInCoHost) {
      title = LiveKitLocalizations.of(context)!.common_end_connection_tips;
      final endCoHost = ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(context)!.common_end_connection,
          textStyle:
              const TextStyle(color: LiveColors.notStandardRed, fontSize: 16),
          lineColor: lineColor,
          bingData: endCoHostNumber);
      menuData.add(endCoHost);
    } else if (isSelfInCoGuest) {
      title = LiveKitLocalizations.of(context)!.common_anchor_end_link_tips;
    }

    final endLive = ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(context)!.common_end_live,
        textStyle:
            const TextStyle(color: LiveColors.designStandardG2, fontSize: 16),
        lineColor: lineColor,
        bingData: endLiveNumber);
    menuData.add(endLive);

    final cancel = ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(Global.appContext())!.common_cancel,
        textStyle:
            const TextStyle(color: LiveColors.designStandardG2, fontSize: 16),
        lineColor: lineColor,
        bingData: cancelNumber);
    menuData.add(cancel);

    ActionSheet.show(menuData, (model) {
      switch (model.bingData) {
        case endBattleNumber:
          _exitBattle();
          break;
        case endCoHostNumber:
          _exitCoHost();
          break;
        case endLiveNumber:
          _stopLiveStream();
          break;
        default:
          break;
      }
    }, backgroundColor: LiveColors.designStandardFlowkitWhite, title: title);
  }

  void _exitBattle() {
    liveCoreController
        .terminateBattle(liveStreamManager.battleState.battleId.value);
  }

  void _exitCoHost() {
    liveCoreController.terminateCrossRoomConnection();
    liveStreamManager.onCrossRoomConnectionTerminated();
  }

  void _stopLiveStream() async {
    liveCoreController
        .terminateBattle(liveCoreController.battleState.battleId.value);

    final future = liveCoreController.stopLiveStream();
    BarrageDisplayController.resetState();
    GiftDisplayController.resetState();

    final result = await future;
    if (result.code != TUIError.success) {
      liveStreamManager.toastSubject.add(ErrorHandler.convertToErrorMessage(
              result.code.rawValue, result.message) ??
          '');
    }
    liveStreamManager.onStopLive();
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
}
