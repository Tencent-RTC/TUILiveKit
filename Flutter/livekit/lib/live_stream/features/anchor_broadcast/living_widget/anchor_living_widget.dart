import 'package:flutter/material.dart';
import 'package:live_stream_core/common/logger/logger.dart';
import 'package:live_stream_core/live_core_widget/index.dart' hide LiveStatus;
import 'package:live_stream_core/live_core_widget/live_core_controller.dart';
import 'package:live_uikit_barrage/live_uikit_barrage.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/component/float_window/global_float_window_manager.dart';
import 'package:tencent_live_uikit/component/network_info/index.dart';
import 'package:tencent_live_uikit/component/network_info/manager/network_info_manager.dart';
import 'package:tencent_live_uikit/live_stream/features/anchor_broadcast/co_guest/anchor_co_guest_float_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/anchor_broadcast/living_widget/anchor_user_management_panel_widget.dart';

import '../../../../common/constants/constants.dart';
import '../../../../common/error/index.dart';
import '../../../../common/language/index.dart';
import '../../../../common/resources/index.dart';
import '../../../../common/screen/index.dart';
import '../../../../common/widget/index.dart';
import '../../../../component/audience_list/index.dart';
import '../../../../component/gift_access/gift_barrage_item_builder.dart';
import '../../../../component/live_info/index.dart';
import '../../../live_define.dart';
import '../../../manager/live_stream_manager.dart';
import 'anchor_bottom_menu_widget.dart';

class AnchorLivingWidget extends StatefulWidget {
  final LiveStreamManager liveStreamManager;
  final LiveCoreController liveCoreController;
  final VoidCallback? onTapEnterFloatWindowInApp;

  const AnchorLivingWidget(
      {super.key, required this.liveStreamManager, required this.liveCoreController, this.onTapEnterFloatWindowInApp});

  @override
  State<AnchorLivingWidget> createState() => _AnchorLivingWidgetState();
}

class _AnchorLivingWidgetState extends State<AnchorLivingWidget> {
  late final LiveStreamManager liveStreamManager;
  late final LiveCoreController liveCoreController;
  BarrageDisplayController? _barrageDisplayController;
  GiftPlayController? _giftPlayController;
  final NetworkInfoManager _networkInfoManager = NetworkInfoManager();
  late final VoidCallback _userEnterRoomListener = _onRemoteUserEnterRoom;

  @override
  void initState() {
    super.initState();
    liveStreamManager = widget.liveStreamManager;
    liveCoreController = widget.liveCoreController;
    _addObserver();
  }

  @override
  void dispose() {
    _networkInfoManager.dispose();
    _removeObserver();
    enablePictureInPicture(false);
    super.dispose();
  }

  void enablePictureInPicture(bool enable) {
    if (GlobalFloatWindowManager.instance.isEnableFloatWindowFeature()) {
      final roomId = widget.liveCoreController.roomState.roomId;
      final jsonString = widget.liveStreamManager.buildEnablePipJsonParams(enable, roomId);
      widget.liveCoreController.enablePictureInPicture(jsonString).then((result) {
        LiveStreamCoreLogger.info("enablePictureInPicture,enable=$enable,result=$result");
        liveStreamManager.enablePipMode(enable && result);
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
        valueListenable: widget.liveStreamManager.floatWindowState.isFloatWindowMode,
        builder: (context, isFloatWindowMode, child) {
          return Visibility(
            visible: !isFloatWindowMode,
            child: Stack(children: [
              _buildPureBroadcastTapWidget(),
              _buildCloseWidget(),
              _buildFloatWindowWidget(),
              _buildAudienceListWidget(),
              _buildLiveInfoWidget(),
              _buildNetworkInfoButtonWidget(),
              _buildBarrageDisplayWidget(),
              _buildGiftDisplayWidget(),
              _buildAnchorBottomMenuWidget(),
              _buildApplyLinkAudienceWidget(),
              _buildNetworkToastWidget()
            ]),
          );
        });
  }

  Widget _buildPureBroadcastTapWidget() {
    return ListenableBuilder(
        listenable: Listenable.merge(
            [widget.liveCoreController.coGuestState.seatList, widget.liveCoreController.coHostState.connectedUserList]),
        builder: (context, _) {
          return _isPureAnchorBroadcast()
              ? GestureDetector(
                  onTap: () => onTapPureBroadcastTapWidget(),
                  child: Container(color: Colors.transparent))
              : Container();
        });
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

  Widget _buildFloatWindowWidget() {
    return Visibility(
      visible: GlobalFloatWindowManager.instance.isEnableFloatWindowFeature(),
      child: Positioned(
        right: 38.width,
        top: 68.height,
        width: 24.width,
        height: 24.width,
        child: GestureDetector(
          onTap: () {
            widget.onTapEnterFloatWindowInApp?.call();
          },
          child: Image.asset(
            LiveImages.floatWindow,
            package: Constants.pluginName,
          ),
        ),
      ),
    );
  }

  Widget _buildAudienceListWidget() {
    return Positioned(
        right: GlobalFloatWindowManager.instance.isEnableFloatWindowFeature() ? 66.width : 38.width,
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
                    onClickUserItem: (user) {
                      popupWidget(AnchorUserManagementPanelWidget(
                        panelType: AnchorUserManagementPanelType.messageAndKickOut,
                        user: user,
                        liveStreamManager: liveStreamManager,
                        liveCoreController: liveCoreController,
                      ));
                    },
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
          constraints: BoxConstraints(maxHeight: 40.height, maxWidth: 200.width),
          child: ValueListenableBuilder(
              valueListenable: liveStreamManager.roomState.liveStatus,
              builder: (context, liveStatus, _) {
                return Visibility(
                  visible: liveStatus == LiveStatus.pushing,
                  child: LiveInfoWidget(
                    roomId: liveStreamManager.roomState.roomId,
                    isFloatWindowMode: widget.liveStreamManager.floatWindowState.isFloatWindowMode,
                  ),
                );
              }),
        ));
  }

  Widget _buildNetworkInfoButtonWidget() {
    return Positioned(
        right: 12.width,
        top: 100.height,
        height: 20.height,
        width: 78.width,
        child: ValueListenableBuilder(
            valueListenable: liveStreamManager.roomState.liveStatus,
            builder: (context, liveStatus, _) {
              if (liveStatus != LiveStatus.pushing) {
                return Container();
              }
              return NetworkInfoButton(
                  manager: _networkInfoManager,
                  createTime: liveStreamManager.roomState.createTime,
                  isAudience: !liveStreamManager.roomState.liveInfo.keepOwnerOnSeat);
            }));
  }

  Widget _buildNetworkToastWidget() {
    return ValueListenableBuilder(
      valueListenable: _networkInfoManager.state.showToast,
      builder: (context, showToast, _) {
        return Center(
            child: Visibility(
                visible: showToast,
                child: NetworkStatusToastWidget(
                  manager: _networkInfoManager,
                )));
      },
    );
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
                  GiftBarrageItemBuilder(selfUserId: liveStreamManager.coreUserState.selfInfo.userId));
            }
            return BarrageDisplayWidget(
              controller: _barrageDisplayController!,
              onClickBarrageItem: (barrage) {
                final isOwner = widget.liveStreamManager.coreRoomState.ownerInfo.userId == barrage.user.userId;
                if (isOwner) {
                  return;
                }
                final user = TUIUserInfo(
                    userId: barrage.user.userId,
                    userName: barrage.user.userName,
                    avatarUrl: barrage.user.avatarUrl,
                    userRole: TUIRole.generalUser);
                popupWidget(AnchorUserManagementPanelWidget(
                  panelType: AnchorUserManagementPanelType.messageAndKickOut,
                  user: user,
                  liveStreamManager: liveStreamManager,
                  liveCoreController: liveCoreController,
                ));
              },
            );
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
            if (_giftPlayController == null) {
              _giftPlayController = GiftPlayController(
                  roomId: liveStreamManager.coreRoomState.roomId,
                  language: DeviceLanguage.getCurrentLanguageCode(context));
              _giftPlayController?.onReceiveGiftCallback = _insertToBarrageMessage;
            }
            return GiftPlayWidget(giftPlayController: _giftPlayController!);
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
            child:
                AnchorBottomMenuWidget(liveStreamManager: liveStreamManager, liveCoreController: liveCoreController)));
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
    liveStreamManager.userState.enterUser.addListener(_userEnterRoomListener);
  }

  void _removeObserver() {
    liveStreamManager.userState.enterUser.removeListener(_userEnterRoomListener);
  }

  void _onRemoteUserEnterRoom() {
    final userInfo = liveStreamManager.userState.enterUser.value;
    BarrageUser barrageUser = BarrageUser();
    barrageUser.userId = userInfo.userId;
    barrageUser.userName = userInfo.userName;
    barrageUser.avatarUrl = userInfo.avatarUrl;

    Barrage barrage = Barrage();
    barrage.user = barrageUser;
    barrage.content = LiveKitLocalizations.of(Global.appContext())!.common_entered_room;
    _barrageDisplayController?.insertMessage(barrage);
  }

  void _closeButtonClick() {
    String title = '';
    final selfUserId = liveStreamManager.coreUserState.selfInfo.userId;
    final isSelfInBattle = liveStreamManager.battleState.battleUsers.value.any((user) => user.userId == selfUserId);
    final isSelfInCoHost = liveStreamManager.coHostState.connectedUsers.value.length > 1;
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
          text: LiveKitLocalizations.of(context)!.common_battle_end_pk,
          textStyle: const TextStyle(color: LiveColors.notStandardRed, fontSize: 16),
          lineColor: lineColor,
          bingData: endBattleNumber);
      menuData.add(endBattle);
    } else if (isSelfInCoHost) {
      title = LiveKitLocalizations.of(context)!.common_end_connection_tips;
      final endCoHost = ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(context)!.common_end_connect,
          textStyle: const TextStyle(color: LiveColors.notStandardRed, fontSize: 16),
          lineColor: lineColor,
          bingData: endCoHostNumber);
      menuData.add(endCoHost);
    } else if (isSelfInCoGuest) {
      title = LiveKitLocalizations.of(context)!.common_anchor_end_link_tips;
    }

    final isObsBroadcast = !liveStreamManager.roomState.liveInfo.keepOwnerOnSeat;
    final leaveLiveText = isObsBroadcast
        ? LiveKitLocalizations.of(context)!.common_exit_live
        : LiveKitLocalizations.of(context)!.common_end_live;

    final endLive = ActionSheetModel(
        isCenter: true,
        text: leaveLiveText,
        textStyle: const TextStyle(color: LiveColors.designStandardG2, fontSize: 16),
        lineColor: lineColor,
        bingData: endLiveNumber);
    menuData.add(endLive);

    final cancel = ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(Global.appContext())!.common_cancel,
        textStyle: const TextStyle(color: LiveColors.designStandardG2, fontSize: 16),
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
    liveCoreController.terminateBattle(liveStreamManager.battleState.battleId.value);
  }

  void _exitCoHost() {
    liveCoreController.terminateCrossRoomConnection();
    liveStreamManager.onCrossRoomConnectionTerminated();
  }

  void _stopLiveStream() async {
    liveCoreController.terminateBattle(liveCoreController.battleState.battleId.value);

    final isObsBroadcast = !liveStreamManager.roomState.liveInfo.keepOwnerOnSeat;
    if (isObsBroadcast) {
      widget.liveCoreController.leaveLiveStream();
      Navigator.pop(context);
    } else {
      final future = liveCoreController.stopLiveStreamV2();
      BarrageDisplayController.resetState();
      GiftPlayController.resetState();

      final result = await future;
      if (result.code != TUIError.success) {
        liveStreamManager.toastSubject
            .add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
      }
      liveStreamManager.onStopLive();
    }
  }

  void _insertToBarrageMessage(TUIGiftInfo giftInfo, int count, TUIUserInfo sender) {
    final receiver = widget.liveCoreController.roomState.ownerInfo;
    if (receiver.userId == widget.liveCoreController.userState.selfInfo.userId) {
      receiver.userName = LiveKitLocalizations.of(Global.appContext())!.common_gift_me;
    }

    Barrage barrage = Barrage();
    barrage.content = "gift";
    barrage.user.userId = sender.userId;
    barrage.user.userName = sender.userName.isNotEmpty ? sender.userName : sender.userId;
    barrage.user.avatarUrl = sender.avatarUrl;
    barrage.extInfo[Constants.keyGiftViewType] = Constants.valueGiftViewType;
    barrage.extInfo[Constants.keyGiftName] = giftInfo.name;
    barrage.extInfo[Constants.keyGiftCount] = count;
    barrage.extInfo[Constants.keyGiftImage] = giftInfo.iconUrl;
    barrage.extInfo[Constants.keyGiftReceiverUserId] = receiver.userId;

    barrage.extInfo[Constants.keyGiftReceiverUsername] = receiver.userName;
    _barrageDisplayController?.insertMessage(barrage);
  }

  void onTapPureBroadcastTapWidget() {
    popupWidget(AnchorUserManagementPanelWidget(
      panelType: AnchorUserManagementPanelType.pureMedia,
      user: liveCoreController.userState.selfInfo,
      liveStreamManager: liveStreamManager,
      liveCoreController: liveCoreController,
    ));
  }

  bool _isPureAnchorBroadcast() {
    final selfUserId = widget.liveCoreController.userState.selfInfo.userId;
    return widget.liveCoreController.coGuestState.seatList.value
            .where((seat) => seat.userId.isNotEmpty && seat.userId != selfUserId)
            .isEmpty &&
        widget.liveCoreController.coHostState.connectedUserList.value.isEmpty;
  }
}
