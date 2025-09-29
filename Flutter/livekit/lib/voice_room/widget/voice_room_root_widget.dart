import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';
import 'package:live_stream_core/live_stream_core.dart';
import 'package:live_uikit_barrage/live_uikit_barrage.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/common/screen/index.dart';
import 'package:tencent_live_uikit/live_navigator_observer.dart';
import 'package:tencent_live_uikit/voice_room/widget/panel/seat_invitation_panel_widget.dart';
import 'package:tencent_live_uikit/voice_room/widget/panel/user_management_panel_widget.dart';

import '../../common/constants/index.dart';
import '../../common/error/index.dart';
import '../../common/language/index.dart';
import '../../common/resources/index.dart';
import '../../common/widget/index.dart';
import '../../component/gift_access/gift_barrage_item_builder.dart';
import '../index.dart';
import 'end_statistics/anchor_end_statistics_widget.dart' as voice_end_anchor;
import 'end_statistics/audience_end_statistics_widget.dart' as voice_end_audience;
import 'end_statistics/end_statistics_widget_define.dart' as voice_end_define;

typedef ShowEndViewCallback = void Function(Map<String, dynamic> endInfo, bool isAnchor);

class VoiceRoomRootWidget extends StatefulWidget {
  final String roomId;
  final VoiceRoomManager manager;
  final SeatGridController seatGridController;
  final bool isCreate;

  const VoiceRoomRootWidget(
      {super.key,
      required this.roomId,
      required this.manager,
      required this.seatGridController,
      required this.isCreate});

  @override
  State<VoiceRoomRootWidget> createState() => _VoiceRoomRootWidgetState();
}

class _VoiceRoomRootWidgetState extends State<VoiceRoomRootWidget> {
  late final String roomId;
  late final VoiceRoomManager manager;
  late final SeatGridController seatGridController;
  late final bool isCreate;
  late final bool isOwner;
  final ValueNotifier<bool> enterRoomSuccess = ValueNotifier(false);
  late BarrageSendController _barrageSendController;
  BarrageDisplayController? _barrageDisplayController;
  GiftPlayController? _giftPlayController;
  late final SeatGridWidgetObserver seatGridObserver;
  bool isShowingAlert = false;

  late Size _screenSize;

  @override
  void initState() {
    super.initState();
    roomId = widget.roomId;
    manager = widget.manager;
    seatGridController = widget.seatGridController;
    isCreate = widget.isCreate;
    isOwner = widget.isCreate;
    if (isCreate) {
      _start(roomId: roomId);
    } else {
      _join(roomId: roomId);
    }
    _addObserver();
  }

  @override
  void dispose() {
    _removeObserver();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    _screenSize = MediaQuery.sizeOf(context);
    return PopScope(
      canPop: false,
      child: SizedBox(
        width: _screenSize.width,
        height: _screenSize.height,
        child: Stack(children: [
          _initBackgroundWidget(),
          _initBackgroundGradientWidget(),
          _initBarrageDisplayWidget(),
          _initSeatGridWidget(),
          _initGiftDisplayWidget(),
          _initTopWidget(),
          _initBottomMenuWidget(),
          _initBarrageInputWidget(),
          _initMuteMicrophoneWidget(),
          _buildEndStatisticsWidget()
        ]),
      ),
    );
  }

  Widget _initBackgroundWidget() {
    return SizedBox(
        width: _screenSize.width,
        height: _screenSize.height,
        child: ValueListenableBuilder(
            valueListenable: manager.roomState.backgroundUrl,
            builder: (context, value, child) {
              return CachedNetworkImage(
                  imageUrl: value,
                  fit: BoxFit.cover,
                  placeholder: (context, url) {
                    return Image.asset(LiveImages.defaultBackground, package: Constants.pluginName, fit: BoxFit.cover);
                  });
            }));
  }

  Widget _initBackgroundGradientWidget() {
    return Container(
        width: _screenSize.width,
        height: _screenSize.height,
        decoration: BoxDecoration(
            gradient: LinearGradient(colors: [
          LiveColors.designStandardG1,
          LiveColors.designStandardG1.withAlpha(0x80),
          LiveColors.designStandardG1
        ], begin: Alignment.topCenter, end: Alignment.bottomCenter)));
  }

  Widget _initBarrageDisplayWidget() {
    return Positioned(
        left: 16.width,
        bottom: 84.height,
        child: SizedBox(
          width: 305.width,
          height: 224.height,
          child: ValueListenableBuilder(
              valueListenable: enterRoomSuccess,
              builder: (context, success, child) {
                if (!success) {
                  return Container();
                }

                _barrageDisplayController ??= BarrageDisplayController(
                    roomId: manager.roomState.roomId,
                    ownerId: manager.roomState.ownerInfo.userId,
                    selfUserId: manager.userState.selfInfo.userId,
                    selfName: manager.userState.selfInfo.name);
                _barrageDisplayController
                    ?.setCustomBarrageBuilder(GiftBarrageItemBuilder(selfUserId: manager.userState.selfInfo.userId));
                return BarrageDisplayWidget(controller: _barrageDisplayController!);
              }),
        ));
  }

  Widget _initSeatGridWidget() {
    return Positioned(
        top: 122.height,
        child: SizedBox(
            width: _screenSize.width,
            height: 245.height,
            child: SeatGridWidget(
                controller: seatGridController,
                onSeatWidgetTap: (seatInfo) {
                  _onTapSeatGridWidget(seatInfo);
                })));
  }

  Widget _initGiftDisplayWidget() {
    return ValueListenableBuilder(
      valueListenable: enterRoomSuccess,
      builder: (context, success, child) {
        if (!success) {
          return Container();
        }
        if (_giftPlayController == null) {
          _giftPlayController = GiftPlayController(
              roomId: manager.roomState.roomId, language: DeviceLanguage.getCurrentLanguageCode(context));
          _giftPlayController?.onReceiveGiftCallback = _insertToBarrageMessage;
        }
        return GiftPlayWidget(giftPlayController: _giftPlayController!);
      },
    );
  }

  Widget _initTopWidget() {
    return ValueListenableBuilder(
      valueListenable: enterRoomSuccess,
      builder: (context, success, child) {
        return Visibility(
          visible: success,
          child: Positioned(
              top: 54.height,
              left: 12.width,
              right: 12.width,
              child: SizedBox(
                width: _screenSize.width,
                height: 40.height,
                child: TopWidget(
                    manager: manager,
                    onTapTopWidget: (tapEvent) {
                      _onTapTopWidget(tapEvent);
                    }),
              )),
        );
      },
    );
  }

  Widget _initBottomMenuWidget() {
    return Positioned(
        right: 27.width,
        bottom: 36.height,
        child: SizedBox(
            width: isOwner ? 72.width : 152.width,
            height: 46.height,
            child: BottomMenuWidget(manager: manager, seatGridController: seatGridController, isOwner: isOwner)));
  }

  Widget _initBarrageInputWidget() {
    return Positioned(
        left: 15.width,
        bottom: 36.height,
        child: SizedBox(
          height: 36.height,
          width: 130.width,
          child: ValueListenableBuilder(
            valueListenable: enterRoomSuccess,
            builder: (context, value, child) {
              if (!enterRoomSuccess.value) {
                return Container();
              }
              _barrageSendController = BarrageSendController(
                  roomId: manager.roomState.roomId,
                  ownerId: manager.roomState.ownerInfo.userId,
                  selfUserId: manager.userState.selfInfo.userId,
                  selfName: manager.userState.selfInfo.name);
              return BarrageSendWidget(controller: _barrageSendController);
            },
          ),
        ));
  }

  Widget _initMuteMicrophoneWidget() {
    return Positioned(
        left: 153.width,
        bottom: 38.height,
        child: Center(
          child: ListenableBuilder(
              listenable:
                  Listenable.merge([seatGridController.userState.hasAudioStreamUserList, manager.seatState.seatList]),
              builder: (context, child) {
                final hasAudioUserIdList = Set<String>.from(seatGridController.userState.hasAudioStreamUserList.value);
                final selfUserId = seatGridController.userState.selfInfo.userId;
                final hasAudio = hasAudioUserIdList.any((hasAudioUserId) => hasAudioUserId == selfUserId);
                final imageUrl = hasAudio ? LiveImages.openMicrophone : LiveImages.closeMicrophone;
                return Visibility(
                  visible: manager.seatState.seatList.value
                      .any((seatInfo) => seatInfo.userId == manager.userState.selfInfo.userId),
                  child: Container(
                    width: 32.radius,
                    height: 32.radius,
                    decoration: BoxDecoration(
                        shape: BoxShape.circle,
                        border: Border.all(color: LiveColors.designStandardWhite7.withAlpha(0x1A), width: 1)),
                    child: IconButton(
                        onPressed: () {
                          _muteMicrophone(hasAudio);
                        },
                        iconSize: 20.radius,
                        padding: EdgeInsets.zero,
                        icon: Image.asset(
                          imageUrl,
                          package: Constants.pluginName,
                          width: 20.radius,
                          height: 20.radius,
                        )),
                  ),
                );
              }),
        ));
  }

  Widget _buildEndStatisticsWidget() {
    return ValueListenableBuilder(
        valueListenable: manager.roomState.exitRoom,
        builder: (context, exitRoom, child) {
          var endInfo = voice_end_define.AnchorEndStatisticsWidgetInfo(
              roomId: manager.roomState.roomId,
              liveDuration: ((DateTime.now().millisecondsSinceEpoch - manager.roomState.createTime) ~/ 1000).abs(),
              viewCount: manager.roomState.liveExtraInfo.value.maxAudienceCount,
              messageCount: manager.roomState.liveExtraInfo.value.messageCount,
              giftTotalCoins: manager.roomState.liveExtraInfo.value.giftIncome,
              giftTotalUniqueSender: manager.roomState.liveExtraInfo.value.giftSenderCount,
              likeTotalUniqueSender: manager.roomState.liveExtraInfo.value.likeCount);
          if (isOwner) {
            return Visibility(
                visible: exitRoom, child: voice_end_anchor.AnchorEndStatisticsWidget(endWidgetInfo: endInfo));
          } else {
            final roomId = manager.roomState.roomId;
            final userName = manager.roomState.ownerInfo.name;
            final avatarUrl = manager.roomState.ownerInfo.avatarUrl;
            return Visibility(
                visible: exitRoom,
                child: voice_end_audience.AudienceEndStatisticsWidget(
                    roomId: roomId, userName: userName, avatarUrl: avatarUrl));
          }
        });
  }
}

extension _RoomOperation on _VoiceRoomRootWidgetState {
  void _start({required roomId}) async {
    final liveInfo = TUILiveInfo();
    liveInfo.roomId = roomId;
    final roomState = manager.roomState;
    liveInfo.name = roomState.roomName.value;
    liveInfo.seatMode = roomState.seatMode.value;
    liveInfo.maxSeatCount = roomState.maxSeatCount.value;
    liveInfo.isSeatEnabled = true;
    liveInfo.keepOwnerOnSeat = true;
    liveInfo.coverUrl = roomState.coverUrl.value;
    liveInfo.backgroundUrl = roomState.backgroundUrl.value;
    liveInfo.isPublicVisible = roomState.liveExtraInfo.value.liveMode.value == PrivacyStatus.publicity;
    final result = await seatGridController.startVoiceRoomV2(liveInfo);
    if (result.code == TUIError.success && result.data != null) {
      return _onStartSuccess(result.data!);
    }
    _toastAndPopup();
  }

  void _join({required roomId}) async {
    final result = await seatGridController.joinVoiceRoomV2(roomId);
    if (result.code == TUIError.success && result.data != null) {
      return _didEnterRoom(result.data!);
    }
    _toastAndPopup();
  }

  void _onStartSuccess(TUILiveInfo liveInfo) {
    manager.onLiveCreateTimeChanged(liveInfo.createTime);
    manager.onRoomOwnerInfoChanged(manager.userState.selfInfo);
    manager.setRoomSeatModeByAdmin(manager.roomState.seatMode.value);
    return _didEnterRoom(liveInfo);
  }

  void _didEnterRoom(TUILiveInfo liveInfo) {
    _initTopWidget();
    manager.fetchUserList();
    manager.fetchSeatList();
    final owner = User(
        userId: liveInfo.ownerId,
        name: liveInfo.ownerName,
        avatarUrl: liveInfo.ownerAvatarUrl,
        role: TUIRole.roomOwner);
    manager.onRoomOwnerInfoChanged(owner);
    enterRoomSuccess.value = true;

    if (!isOwner) {
      manager.fetchLiveInfo(roomId);
      manager.checkFollowType(liveInfo.ownerId);
    }
  }

  void _toastAndPopup() {
    manager.toastSubject.add(LiveKitLocalizations.of(Global.appContext())!.common_server_error_room_does_not_exist);
    if (mounted) {
      Navigator.of(context).pop();
    }
  }
}

extension _MediaOperation on _VoiceRoomRootWidgetState {
  void _startMicrophone() {
    seatGridController.startMicrophone();
  }

  void _stopMicrophone() {
    seatGridController.stopMicrophone();
  }

  void _muteMicrophone(bool mute) async {
    if (mute) {
      seatGridController.muteMicrophone();
      return;
    }

    final result = await seatGridController.unmuteMicrophone();
    if (result.code != TUIError.success) {
      manager.toastSubject.add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
    }
  }
}

extension _TopWidgetTapEventHandler on _VoiceRoomRootWidgetState {
  void _onTapTopWidget(TopWidgetTapEvent event) {
    switch (event) {
      case TopWidgetTapEvent.stop:
        isOwner ? _showExitConfirmPanel() : _normalUserLeave();
        break;
      case TopWidgetTapEvent.audienceList:
        break;
      case TopWidgetTapEvent.liveInfo:
        break;
      default:
        break;
    }
  }

  void _showExitConfirmPanel() {
    const lineColor = LiveColors.designStandardWhite7;
    const textStyle = TextStyle(
      color: LiveColors.designStandardG2,
      fontSize: 16,
    );
    final List<ActionSheetModel> menuData = List.empty(growable: true);
    final takeOrMoveSeat = ActionSheetModel(
        text: LiveKitLocalizations.of(Global.appContext())!.common_end_live,
        textStyle: textStyle,
        lineColor: lineColor,
        bingData: 1);
    menuData.add(takeOrMoveSeat);

    final cancel = ActionSheetModel(
        text: LiveKitLocalizations.of(Global.appContext())!.common_cancel,
        textStyle: textStyle,
        lineColor: lineColor,
        bingData: 2);
    menuData.add(cancel);

    ActionSheet.show(menuData, (model) {
      if (model.bingData != 1) return;
      _roomOwnerLeave();
    }, backgroundColor: LiveColors.designStandardFlowkitWhite);
  }

  void _roomOwnerLeave() async {
    manager.onLinkStatusChanged(LinkStatus.none);
    final future = seatGridController.stopVoiceRoomV2();
    BarrageDisplayController.resetState();
    GiftPlayController.resetState();
    final result = await future;
    result.code == TUIError.success
        ? manager.onMicrophoneClosed()
        : manager.toastSubject.add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
    manager.onLiveEnd(result.data);
  }

  void _normalUserLeave() async {
    final selfUserId = manager.userState.selfInfo.userId;
    if (!manager.seatState.seatList.value.any((seatInfo) => seatInfo.userId == selfUserId)) {
      return _leaveRoom();
    }

    const lineColor = LiveColors.designStandardWhite7;
    const textStyle = TextStyle(
      color: LiveColors.designStandardG2,
      fontSize: 16,
    );

    const endLinkNumber = 1;
    const exitLiveNumber = 2;
    const cancelNumber = 3;
    final List<ActionSheetModel> menuData = List.empty(growable: true);
    final endLink = ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(Global.appContext())!.common_end_link,
        textStyle: const TextStyle(color: LiveColors.notStandardRed, fontSize: 16),
        lineColor: lineColor,
        bingData: endLinkNumber);
    menuData.add(endLink);

    final exitLive = ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(Global.appContext())!.common_exit_live,
        textStyle: textStyle,
        lineColor: lineColor,
        bingData: exitLiveNumber);
    menuData.add(exitLive);

    final cancel = ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(Global.appContext())!.common_cancel,
        textStyle: textStyle,
        lineColor: lineColor,
        bingData: cancelNumber);
    menuData.add(cancel);

    ActionSheet.show(menuData, (model) {
      switch (model.bingData) {
        case endLinkNumber:
          _leaveSeat();
          break;
        case exitLiveNumber:
          _leaveRoom();
          break;
        default:
          break;
      }
    },
        title: LiveKitLocalizations.of(Global.appContext())!.common_audience_end_link_tips,
        backgroundColor: LiveColors.designStandardFlowkitWhite);
  }

  Future<void> _leaveSeat() async {
    seatGridController.leaveSeat();
  }

  Future<void> _leaveRoom() async {
    manager.onLinkStatusChanged(LinkStatus.none);
    final future = seatGridController.leaveVoiceRoom();
    BarrageDisplayController.resetState();
    GiftPlayController.resetState();
    Navigator.of(context).pop();

    final result = await future;
    if (result.code != TUIError.success) {
      manager.toastSubject.add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
    }
  }
}

extension _SeatGridWidgetTapEventHandler on _VoiceRoomRootWidgetState {
  void _onTapSeatGridWidget(TUISeatInfo seatInfo) {
    showSeatOperationMenu(seatInfo);
  }

  void showSeatOperationMenu(TUISeatInfo seatInfo) {
    isOwner ? _showRoomOwnerSeatOperationMenu(seatInfo) : _showNormalUserSeatOperationMenu(seatInfo);
  }

  void _showRoomOwnerSeatOperationMenu(TUISeatInfo seatInfo) {
    if (seatInfo.userId.isEmpty) {
      return _showRoomOwnerEmptySeatOperationMenu(seatInfo);
    }

    final isSelf = seatInfo.userId == manager.userState.selfInfo.userId;
    if (!isSelf) {
      _showUserManagementPanel(seatInfo);
    }
  }

  void _showRoomOwnerEmptySeatOperationMenu(TUISeatInfo seatInfo) {
    const lineColor = LiveColors.designStandardWhite7;
    const textStyle = TextStyle(
      color: LiveColors.designStandardG2,
      fontSize: 16,
    );
    final List<ActionSheetModel> menuData = List.empty(growable: true);
    if (seatInfo.isLocked != null && !seatInfo.isLocked!) {
      final inviteToTakeSeat = ActionSheetModel(
          text: LiveKitLocalizations.of(Global.appContext())!.common_voiceroom_invite,
          textStyle: textStyle,
          lineColor: lineColor,
          autoPopSheet: false,
          bingData: 1);
      menuData.add(inviteToTakeSeat);
    }

    final isSeatLocked = seatInfo.isLocked ?? false;
    final lockSeat = ActionSheetModel(
        text: isSeatLocked
            ? LiveKitLocalizations.of(Global.appContext())!.common_voiceroom_unlock
            : LiveKitLocalizations.of(Global.appContext())!.common_voiceroom_lock,
        textStyle: textStyle,
        lineColor: lineColor,
        bingData: 2);
    menuData.add(lockSeat);

    final cancel = ActionSheetModel(
        text: LiveKitLocalizations.of(Global.appContext())!.common_cancel,
        textStyle: textStyle,
        lineColor: lineColor,
        bingData: 3);
    menuData.add(cancel);

    ActionSheet.show(menuData, (model) {
      switch (model.bingData) {
        case 1:
          Navigator.of(context).pop();
          _showSeatInvitationPanel(seatInfo);
          break;
        case 2:
          _lockSeat(seatInfo);
        default:
          break;
      }
    }, backgroundColor: LiveColors.designStandardFlowkitWhite);
  }

  void _showSeatInvitationPanel(TUISeatInfo seatInfo) {
    popupWidget(
        SeatInvitationPanelWidget(manager: manager, seatGridController: seatGridController, seatIndex: seatInfo.index));
  }

  void _lockSeat(TUISeatInfo seatInfo) {
    final lockParam = TUISeatLockParams();
    lockParam.lockSeat = !(seatInfo.isLocked ?? false);
    seatGridController.lockSeat(seatInfo.index, lockParam);
  }

  void _showUserManagementPanel(TUISeatInfo seatInfo) {
    popupWidget(
        UserManagementPanelWidget(manager: manager, seatGridController: seatGridController, seatInfo: seatInfo));
  }

  void _showNormalUserSeatOperationMenu(TUISeatInfo seatInfo) {
    final isLocked = seatInfo.isLocked ?? false;
    if (seatInfo.userId.isEmpty && !isLocked) {
      return _showNormalUserEmptySeatOperationMenu(seatInfo);
    }

    if (seatInfo.userId.isNotEmpty && seatInfo.userId != manager.userState.selfInfo.userId) {
      popupWidget(
          UserManagementPanelWidget(manager: manager, seatGridController: seatGridController, seatInfo: seatInfo));
    }
  }

  void _showNormalUserEmptySeatOperationMenu(TUISeatInfo seatInfo) {
    const lineColor = LiveColors.designStandardWhite7;
    const textStyle = TextStyle(
      color: LiveColors.designStandardG2,
      fontSize: 16,
    );
    final List<ActionSheetModel> menuData = List.empty(growable: true);
    final takeOrMoveSeat = ActionSheetModel(
        text: LiveKitLocalizations.of(Global.appContext())!.common_voiceroom_take_seat,
        textStyle: textStyle,
        lineColor: lineColor,
        bingData: 1);
    menuData.add(takeOrMoveSeat);

    final cancel = ActionSheetModel(
        text: LiveKitLocalizations.of(Global.appContext())!.common_cancel,
        textStyle: textStyle,
        lineColor: lineColor,
        bingData: 2);
    menuData.add(cancel);

    ActionSheet.show(menuData, (model) {
      if (model.bingData != 1) return;
      final isOnSeat =
          manager.seatState.seatList.value.any((seatInfo) => seatInfo.userId == manager.userState.selfInfo.userId);
      isOnSeat ? _moveToSeat(seatInfo) : _takeSeat(seatInfo);
    }, backgroundColor: LiveColors.designStandardFlowkitWhite);
  }

  void _moveToSeat(TUISeatInfo seatInfo) async {
    final result = await seatGridController.moveToSeat(seatInfo.index);
    if (result.code != TUIError.success) {
      manager.toastSubject.add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
    }
  }

  void _takeSeat(TUISeatInfo seatInfo) async {
    if (manager.seatState.isApplyingToTakeSeat.value) {
      return manager.toastSubject
          .add(LiveKitLocalizations.of(Global.appContext())!.common_client_error_request_id_repeat);
    }
    manager.onApplyingToSeatStateChanged(true);
    const timeoutValue = 60;
    final result = await seatGridController.takeSeat(seatInfo.index, timeoutValue);
    if (result.code == TUIError.success) {
      switch (result.type) {
        case RequestResultType.onAccepted:
          manager.onApplyingToSeatStateChanged(false);
          break;
        case RequestResultType.onRejected:
          manager.onApplyingToSeatStateChanged(false);
          manager.toastSubject.add(LiveKitLocalizations.of(Global.appContext())!.common_voiceroom_take_seat_rejected);
          break;
        case RequestResultType.onCancelled:
          manager.onApplyingToSeatStateChanged(false);
          break;
        case RequestResultType.onTimeout:
          manager.onApplyingToSeatStateChanged(false);
          manager.toastSubject.add(LiveKitLocalizations.of(Global.appContext())!.common_voiceroom_take_seat_timeout);
          break;
        default:
          break;
      }
    } else {
      manager.onApplyingToSeatStateChanged(false);
      manager.toastSubject.add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
    }
  }
}

extension _ObserverOperation on _VoiceRoomRootWidgetState {
  void _addObserver() {
    manager.userState.enterUser.addListener(_onRemoteUserEnterRoom);
    _initSeatGridObserver();
    seatGridController.addObserver(seatGridObserver);
    _addSeatStateObserver();
    _addUserStateObserver();
  }

  void _removeObserver() {
    seatGridController.removeObserver(seatGridObserver);
    manager.userState.enterUser.removeListener(_onRemoteUserEnterRoom);
    _removeSeatStateObserver();
    _removeUserStateObserver();
  }
}

extension _SeatGridObserver on _VoiceRoomRootWidgetState {
  void _initSeatGridObserver() {
    seatGridObserver = SeatGridWidgetObserver(onRoomDismissed: (roomId) {
      if (isOwner) return;
      manager.toastSubject.add(LiveKitLocalizations.of(Global.appContext())!.live_room_has_been_dismissed);
      manager.onLiveEnd(null);
    }, onSeatRequestReceived: (requestType, userInfo) {
      _handleReceivedRequest(requestType, userInfo);
    }, onSeatRequestCancelled: (requestType, userInfo) {
      _handleCancelledRequest(requestType, userInfo);
    }, onKickedOffSeat: (userInfo) {
      manager.toastSubject.add(LiveKitLocalizations.of(Global.appContext())!.common_voiceroom_kicked_out_of_seat);
    });
  }

  void _handleReceivedRequest(RequestType requestType, TUIUserInfo userInfo) {
    if (requestType == RequestType.applyToTakeSeat) {
      return manager.onSeatApplicationReceived(userInfo);
    }
    final alertInfo = AlertInfo(
        imageUrl: userInfo.avatarUrl,
        description: LiveKitLocalizations.of(Global.appContext())!
            .common_voiceroom_receive_seat_invitation
            .replaceAll('xxx', userInfo.userName),
        cancelActionInfo: (
          title: LiveKitLocalizations.of(Global.appContext())!.common_reject,
          titleColor: LiveColors.designStandardG3
        ),
        cancelCallback: () {
          _responseSeatInvitation(userInfo, false);
          isShowingAlert = false;
        },
        defaultActionInfo: (
          title: LiveKitLocalizations.of(Global.appContext())!.common_accept,
          titleColor: LiveColors.designStandardB1
        ),
        defaultCallback: () {
          _responseSeatInvitation(userInfo, true);
          isShowingAlert = false;
        });

    Alert.showAlert(alertInfo);
    isShowingAlert = true;
  }

  void _responseSeatInvitation(TUIUserInfo userInfo, bool agree) async {
    final result = await seatGridController.responseRemoteRequest(userInfo.userId, agree);
    if (result.code != TUIError.success) {
      manager.toastSubject.add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
    }

    if (mounted) Navigator.of(context).pop();
  }

  void _handleCancelledRequest(RequestType requestType, TUIUserInfo userInfo) {
    if (requestType == RequestType.applyToTakeSeat) {
      return manager.onSeatApplicationProcessed(userInfo);
    }
    if (isShowingAlert) Navigator.of(context).pop();
  }
}

extension _SubscribeState on _VoiceRoomRootWidgetState {
  void _addSeatStateObserver() {
    manager.seatState.seatList.addListener(_onSeatListChange);
  }

  void _removeSeatStateObserver() {
    manager.seatState.seatList.removeListener(_onSeatListChange);
  }

  void _onSeatListChange() {
    final isLinking =
        manager.seatState.seatList.value.any((seatInfo) => seatInfo.userId == manager.userState.selfInfo.userId);
    manager.onLinkStatusChanged(isLinking ? LinkStatus.linking : LinkStatus.none);
  }

  void _addUserStateObserver() {
    manager.userState.selfInfo.linkStatus.addListener(_onLinkStatusChanged);
  }

  void _removeUserStateObserver() {
    manager.userState.selfInfo.linkStatus.removeListener(_onLinkStatusChanged);
  }

  void _onLinkStatusChanged() {
    switch (manager.userState.selfInfo.linkStatus.value) {
      case LinkStatus.linking:
        _startMicrophone();
        _muteMicrophone(false);
        break;
      case LinkStatus.none:
        _stopMicrophone();
        break;
      default:
        break;
    }
  }
}

extension _BarrageOperation on _VoiceRoomRootWidgetState {
  void _onRemoteUserEnterRoom() {
    final userInfo = manager.userState.enterUser.value;
    BarrageUser barrageUser = BarrageUser();
    barrageUser.userId = userInfo.userId;
    barrageUser.userName = userInfo.name;
    barrageUser.avatarUrl = userInfo.avatarUrl;
    barrageUser.level = "66";

    Barrage barrage = Barrage();
    barrage.user = barrageUser;
    barrage.content = LiveKitLocalizations.of(Global.appContext())!.common_entered_room;
    _barrageDisplayController?.insertMessage(barrage);
  }

  void _insertToBarrageMessage(TUIGiftInfo giftInfo, int count, TUIUserInfo sender) {
    final receiverUserId = widget.seatGridController.roomState.ownerId;
    String receiverUserName = widget.seatGridController.roomState.ownerName;
    if (receiverUserId == widget.seatGridController.userState.selfInfo.userId) {
      receiverUserName = LiveKitLocalizations.of(Global.appContext())!.common_gift_me;
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
    barrage.extInfo[Constants.keyGiftReceiverUserId] = receiverUserId;

    barrage.extInfo[Constants.keyGiftReceiverUsername] = receiverUserName;
    _barrageDisplayController?.insertMessage(barrage);
  }
}
