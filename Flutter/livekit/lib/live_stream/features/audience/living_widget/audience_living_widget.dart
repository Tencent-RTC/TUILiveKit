import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:live_stream_core/common/logger/logger.dart';
import 'package:live_stream_core/live_core_widget/index.dart' hide LiveStatus;
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/component/float_window/global_float_window_manager.dart';
import 'package:tencent_live_uikit/component/index.dart';
import 'package:tencent_live_uikit/component/network_info/manager/network_info_manager.dart';
import 'package:tencent_live_uikit/live_stream/features/audience/living_widget/audience_bottom_menu_widget.dart';
import 'package:tencent_live_uikit/live_stream/features/decorations/co_guest/co_guest_waiting_agree_widget.dart';
import 'package:tencent_live_uikit/live_stream/manager/live_stream_manager.dart';
import 'package:tencent_live_uikit/live_stream/state/co_guest_state.dart' as ls_co_guest;
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

import '../../../../common/gesture/gesture_mock.dart';
import '../../../../component/network_info/index.dart';
import '../../../live_define.dart';
import '../panel/audience_user_info_panel_widget.dart';
import './player_menu_widget.dart';

class AudienceLivingWidget extends StatefulWidget {
  final LiveCoreController liveCoreController;
  final LiveStreamManager liveStreamManager;
  final VoidCallback? onTapEnterFloatWindowInApp;

  const AudienceLivingWidget(
      {super.key, required this.liveCoreController, required this.liveStreamManager, this.onTapEnterFloatWindowInApp});

  @override
  State<StatefulWidget> createState() => _AudienceLivingWidgetState();
}

class _AudienceLivingWidgetState extends State<AudienceLivingWidget> {
  BarrageDisplayController? _barrageDisplayController;
  GiftPlayController? _giftPlayController;
  final NetworkInfoManager _networkInfoManager = NetworkInfoManager();
  late final VoidCallback _userEnterRoomListener = _onRemoteUserEnterRoom;
  late final VoidCallback _playbackVideoQualityChangedListener = _onPlaybackVideoQualityChanged;
  TUIVideoQuality? playbackQuality;

  @override
  void initState() {
    super.initState();
    widget.liveStreamManager.userState.enterUser.addListener(_userEnterRoomListener);
    widget.liveStreamManager.mediaState.playbackQuality.addListener(_playbackVideoQualityChangedListener);
  }

  @override
  void dispose() {
    _networkInfoManager.dispose();
    widget.liveStreamManager.mediaState.playbackQuality.removeListener(_playbackVideoQualityChangedListener);
    widget.liveStreamManager.userState.enterUser.removeListener(_userEnterRoomListener);
    enablePictureInPicture(false);
    super.dispose();
  }

  void enablePictureInPicture(bool enable) {
    final roomId = widget.liveCoreController.roomState.roomId;
    final jsonString = widget.liveStreamManager.buildEnablePipJsonParams(enable, roomId);
    widget.liveCoreController.enablePictureInPicture(jsonString).then((result) {
      LiveStreamCoreLogger.info("enablePictureInPicture,enable=$enable,result=$result");
    });
  }

  @override
  Widget build(BuildContext context) {
    final screenWidth = 1.screenWidth;
    final screenHeight = 1.screenHeight;
    return ValueListenableBuilder(
        valueListenable: widget.liveStreamManager.floatWindowState.isFloatWindowMode,
        builder: (context, isFloatWindowMode, child) {
          return Visibility(
            visible: !isFloatWindowMode,
            child: GestureDetector(
              onTap: () => onTapAudienceLivingWidget(),
              child: Stack(
                children: [
                  _buildTopMeanWidget(context),
                  _buildNetworkInfoButtonWidget(),
                  _buildCoGuestWaitingAgreeWidget(),
                  _buildBarrageDisplayWidget(screenWidth, context),
                  _buildGiftDisplayWidget(screenWidth, screenHeight),
                  _buildBottomMenuWidget(screenWidth, context),
                  _buildRotateScreenButton(context),
                  _buildNetworkToastWidget()
                ],
              ),
            ),
          );
        });
  }

  Widget _buildTopMeanWidget(BuildContext context) {
    return Positioned(
      left: 16.width,
      top: MediaQuery.orientationOf(context) == Orientation.portrait ? 54.height : 20.width,
      right: 16.width,
      child: SizedBox(
        height: 40.height,
        child: Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: [
            LiveInfoWidget(
              roomId: widget.liveCoreController.roomState.roomId,
              isFloatWindowMode: widget.liveStreamManager.floatWindowState.isFloatWindowMode,
            ),
            Row(
              children: [
                AudienceListWidget(
                  roomId: widget.liveCoreController.roomState.roomId,
                  onClickUserItem: (user) {
                    final isSelf = widget.liveStreamManager.coreUserState.selfInfo.userId == user.userId;
                    if (!isSelf) {
                      popupWidget(AudienceUserInfoPanelWidget(user: user, liveStreamManager: widget.liveStreamManager),
                          backgroundColor: LiveColors.designStandardTransparent);
                    }
                  },
                ),
                Visibility(
                    visible: GlobalFloatWindowManager.instance.isEnableFloatWindowFeature(),
                    child: SizedBox(width: 8.width)), // Add spacing between widgets
                ValueListenableBuilder(
                    valueListenable: widget.liveStreamManager.mediaState.isRemoteVideoStreamPaused,
                    builder: (context, isRemoteVideoStreamPaused, _) {
                      return Visibility(
                        visible: GlobalFloatWindowManager.instance.isEnableFloatWindowFeature() &&
                            !isRemoteVideoStreamPaused,
                        child: SizedBox(
                          width: 24.width,
                          height: 24.height,
                          child: GestureDetector(
                            onTap: () {
                              if (MediaQuery.of(context).orientation == Orientation.portrait) {
                                widget.onTapEnterFloatWindowInApp?.call();
                              } else {
                                SystemChrome.setPreferredOrientations([
                                  DeviceOrientation.portraitUp,
                                ]).then((value) {
                                  Future.delayed(const Duration(milliseconds: 300), () {
                                    widget.onTapEnterFloatWindowInApp?.call();
                                  });
                                });
                              }
                            },
                            child: Image.asset(
                              LiveImages.floatWindow,
                              package: Constants.pluginName,
                              fit: BoxFit.contain,
                            ),
                          ),
                        ),
                      );
                    }),
                ValueListenableBuilder(
                    valueListenable: widget.liveStreamManager.mediaState.isRemoteVideoStreamPaused,
                    builder: (context, isRemoteVideoStreamPaused, _) {
                      return Visibility(visible: !isRemoteVideoStreamPaused, child: SizedBox(width: 8.width));
                    }),
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

  Widget _buildNetworkInfoButtonWidget() {
    return Positioned(
        right: 12.width,
        top: 100.height,
        height: 20.height,
        width: 78.width,
        child: ListenableBuilder(
            listenable: Listenable.merge(
                [widget.liveStreamManager.roomState.liveStatus, widget.liveStreamManager.coGuestState.coGuestStatus]),
            builder: (context, _) {
              final liveStatus = widget.liveStreamManager.roomState.liveStatus.value;
              if (liveStatus != LiveStatus.playing) {
                return Container();
              }
              final isOnSeat =
                  widget.liveStreamManager.coGuestState.coGuestStatus.value == ls_co_guest.CoGuestStatus.linking;
              return NetworkInfoButton(
                  manager: _networkInfoManager,
                  createTime: widget.liveStreamManager.roomState.createTime,
                  isAudience: !isOnSeat);
            }));
  }

  Widget _buildNetworkToastWidget() {
    return ValueListenableBuilder(
        valueListenable: _networkInfoManager.state.showToast,
        builder: (context, showToast, _) {
          return Center(
              child: Visibility(visible: showToast, child: NetworkStatusToastWidget(manager: _networkInfoManager)));
        });
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

  Widget _buildBarrageDisplayWidget(double screenWidth, BuildContext context) {
    final orientation = MediaQuery.orientationOf(context);
    return Positioned(
      left: orientation == Orientation.portrait ? 16.width : 35.height,
      bottom: orientation == Orientation.portrait ? 80.height : 20.width,
      height: 182.height,
      width: screenWidth - 72.width,
      child: ValueListenableBuilder(
        valueListenable: widget.liveCoreController.roomState.liveStatus,
        builder: (BuildContext context, value, Widget? child) {
          if (widget.liveStreamManager.roomState.liveStatus.value != LiveStatus.playing) {
            return const SizedBox.shrink();
          }

          _initBarrageDisPlayController();
          return BarrageDisplayWidget(
            controller: _barrageDisplayController!,
            onClickBarrageItem: (barrage) {
              final isSelf = widget.liveStreamManager.coreUserState.selfInfo.userId == barrage.user.userId;
              final isOwner = widget.liveStreamManager.coreRoomState.ownerInfo.userId == barrage.user.userId;
              final user = TUIUserInfo(
                  userId: barrage.user.userId,
                  userName: barrage.user.userName,
                  avatarUrl: barrage.user.avatarUrl,
                  userRole: isOwner ? TUIRole.roomOwner : TUIRole.generalUser);
              if (!isSelf) {
                popupWidget(AudienceUserInfoPanelWidget(user: user, liveStreamManager: widget.liveStreamManager),
                    backgroundColor: LiveColors.designStandardTransparent);
              }
            },
          );
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
          if (widget.liveStreamManager.roomState.liveStatus.value != LiveStatus.playing) {
            return const SizedBox.shrink();
          }

          _initGiftDisPlayController();
          return GiftPlayWidget(giftPlayController: _giftPlayController!);
        },
      ),
    );
  }

  void onTapAudienceLivingWidget() {
    if (!isPureViewingMode()) {
      return;
    }
    final orientation = MediaQuery.orientationOf(context);
    if (widget.liveStreamManager.roomState.roomVideoStreamIsLandscape.value && orientation == Orientation.landscape) {
      popupWidget(
        PlayerMenuWidget(liveStreamManager: widget.liveStreamManager),
        barrierColor: LiveColors.designStandardTransparent,
        backgroundColor: LiveColors.designStandardTransparent,
      );
    }
  }

  Widget _buildBottomMenuWidget(double screenWidth, BuildContext context) {
    return Positioned(
      left: 0,
      bottom: 34.height,
      height: 36.height,
      width: screenWidth,
      child: Visibility(
        visible: MediaQuery.orientationOf(context) == Orientation.portrait,
        child: AudienceBottomMenuWidget(
          liveCoreController: widget.liveCoreController,
          liveStreamManager: widget.liveStreamManager,
        ),
      ),
    );
  }

  Widget _buildRotateScreenButton(BuildContext context) {
    final orientation = MediaQuery.orientationOf(context);
    return Positioned(
      right: orientation == Orientation.portrait ? 10.width : 20.height,
      top: orientation == Orientation.portrait ? 475.height : 185.width,
      height: 32.radius,
      width: 32.radius,
      child: ListenableBuilder(
        listenable: Listenable.merge([
          widget.liveStreamManager.roomState.roomVideoStreamIsLandscape,
          widget.liveStreamManager.mediaState.isRemoteVideoStreamPaused
        ]),
        builder: (context, _) {
          final roomVideoStreamIsLandscape = widget.liveStreamManager.roomState.roomVideoStreamIsLandscape.value;
          final isRemoteVideoStreamPaused = widget.liveStreamManager.mediaState.isRemoteVideoStreamPaused.value;
          final visible = !isRemoteVideoStreamPaused && roomVideoStreamIsLandscape;
          return Visibility(
            visible: visible,
            child: IconButton(
              icon: Image.asset(
                LiveImages.rotateScreen,
                package: Constants.pluginName,
                width: 32.radius,
                height: 32.radius,
                fit: BoxFit.fill,
              ),
              iconSize: 32.radius,
              padding: EdgeInsets.zero,
              onPressed: () => _onRotateButtonTapped(orientation),
            ),
          );
        },
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
    if (_giftPlayController != null) {
      return;
    }

    _initBarrageDisPlayController();

    _barrageDisplayController?.setCustomBarrageBuilder(GiftBarrageItemBuilder(
      selfUserId: widget.liveCoreController.userState.selfInfo.userId,
    ));

    _giftPlayController = GiftPlayController(
        roomId: widget.liveCoreController.roomState.roomId, language: DeviceLanguage.getCurrentLanguageCode(context));
    _giftPlayController?.onReceiveGiftCallback = _insertToBarrageMessage;
  }

  void _onRemoteUserEnterRoom() {
    final userInfo = widget.liveStreamManager.userState.enterUser.value;
    BarrageUser barrageUser = BarrageUser();
    barrageUser.userId = userInfo.userId;
    barrageUser.userName = userInfo.userName.isNotEmpty ? userInfo.userName : userInfo.userId;
    barrageUser.avatarUrl = userInfo.avatarUrl;
    barrageUser.level = "66";

    Barrage barrage = Barrage();
    barrage.user = barrageUser;
    barrage.content = LiveKitLocalizations.of(Global.appContext())!.common_entered_room;
    _barrageDisplayController?.insertMessage(barrage);
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

  void _onPlaybackVideoQualityChanged() {
    final playbackVideoQuality = widget.liveStreamManager.mediaState.playbackQuality.value;
    if (playbackVideoQuality != null && playbackQuality != null) {
      final toast = LiveKitLocalizations.of(context)!.live_video_resolution_changed +
          _getVideoQualityString(playbackVideoQuality);
      widget.liveStreamManager.toastSubject.add(toast);
    }
    playbackQuality = playbackVideoQuality;
  }

  void _onCloseIconTap() {
    if (widget.liveCoreController.coGuestState.coGuestStatus.value != CoGuestStatus.linking) {
      widget.liveCoreController.leaveLiveStream();
      if (GlobalFloatWindowManager.instance.state.enableFloatWindowFeature) {
        GlobalFloatWindowManager.instance.overlayManager.closeOverlay();
      } else {
        Navigator.of(context).pop();
      }
      SystemChrome.setPreferredOrientations([
        DeviceOrientation.portraitUp,
      ]);
      return;
    }

    final actionSheetItems = [
      ActionSheetModel(
        isCenter: true,
        text: LiveKitLocalizations.of(Global.appContext())!.common_audience_end_link_tips,
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
        text: LiveKitLocalizations.of(Global.appContext())!.common_end_link,
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
        text: LiveKitLocalizations.of(Global.appContext())!.common_exit_live,
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
          GestureMock.mockTapEvent();
          widget.liveCoreController.leaveLiveStream();
          if (GlobalFloatWindowManager.instance.state.enableFloatWindowFeature) {
            GlobalFloatWindowManager.instance.overlayManager.closeOverlay();
          } else {
            if (mounted) {
              Navigator.of(context).pop();
            }
          }
          break;
        default:
          break;
      }
    });
  }

  void _onRotateButtonTapped(Orientation currentOrientation) {
    if (currentOrientation == Orientation.portrait) {
      SystemChrome.setPreferredOrientations([
        DeviceOrientation.landscapeLeft,
        DeviceOrientation.landscapeRight,
      ]);
    } else {
      SystemChrome.setPreferredOrientations([
        DeviceOrientation.portraitUp,
      ]);
    }
  }

  String _getVideoQualityString(TUIVideoQuality videoQuality) {
    switch (videoQuality) {
      case TUIVideoQuality.videoQuality_1080P:
        return '1080P';
      case TUIVideoQuality.videoQuality_720P:
        return '720P';
      case TUIVideoQuality.videoQuality_540P:
        return '540P';
      case TUIVideoQuality.videoQuality_360P:
        return '360P';
      default:
        return 'Original';
    }
  }

  bool isPureViewingMode() {
    final selfUserId = widget.liveCoreController.userState.selfInfo.userId;
    return !widget.liveCoreController.coGuestState.seatList.value.any((seat) => seat.userId == selfUserId);
  }
}
