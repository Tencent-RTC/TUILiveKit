import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/index.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_operation_result.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_type_check_result.dart';
import 'package:tencent_cloud_chat_sdk/tencent_im_sdk_plugin.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/live_stream/manager/live_stream_manager.dart';
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

import '../../../../component/live_info/state/follow_define.dart';

enum AnchorUserManagementPanelType { messageAndKickOut, pureMedia, mediaAndSeat }

class AnchorUserManagementPanelWidget extends StatefulWidget {
  final AnchorUserManagementPanelType panelType;
  final TUIUserInfo user;
  final LiveStreamManager liveStreamManager;
  final LiveCoreController liveCoreController;

  const AnchorUserManagementPanelWidget(
      {super.key,
      required this.panelType,
      required this.user,
      required this.liveStreamManager,
      required this.liveCoreController});

  @override
  State<AnchorUserManagementPanelWidget> createState() => _AnchorUserManagementPanelWidgetState();
}

class _AnchorUserManagementPanelWidgetState extends State<AnchorUserManagementPanelWidget> {
  final ValueNotifier<bool> _isFollow = ValueNotifier(false);
  final ValueNotifier<bool> _isMessageDisabled = ValueNotifier(false);
  bool _enableFollowButton = true;
  bool isShowingAlert = false;
  late final TUIUserInfo user;

  @override
  void initState() {
    super.initState();
    _checkFollowType();
    _supplyUserInfo();
  }

  @override
  void dispose() {
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      width: 1.screenWidth,
      constraints: BoxConstraints(minHeight: 88.height, maxHeight: 179.height),
      decoration: BoxDecoration(
          borderRadius: BorderRadius.only(topLeft: Radius.circular(15.width), topRight: Radius.circular(15.width))),
      height: 179.height,
      child: Column(children: [
        SizedBox(height: 24.height),
        _buildUserInfoWidget(),
        SizedBox(height: 20.height),
        _buildMenuWidget()
      ]),
    );
  }

  Widget _buildMenuWidget() {
    switch (widget.panelType) {
      case AnchorUserManagementPanelType.messageAndKickOut:
        return _buildMessageAndKickOutWidget();
      case AnchorUserManagementPanelType.pureMedia:
        return _buildPureMediaWidget();
      case AnchorUserManagementPanelType.mediaAndSeat:
        return _buildMediaAndSeatWidget();
    }
  }

  Widget _buildUserInfoWidget() {
    return Padding(
      padding: EdgeInsets.symmetric(horizontal: 24.width),
      child: SizedBox(
        width: 1.screenWidth,
        child: Stack(
          children: [
            SizedBox(
              width: 40.radius,
              height: 40.radius,
              child: ClipOval(
                child: Image.network(widget.user.avatarUrl, errorBuilder: (context, error, stackTrace) {
                  return Image.asset(LiveImages.defaultAvatar, package: Constants.pluginName);
                }),
              ),
            ),
            Positioned(
                top: 0,
                left: 52.width,
                child: Column(
                  mainAxisSize: MainAxisSize.min,
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Text(widget.user.userName.isNotEmpty ? widget.user.userName : widget.user.userId,
                        style: const TextStyle(color: LiveColors.designStandardG6, fontSize: 16),
                        textAlign: TextAlign.left,
                        overflow: TextOverflow.ellipsis),
                    Text('Id: ${widget.user.userId}',
                        style: const TextStyle(
                          color: LiveColors.notStandardGrey,
                          fontSize: 12,
                        ),
                        textAlign: TextAlign.left,
                        overflow: TextOverflow.ellipsis)
                  ],
                )),
            Positioned(
                top: 4.height,
                bottom: 4.height,
                right: 0,
                child: Visibility(
                  visible: widget.user.userId != widget.liveCoreController.userState.selfInfo.userId,
                  child: ValueListenableBuilder(
                    valueListenable: _isFollow,
                    builder: (context, isFollow, child) {
                      return GestureDetector(
                        onTap: () => _followButtonClicked(),
                        child: Container(
                          width: 70.width,
                          height: 32.height,
                          decoration: BoxDecoration(
                              color: isFollow ? LiveColors.notStandardGreyC5 : LiveColors.notStandardBlue,
                              borderRadius: BorderRadius.circular(16.height)),
                          child: Center(
                            child: isFollow
                                ? Image.asset(
                                    LiveImages.followed,
                                    package: Constants.pluginName,
                                    width: 16.radius,
                                    height: 16.radius,
                                  )
                                : Text(
                                    LiveKitLocalizations.of(Global.appContext())!.common_follow_anchor,
                                    style: const TextStyle(
                                        fontSize: 12, fontStyle: FontStyle.normal, color: LiveColors.designStandardG7),
                                  ),
                          ),
                        ),
                      );
                    },
                  ),
                ))
          ],
        ),
      ),
    );
  }

  Widget _buildMessageAndKickOutWidget() {
    return Padding(
      padding: EdgeInsets.symmetric(horizontal: 24.width),
      child: Container(
        constraints: BoxConstraints(maxWidth: 327.width),
        height: 77.height,
        child: Row(
          mainAxisAlignment: MainAxisAlignment.start,
          crossAxisAlignment: CrossAxisAlignment.center,
          spacing: 20.width,
          children: [
            ValueListenableBuilder(
              valueListenable: _isMessageDisabled,
              builder: (context, isMessageDisabled, child) {
                return GestureDetector(
                  onTap: () => _messageButtonClicked(),
                  child: Column(
                    children: [
                      Container(
                          decoration: BoxDecoration(
                              color: LiveColors.designStandardG3.withAlpha(77),
                              borderRadius: BorderRadius.circular(12.radius)),
                          width: 50.radius,
                          height: 50.radius,
                          child: Center(
                            child: Image.asset(isMessageDisabled ? LiveImages.disableChat : LiveImages.enableChat,
                                package: Constants.pluginName, width: 25.radius, height: 25.radius),
                          )),
                      Text(
                        isMessageDisabled
                            ? LiveKitLocalizations.of(context)!.common_enable_message
                            : LiveKitLocalizations.of(context)!.common_disable_message,
                        style: const TextStyle(color: LiveColors.designStandardG6, fontSize: 12),
                      )
                    ],
                  ),
                );
              },
            ),
            GestureDetector(
              onTap: () => _kickOutButtonClicked(),
              child: Column(
                children: [
                  Container(
                      decoration: BoxDecoration(
                          color: LiveColors.designStandardG3.withAlpha(77),
                          borderRadius: BorderRadius.circular(12.5.radius)),
                      width: 50.radius,
                      height: 50.radius,
                      child: Center(
                        child: Image.asset(LiveImages.anchorKickOut,
                            package: Constants.pluginName, width: 25.radius, height: 25.radius),
                      )),
                  Text(
                    LiveKitLocalizations.of(context)!.common_kick_out_of_room,
                    style: const TextStyle(color: LiveColors.designStandardG6, fontSize: 12),
                  )
                ],
              ),
            )
          ],
        ),
      ),
    );
  }

  Widget _buildPureMediaWidget() {
    return Padding(
      padding: EdgeInsets.symmetric(horizontal: 24.width),
      child: Container(
        constraints: BoxConstraints(maxWidth: 327.width),
        height: 77.height,
        child: Row(
          mainAxisAlignment: MainAxisAlignment.start,
          crossAxisAlignment: CrossAxisAlignment.center,
          spacing: 20.width,
          children: [
            ValueListenableBuilder(
              valueListenable: widget.liveStreamManager.coreMediaState.isMicrophoneMuted,
              builder: (context, isMicrophoneMuted, child) {
                return GestureDetector(
                  onTap: () => _localMicrophoneButtonClicked(),
                  child: Column(
                    children: [
                      Container(
                          decoration: BoxDecoration(
                              color: LiveColors.designStandardG3.withAlpha(77),
                              borderRadius: BorderRadius.circular(12.radius)),
                          width: 50.radius,
                          height: 50.radius,
                          child: Center(
                            child: Image.asset(isMicrophoneMuted ? LiveImages.anchorMute : LiveImages.anchorUnmute,
                                package: Constants.pluginName, width: 25.radius, height: 25.radius),
                          )),
                      Text(
                        isMicrophoneMuted
                            ? LiveKitLocalizations.of(context)!.common_voiceroom_unmuted_seat
                            : LiveKitLocalizations.of(context)!.common_voiceroom_mute_seat,
                        style: const TextStyle(color: LiveColors.designStandardG6, fontSize: 12),
                      )
                    ],
                  ),
                );
              },
            ),
            ValueListenableBuilder(
                valueListenable: widget.liveStreamManager.coreMediaState.isCameraOpened,
                builder: (context, isCameraOpened, _) {
                  return Visibility(
                    visible: isCameraOpened,
                    child: GestureDetector(
                      onTap: () => _flipButtonClicked(),
                      child: Column(
                        children: [
                          Container(
                              decoration: BoxDecoration(
                                  color: LiveColors.designStandardG3.withAlpha(77),
                                  borderRadius: BorderRadius.circular(12.5.radius)),
                              width: 50.radius,
                              height: 50.radius,
                              child: Center(
                                child: Image.asset(LiveImages.settingsItemFlip,
                                    package: Constants.pluginName, width: 25.radius, height: 25.radius),
                              )),
                          Text(
                            LiveKitLocalizations.of(context)!.common_video_settings_item_flip,
                            style: const TextStyle(color: LiveColors.designStandardG6, fontSize: 12),
                          )
                        ],
                      ),
                    ),
                  );
                })
          ],
        ),
      ),
    );
  }

  Widget _buildMediaAndSeatWidget() {
    return Padding(
      padding: EdgeInsets.symmetric(horizontal: 24.width),
      child: Container(
        constraints: BoxConstraints(maxWidth: 327.width),
        height: 77.height,
        child: Row(
          mainAxisAlignment: MainAxisAlignment.start,
          crossAxisAlignment: CrossAxisAlignment.center,
          spacing: 20.width,
          children: [
            ValueListenableBuilder(
              valueListenable: widget.liveStreamManager.coGuestState.lockAudioUserList,
              builder: (context, lockAudioUserList, child) {
                final isAudioLocked = _isAudioLocked();
                return GestureDetector(
                  onTap: () => _remoteMicrophoneButtonClicked(),
                  child: Column(
                    children: [
                      Container(
                          decoration: BoxDecoration(
                              color: LiveColors.designStandardG3.withAlpha(77),
                              borderRadius: BorderRadius.circular(12.radius)),
                          width: 50.radius,
                          height: 50.radius,
                          child: Center(
                            child: Image.asset(isAudioLocked ? LiveImages.disableAudio : LiveImages.anchorUnmute,
                                package: Constants.pluginName, width: 25.radius, height: 25.radius),
                          )),
                      Text(
                        isAudioLocked
                            ? LiveKitLocalizations.of(context)!.common_enable_audio
                            : LiveKitLocalizations.of(context)!.common_disable_audio,
                        style: const TextStyle(color: LiveColors.designStandardG6, fontSize: 12),
                      )
                    ],
                  ),
                );
              },
            ),
            ValueListenableBuilder(
              valueListenable: widget.liveStreamManager.coGuestState.lockVideoUserList,
              builder: (context, lockVideoUserList, child) {
                final isVideoLocked = _isVideoLocked();
                return GestureDetector(
                  onTap: () => _remoteCameraButtonClicked(),
                  child: Column(
                    children: [
                      Container(
                          decoration: BoxDecoration(
                              color: LiveColors.designStandardG3.withAlpha(77),
                              borderRadius: BorderRadius.circular(12.radius)),
                          width: 50.radius,
                          height: 50.radius,
                          child: Center(
                            child: Image.asset(isVideoLocked ? LiveImages.disableCamera : LiveImages.openCamera,
                                package: Constants.pluginName, width: 25.radius, height: 25.radius),
                          )),
                      Text(
                        isVideoLocked
                            ? LiveKitLocalizations.of(context)!.common_enable_video
                            : LiveKitLocalizations.of(context)!.common_disable_video,
                        style: const TextStyle(color: LiveColors.designStandardG6, fontSize: 12),
                      )
                    ],
                  ),
                );
              },
            ),
            GestureDetector(
              onTap: () => _kickOutOfSeatButtonClicked(),
              child: Column(
                children: [
                  Container(
                      decoration: BoxDecoration(
                          color: LiveColors.designStandardG3.withAlpha(77),
                          borderRadius: BorderRadius.circular(12.5.radius)),
                      width: 50.radius,
                      height: 50.radius,
                      child: Center(
                        child: Image.asset(LiveImages.leaveSeat,
                            package: Constants.pluginName, width: 25.radius, height: 25.radius),
                      )),
                  Text(
                    LiveKitLocalizations.of(context)!.common_end_user,
                    style: const TextStyle(color: LiveColors.designStandardG6, fontSize: 12),
                  )
                ],
              ),
            )
          ],
        ),
      ),
    );
  }
}

extension on _AnchorUserManagementPanelWidgetState {
  void _checkFollowType() async {
    final result =
        await TencentImSDKPlugin.v2TIMManager.getFriendshipManager().checkFollowType(userIDList: [widget.user.userId]);
    if (result.code != 0 || result.data == null || result.data is! List<V2TimFollowTypeCheckResult>) {
      return;
    }
    final V2TimFollowTypeCheckResult? checkResult = result.data!.firstOrNull;
    if (checkResult == null) {
      return;
    }
    final followType = IMFollowType.fromInt(result.data![0].followType ?? 0);
    _isFollow.value = followType == IMFollowType.inMyFollowingList || followType == IMFollowType.inBothFollowersList;
  }

  void _followButtonClicked() async {
    if (_enableFollowButton == false) {
      return;
    }
    _enableFollowButton = false;
    final friendshipManager = TencentImSDKPlugin.v2TIMManager.getFriendshipManager();
    final userId = widget.user.userId;
    if (userId.isEmpty) {
      return;
    }

    if (!_isFollow.value) {
      final result = await friendshipManager.followUser(userIDList: [userId]);
      if (result.code != 0) {
        widget.liveStreamManager.toastSubject.add('code:${result.code}, message:${result.desc}');
        return;
      }
      final V2TimFollowOperationResult? followResult = result.data!.firstOrNull;
      if (followResult == null) {
        return;
      }
      _isFollow.value = true;
      _enableFollowButton = true;
    } else {
      final result = await friendshipManager.unfollowUser(userIDList: [userId]);
      if (result.code != 0) {
        widget.liveStreamManager.toastSubject.add('code:${result.code}, message:${result.desc}');
        return;
      }
      final V2TimFollowOperationResult? followResult = result.data!.firstOrNull;
      if (followResult == null) {
        return;
      }
      _isFollow.value = false;
      _enableFollowButton = true;
    }
  }

  void _messageButtonClicked() {
    widget.liveStreamManager
        .onDisableSendingMessageBtnClicked(widget.user.userId, !_isMessageDisabled.value)
        .then((result) {
      if (result.code != TUIError.success) {
        widget.liveStreamManager.toastSubject
            .add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
      } else {
        _isMessageDisabled.value = !_isMessageDisabled.value;
      }
    });
    Navigator.pop(context);
  }

  void _supplyUserInfo() async {
    final result = await widget.liveStreamManager.getUserInfo(widget.user.userId);
    if (result.code != TUIError.success || result.data == null) {
      LiveKitLogger.error('_supplyUserInfo failed. code:${result.code}, message:${result.message}');
      return;
    }
    user = result.data!;
    _isMessageDisabled.value = user.isMessageDisabled ?? false;
  }

  void _kickOutButtonClicked() {
    final userName = widget.user.userName.isNotEmpty ? widget.user.userName : widget.user.userId;
    final alertInfo = AlertInfo(
        description:
            LiveKitLocalizations.of(Global.appContext())!.common_kick_user_confirm_message.replaceAll('xxx', userName),
        cancelActionInfo: (
          title: LiveKitLocalizations.of(Global.appContext())!.common_cancel,
          titleColor: LiveColors.designStandardG3
        ),
        cancelCallback: () {
          isShowingAlert = false;
          Navigator.pop(Global.appContext());
        },
        defaultActionInfo: (
          title: LiveKitLocalizations.of(Global.appContext())!.common_kick_out_of_room,
          titleColor: LiveColors.notStandardRed
        ),
        defaultCallback: () {
          widget.liveStreamManager.onKickedOutBtnClicked(widget.user.userId).then((result) {
            widget.liveStreamManager.toastSubject
                .add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
          });
          isShowingAlert = false;
          Navigator.pop(Global.appContext());
          Navigator.pop(Global.appContext());
        });

    Alert.showAlert(alertInfo);
    isShowingAlert = true;
  }

  void _localMicrophoneButtonClicked() {
    final isMicrophoneMuted = widget.liveStreamManager.coreMediaState.isMicrophoneMuted.value;
    if (isMicrophoneMuted) {
      widget.liveCoreController.unmuteMicrophone().then((result) {
        if (result.code != TUIError.success) {
          widget.liveStreamManager.toastSubject
              .add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
        }
      });
    } else {
      widget.liveCoreController.muteMicrophone();
    }
    Navigator.pop(context);
  }

  void _localCameraButtonClicked() {
    final isCameraOpened = widget.liveStreamManager.coreMediaState.isCameraOpened.value;
    if (isCameraOpened) {
      widget.liveCoreController.stopCamera();
    } else {
      final isFrontCamera = widget.liveStreamManager.coreMediaState.isFrontCamera;
      widget.liveCoreController.startCamera(isFrontCamera).then((result) {
        if (result.code != TUIError.success) {
          widget.liveStreamManager.toastSubject
              .add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
        }
      });
    }
    Navigator.pop(context);
  }

  void _flipButtonClicked() {
    widget.liveCoreController.switchCamera(!widget.liveStreamManager.coreMediaState.isFrontCamera);
    Navigator.pop(context);
  }

  void _remoteMicrophoneButtonClicked() async {
    final lockParams = TUISeatLockParams();
    lockParams.lockAudio = !_isAudioLocked();
    lockParams.lockVideo = _isVideoLocked();
    widget.liveStreamManager.onLockMediaStatusBtnClicked(widget.user.userId, lockParams).then((result) {
      if (result.code != TUIError.success) {
        widget.liveStreamManager.toastSubject
            .add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
      }
    });
    Navigator.pop(context);
  }

  void _remoteCameraButtonClicked() {
    final lockParams = TUISeatLockParams();
    lockParams.lockAudio = _isAudioLocked();
    lockParams.lockVideo = !_isVideoLocked();
    widget.liveStreamManager.onLockMediaStatusBtnClicked(widget.user.userId, lockParams).then((result) {
      if (result.code != TUIError.success) {
        widget.liveStreamManager.toastSubject
            .add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
      }
    });
    Navigator.pop(context);
  }

  void _kickOutOfSeatButtonClicked() {
    String userName = widget.user.userName;
    if (userName.isEmpty) userName = widget.user.userId;
    final alertInfo = AlertInfo(
        description: LiveKitLocalizations.of(Global.appContext())!.common_disconnect_guest_tips.replaceAll("xxx", userName),
        cancelActionInfo: (
          title: LiveKitLocalizations.of(Global.appContext())!.common_cancel,
          titleColor: LiveColors.designStandardG3
        ),
        cancelCallback: () {
          isShowingAlert = false;
          Navigator.pop(Global.appContext());
        },
        defaultActionInfo: (
          title: LiveKitLocalizations.of(Global.appContext())!.common_down,
          titleColor: LiveColors.notStandardRed
        ),
        defaultCallback: () {
          widget.liveCoreController.disconnectUser(widget.user.userId).then((result) {
            widget.liveStreamManager.toastSubject
                .add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
          });
          isShowingAlert = false;
          Navigator.pop(Global.appContext());
          Navigator.pop(Global.appContext());
        });

    Alert.showAlert(alertInfo);
    isShowingAlert = true;
  }

  bool _isAudioLocked() {
    return widget.liveStreamManager.coGuestState.lockAudioUserList.value.contains(widget.user.userId);
  }

  bool _isVideoLocked() {
    return widget.liveStreamManager.coGuestState.lockVideoUserList.value.contains(widget.user.userId);
  }
}
