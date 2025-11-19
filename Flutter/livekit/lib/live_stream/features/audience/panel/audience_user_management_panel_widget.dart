import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/index.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_operation_result.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_type_check_result.dart';
import 'package:tencent_cloud_chat_sdk/tencent_im_sdk_plugin.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/live_stream/manager/live_stream_manager.dart';
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

import '../../../../component/live_info/state/follow_define.dart';

class AudienceUserManagementPanelWidget extends StatefulWidget {
  final TUIUserInfo user;
  final LiveStreamManager liveStreamManager;
  final LiveCoreController liveCoreController;

  const AudienceUserManagementPanelWidget(
      {super.key, required this.user, required this.liveStreamManager, required this.liveCoreController});

  @override
  State<AudienceUserManagementPanelWidget> createState() => _AudienceUserManagementPanelWidgetState();
}

class _AudienceUserManagementPanelWidgetState extends State<AudienceUserManagementPanelWidget> {
  final ValueNotifier<bool> _isFollow = ValueNotifier(false);
  bool _enableFollowButton = true;
  bool isShowingAlert = false;

  @override
  void initState() {
    super.initState();
    _checkFollowType();
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
                child: ValueListenableBuilder(
                  valueListenable: _isFollow,
                  builder: (context, isFollow, child) {
                    return Visibility(
                      visible: widget.user.userId != widget.liveStreamManager.coreUserState.selfInfo.userId,
                      child: GestureDetector(
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
                      ),
                    );
                  },
                ))
          ],
        ),
      ),
    );
  }

  Widget _buildMenuWidget() {
    return Padding(
      padding: EdgeInsets.symmetric(horizontal: 24.width),
      child: Container(
        constraints: BoxConstraints(maxWidth: 327.width),
        height: 77.height,
        child: Row(
          mainAxisAlignment: MainAxisAlignment.start,
          crossAxisAlignment: CrossAxisAlignment.center,
          children: [
            ListenableBuilder(
              listenable: Listenable.merge([
                widget.liveStreamManager.coreMediaState.isMicrophoneMuted,
                widget.liveStreamManager.mediaState.isAudioLocked
              ]),
              builder: (context, child) {
                final isMicrophoneMuted = widget.liveStreamManager.coreMediaState.isMicrophoneMuted.value;
                final isAudioLocked = widget.liveStreamManager.mediaState.isAudioLocked.value;
                return GestureDetector(
                  onTap: () => _microphoneButtonClicked(),
                  child: Column(
                    children: [
                      Container(
                          decoration: BoxDecoration(
                              color: LiveColors.designStandardG3.withAlpha(77),
                              borderRadius: BorderRadius.circular(12.radius)),
                          width: 50.radius,
                          height: 50.radius,
                          child: Center(
                            child: Opacity(
                              opacity: isAudioLocked ? 0.5 : 1.0,
                              child: Image.asset(
                                  isMicrophoneMuted ? LiveImages.muteMicrophone : LiveImages.unmuteMicrophone,
                                  package: Constants.pluginName,
                                  width: 25.radius,
                                  height: 25.radius),
                            ),
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
            SizedBox(width: 20.width),
            ListenableBuilder(
              listenable: Listenable.merge([
                widget.liveStreamManager.coreMediaState.isCameraOpened,
                widget.liveStreamManager.mediaState.isVideoLocked
              ]),
              builder: (context, child) {
                final isCameraOpened = widget.liveStreamManager.coreMediaState.isCameraOpened.value;
                final isVideoLocked = widget.liveStreamManager.mediaState.isVideoLocked.value;
                return GestureDetector(
                  onTap: () => _cameraButtonClicked(),
                  child: Column(
                    children: [
                      Container(
                          decoration: BoxDecoration(
                              color: LiveColors.designStandardG3.withAlpha(77),
                              borderRadius: BorderRadius.circular(12.radius)),
                          width: 50.radius,
                          height: 50.radius,
                          child: Center(
                            child: Opacity(
                              opacity: isVideoLocked ? 0.5 : 1.0,
                              child: Image.asset(isCameraOpened ? LiveImages.openCamera : LiveImages.closeCamera,
                                  package: Constants.pluginName, width: 25.radius, height: 25.radius),
                            ),
                          )),
                      Text(
                        isCameraOpened
                            ? LiveKitLocalizations.of(context)!.common_stop_video
                            : LiveKitLocalizations.of(context)!.common_start_video,
                        style: const TextStyle(color: LiveColors.designStandardG6, fontSize: 12),
                      )
                    ],
                  ),
                );
              },
            ),
            SizedBox(width: 20.width),
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
                                child: Image.asset(LiveImages.videoSettingsFlip,
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
                }),
            ValueListenableBuilder(
                valueListenable: widget.liveStreamManager.coreMediaState.isCameraOpened,
                builder: (context, isCameraOpened, _) {
                  return Visibility(visible: isCameraOpened, child: SizedBox(width: 20.width));
                }),
            GestureDetector(
              onTap: () => _leaveSeatButtonClicked(),
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

extension on _AudienceUserManagementPanelWidgetState {
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

  void _microphoneButtonClicked() {
    final isAudioLocked = widget.liveStreamManager.mediaState.isAudioLocked.value;
    if (isAudioLocked) {
      return;
    }

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

  void _cameraButtonClicked() {
    final isVideoLocked = widget.liveStreamManager.mediaState.isVideoLocked.value;
    if (isVideoLocked) {
      return;
    }

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

  void _leaveSeatButtonClicked() {
    final alertInfo = AlertInfo(
        description: LiveKitLocalizations.of(Global.appContext())!.common_terminate_room_connection_message,
        cancelActionInfo: (
          title: LiveKitLocalizations.of(Global.appContext())!.common_cancel,
          titleColor: LiveColors.designStandardG3
        ),
        cancelCallback: () {
          isShowingAlert = false;
          Navigator.pop(Global.appContext());
        },
        defaultActionInfo: (
          title: LiveKitLocalizations.of(Global.appContext())!.common_end_link,
          titleColor: LiveColors.notStandardRed
        ),
        defaultCallback: () {
          widget.liveCoreController.terminateIntraRoomConnection();
          isShowingAlert = false;
          Navigator.pop(Global.appContext());
          Navigator.pop(Global.appContext());
        });

    Alert.showAlert(alertInfo);
    isShowingAlert = true;
  }
}
