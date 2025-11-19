import 'package:flutter/material.dart';

import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_info.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_operation_result.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_follow_type_check_result.dart';
import 'package:tencent_cloud_chat_sdk/tencent_im_sdk_plugin.dart';
import 'package:tencent_live_uikit/live_stream/manager/live_stream_manager.dart';

import '../../../../../common/constants/constants.dart';
import '../../../../../common/language/index.dart';
import '../../../../../common/resources/colors.dart';
import '../../../../../common/resources/images.dart';
import '../../../../../common/widget/index.dart';
import '../../../../../common/screen/index.dart';
import '../../../../component/live_info/state/follow_define.dart';

class AudienceUserInfoPanelWidget extends StatefulWidget {
  final TUIUserInfo user;
  final LiveStreamManager liveStreamManager;

  const AudienceUserInfoPanelWidget({super.key, required this.user, required this.liveStreamManager});

  @override
  State<AudienceUserInfoPanelWidget> createState() => _AudienceUserInfoPanelWidgetState();
}

class _AudienceUserInfoPanelWidgetState extends State<AudienceUserInfoPanelWidget> {
  final ValueNotifier<bool> _isFollow = ValueNotifier(false);
  final ValueNotifier<int> _fansNumber = ValueNotifier(0);
  bool _enableFollowButton = true;

  @override
  void initState() {
    super.initState();
    _getFansCount();
    _checkFollowType();
  }

  @override
  void dispose() {
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return SizedBox(
      width: MediaQuery.sizeOf(context).width,
      height: 241.height,
      child: Stack(
        alignment: Alignment.topCenter,
        children: [
          _buildBackground(),
          _buildUserAvatarWidget(),
          _buildUserNameWidget(),
          _buildLiveIDWidget(),
          _buildFansWidget(),
          _buildFollowWidget(),
        ],
      ),
    );
  }

  Widget _buildBackground() {
    return Container(
        margin: EdgeInsets.only(top: 30.height),
        width: MediaQuery.sizeOf(context).width,
        height: 211.height,
        decoration: BoxDecoration(
          color: LiveColors.designStandardG2,
          borderRadius: BorderRadius.only(topLeft: Radius.circular(20.radius), topRight: Radius.circular(20.radius)),
        ));
  }

  Widget _buildUserAvatarWidget() {
    return Container(
        margin: EdgeInsets.only(left: 4.width, right: 8.width),
        width: 56.radius,
        height: 56.radius,
        child: ClipOval(
          child: Image.network(
            widget.user.avatarUrl,
            fit: BoxFit.cover,
            errorBuilder: (context, error, stackTrace) {
              return Image.asset(
                LiveImages.defaultAvatar,
                package: Constants.pluginName,
              );
            },
          ),
        ));
  }

  Widget _buildUserNameWidget() {
    return Positioned(
      top: 64.height,
      child: Text(
        widget.user.userName.isNotEmpty ? widget.user.userName : widget.user.userId,
        overflow: TextOverflow.ellipsis,
        style: const TextStyle(fontSize: 16, fontStyle: FontStyle.normal, color: LiveColors.designStandardG7),
      ),
    );
  }

  Widget _buildLiveIDWidget() {
    return Positioned(
      top: 94.height,
      child: Text(
        LiveKitLocalizations.of(Global.appContext())!.common_room_info_liveroom_id +
            widget.liveStreamManager.roomState.roomId,
        overflow: TextOverflow.ellipsis,
        style: const TextStyle(fontSize: 12, fontStyle: FontStyle.normal, color: LiveColors.designStandardG7),
      ),
    );
  }

  Widget _buildFansWidget() {
    return Positioned(
      top: 119.height,
      child: ValueListenableBuilder(
        valueListenable: _fansNumber,
        builder: (context, fansNumber, child) {
          return Text(
            fansNumber.toString() + LiveKitLocalizations.of(Global.appContext())!.common_fan_count,
            overflow: TextOverflow.ellipsis,
            style: const TextStyle(fontSize: 12, fontStyle: FontStyle.normal, color: LiveColors.designStandardG7),
          );
        },
      ),
    );
  }

  Widget _buildFollowWidget() {
    return Positioned(
      top: 144.height,
      width: 275.width,
      height: 40.height,
      child: Visibility(
        visible: widget.liveStreamManager.coreUserState.selfInfo.userId != widget.user.userId,
        child: ValueListenableBuilder(
          valueListenable: _isFollow,
          builder: (context, isFollow, child) {
            return GestureDetector(
              onTap: () {
                _followButtonClicked();
              },
              child: Container(
                margin: EdgeInsets.only(left: 14.width, right: 4.width),
                decoration: BoxDecoration(
                  borderRadius: BorderRadius.circular(20.radius),
                  color: isFollow ? LiveColors.designStandardG3 : LiveColors.designStandardB1,
                ),
                alignment: Alignment.center,
                child: Text(
                  isFollow
                      ? LiveKitLocalizations.of(Global.appContext())!.common_unfollow_anchor
                      : LiveKitLocalizations.of(Global.appContext())!.common_follow_anchor,
                  style: const TextStyle(fontSize: 16, fontStyle: FontStyle.normal, color: LiveColors.designStandardG7),
                ),
              ),
            );
          },
        ),
      ),
    );
  }
}

extension on _AudienceUserInfoPanelWidgetState {
  void _getFansCount() async {
    final result = await TencentImSDKPlugin.v2TIMManager
        .getFriendshipManager()
        .getUserFollowInfo(userIDList: [widget.user.userId]);
    if (result.code != 0 || result.data == null || result.data is! List<V2TimFollowInfo>) {
      return;
    }
    final V2TimFollowInfo? followInfo = result.data!.firstOrNull;
    if (followInfo == null) {
      return;
    }
    _fansNumber.value = followInfo.followersCount ?? 0;
  }

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
      _fansNumber.value += 1;
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
      _fansNumber.value -= 1;
      _isFollow.value = false;
      _enableFollowButton = true;
    }
  }
}
