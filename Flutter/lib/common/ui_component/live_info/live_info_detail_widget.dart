import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/state/index.dart';

class LiveInfoDetailWidget extends BasicWidget {
  const LiveInfoDetailWidget({super.key, required super.liveController});

  @override
  LiveInfoDetailWidgetState getState() {
    return LiveInfoDetailWidgetState();
  }
}

class LiveInfoDetailWidgetState extends BasicState<LiveInfoDetailWidget> {
  @override
  void initState() {
    super.initState();
    _getFansCount();
  }

  @override
  Widget build(BuildContext context) {
    return SizedBox(
      width: MediaQuery.of(context).size.width,
      height: 241,
      child: Stack(
        alignment: Alignment.topCenter,
        children: [
          _initBackground(),
          _initAnchorAvatarWidget(),
          _initAnchorNameWidget(),
          _initLiveIDWidget(),
          _initFansWidget(),
          _initFollowWidget(),
        ],
      ),
    );
  }

  _initBackground() {
    return Container(
        margin: const EdgeInsets.only(top: 30),
        width: MediaQuery.of(context).size.width,
        height: 211,
        decoration: const BoxDecoration(
          color: LivekitColors.livekitDesignStandardG2,
          borderRadius: BorderRadius.only(topLeft: Radius.circular(20), topRight: Radius.circular(20)),
        ));
  }

  _initAnchorAvatarWidget() {
    return Container(
        margin: const EdgeInsets.only(left: 4, right: 8),
        width: 56,
        height: 56,
        child: ClipOval(
          child: ValueListenableBuilder(
            valueListenable: liveController.getRoomSate().ownerInfo.avatarUrl,
            builder: (BuildContext context, String? value, Widget? child) {
              return Image.network(
                liveController.getRoomSate().ownerInfo.avatarUrl.value ?? "",
                fit: BoxFit.cover,
                errorBuilder: (context, error, stackTrace) {
                  return Image.asset(
                    LivekitImages.livekitDefaultAvatar,
                    package: Constants.pluginName,
                  );
                },
              );
            },
          ),
        ));
  }

  _initAnchorNameWidget() {
    return Positioned(
      top: 64,
      child: ValueListenableBuilder(
        valueListenable: liveController.getRoomSate().ownerInfo.name,
        builder: (BuildContext context, String? value, Widget? child) {
          return Text(
            liveController.getRoomSate().ownerInfo.name.value ?? "",
            overflow: TextOverflow.ellipsis,
            style: const TextStyle(
                fontSize: 16, fontStyle: FontStyle.normal, color: LivekitColors.livekitDesignStandardG7),
          );
        },
      ),
    );
  }

  _initLiveIDWidget() {
    return Positioned(
      top: 94,
      child: Text(
        LiveKitLocalizations.of(Global.appContext())!.livekit_live_room_id + liveController.getRoomSate().roomId,
        overflow: TextOverflow.ellipsis,
        style: const TextStyle(fontSize: 12, fontStyle: FontStyle.normal, color: LivekitColors.livekitDesignStandardG7),
      ),
    );
  }

  _initFansWidget() {
    return Positioned(
      top: 119,
      child: ValueListenableBuilder(
        valueListenable: liveController.getRoomSate().ownerInfo.fansCount,
        builder: (BuildContext context, int value, Widget? child) {
          return Text(
            liveController.getRoomSate().ownerInfo.fansCount.value.toString() +
                LiveKitLocalizations.of(Global.appContext())!.livekit_fan_count,
            overflow: TextOverflow.ellipsis,
            style: const TextStyle(
                fontSize: 12, fontStyle: FontStyle.normal, color: LivekitColors.livekitDesignStandardG7),
          );
        },
      ),
    );
  }

  _initFollowWidget() {
    return Positioned(
      top: 144,
      width: 275,
      height: 40,
      child: Visibility(
        visible: liveController.getRoomSate().ownerInfo.userId != liveController.getUserState().selfInfo.userId,
        child: ValueListenableBuilder(
          valueListenable: liveController.getUserState().myFollowingUserList,
          builder: (BuildContext context, value, Widget? child) {
            return GestureDetector(
              onTap: () {
                _followAnchor();
              },
              child: Container(
                margin: const EdgeInsets.only(left: 14.5, right: 4),
                decoration: BoxDecoration(
                  borderRadius: BorderRadius.circular(20),
                  color: _isFollow() ? LivekitColors.livekitDesignStandardG3 : LivekitColors.livekitDesignStandardB1,
                ),
                alignment: Alignment.center,
                child: Text(
                  _isFollow()
                      ? LiveKitLocalizations.of(Global.appContext())!.livekit_unfollow_anchor
                      : LiveKitLocalizations.of(Global.appContext())!.livekit_follow_anchor,
                  style: const TextStyle(
                      fontSize: 16, fontStyle: FontStyle.normal, color: LivekitColors.livekitDesignStandardG7),
                ),
              ),
            );
          },
        ),
      ),
    );
  }
}

extension LiveInfoDetailWidgetStateLogicExtension on LiveInfoDetailWidgetState {
  _getFansCount() {
    liveController.userController.getFansCount();
  }

  bool _isFollow() {
    return liveController
        .getUserState()
        .myFollowingUserList
        .value
        .contains(UserInfo.formUserId(liveController.getRoomSate().ownerInfo.userId));
  }

  _followAnchor() {
    if (_isFollow()) {
      liveController.userController.unfollowUser(liveController.getRoomSate().ownerInfo.userId);
    } else {
      liveController.userController.followUser(liveController.getRoomSate().ownerInfo.userId);
    }
  }
}
