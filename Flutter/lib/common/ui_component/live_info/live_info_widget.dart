import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/common/ui_component/live_info/live_info_detail_widget.dart';
import 'package:tencent_live_uikit/state/index.dart';

class LiveInfoWidget extends BasicWidget {
  const LiveInfoWidget({super.key, required super.liveController});

  @override
  LiveInfoWidgetState getState() {
    return LiveInfoWidgetState();
  }
}

class LiveInfoWidgetState extends BasicState<LiveInfoWidget> {
  @override
  void initState() {
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      onTap: () {
        _showLiveInfoDetailWidget();
      },
      child: Row(
        mainAxisAlignment: MainAxisAlignment.start,
        children: [
          Container(
            height: 32,
            padding: const EdgeInsets.only(right: 4),
            decoration: const BoxDecoration(
              color: LivekitColors.livekitNotStandardGray60Transparency,
              borderRadius: BorderRadius.all(Radius.circular(16)),
            ),
            child: Row(
              mainAxisSize: MainAxisSize.min,
              crossAxisAlignment: CrossAxisAlignment.center,
              children: [
                _initOwnerAvatarWidget(),
                _initOwnerNameWidget(),
                _initFollowWidget(),
              ],
            ),
          ),
          Expanded(child: Container(color: LivekitColors.livekitDesignStandardTransparent)),
        ],
      ),
    );
  }

  _initOwnerAvatarWidget() {
    return Container(
        margin: const EdgeInsets.only(left: 4, right: 4),
        width: 24,
        height: 24,
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

  _initOwnerNameWidget() {
    return Container(
      margin: const EdgeInsets.only(right: 4),
      constraints: const BoxConstraints(maxWidth: 72),
      child: ValueListenableBuilder(
          valueListenable: liveController.getRoomSate().ownerInfo.name,
          builder: (BuildContext context, String? value, Widget? child) {
            return Text(
              liveController.getRoomSate().ownerInfo.name.value ?? "",
              overflow: TextOverflow.ellipsis,
              style: const TextStyle(
                  fontSize: 12, fontStyle: FontStyle.normal, color: LivekitColors.livekitDesignStandardG7),
            );
          }),
    );
  }

  _initFollowWidget() {
    return Visibility(
      visible: liveController.getRoomSate().ownerInfo.userId != liveController.getUserState().selfInfo.userId,
      child: ValueListenableBuilder(
        valueListenable: liveController.getUserState().myFollowingUserList,
        builder: (BuildContext context, value, Widget? child) {
          return Container(
            margin: const EdgeInsets.only(left: 10.5),
            width: 45,
            height: 24,
            decoration: BoxDecoration(
              borderRadius: BorderRadius.circular(12),
              color: _isFollow() ? LivekitColors.livekitNotStandardGreyC5 : LivekitColors.livekitNotStandardBlue,
            ),
            alignment: Alignment.center,
            child: _isFollow()
                ? Image.asset(
                    LivekitImages.livekitFollowed,
                    package: Constants.pluginName,
                    width: 16,
                    height: 16,
                  )
                : GestureDetector(
                    onTap: () {
                      _followAnchor();
                    },
                    child: Text(
                      LiveKitLocalizations.of(Global.appContext())!.livekit_follow_anchor,
                      style: const TextStyle(
                          fontSize: 12, fontStyle: FontStyle.normal, color: LivekitColors.livekitDesignStandardG7),
                    ),
                  ),
          );
        },
      ),
    );
  }
}

extension LiveInfoWidgetStateLogicExtension on LiveInfoWidgetState {
  bool _isFollow() {
    return liveController
        .getUserState()
        .myFollowingUserList
        .value
        .contains(UserInfo.formUserId(liveController.getRoomSate().ownerInfo.userId));
  }

  _followAnchor() {
    liveController.userController.followUser(liveController.getRoomSate().ownerInfo.userId);
  }

  _showLiveInfoDetailWidget() {
    showWidget(LiveInfoDetailWidget(
      liveController: liveController,
    ));
  }
}
