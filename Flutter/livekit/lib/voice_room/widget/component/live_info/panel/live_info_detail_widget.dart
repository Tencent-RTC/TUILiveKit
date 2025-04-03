import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/voice_room/widget/component/live_info/manager/live_info_manager.dart';

import '../../../../../common/constants/constants.dart';
import '../../../../../common/language/index.dart';
import '../../../../../common/resources/colors.dart';
import '../../../../../common/resources/images.dart';
import '../../../../../common/widget/index.dart';
import '../../../../../common/screen/index.dart';

class LiveInfoDetailWidget extends StatefulWidget {
  final LiveInfoManager manager;

  const LiveInfoDetailWidget({super.key, required this.manager});

  @override
  State<LiveInfoDetailWidget> createState() => _LiveInfoDetailWidgetState();
}

class _LiveInfoDetailWidgetState extends State<LiveInfoDetailWidget> {
  late final LiveInfoManager manager;

  @override
  void initState() {
    super.initState();
    manager = widget.manager;
    _getFansCount();
  }

  @override
  Widget build(BuildContext context) {
    return SizedBox(
      width: MediaQuery.of(context).size.width,
      height: context.adapter.getHeight(241),
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

  Widget _initBackground() {
    return Container(
        margin: EdgeInsets.only(top: context.adapter.getHeight(30)),
        width: MediaQuery.of(context).size.width,
        height: context.adapter.getHeight(211),
        decoration: BoxDecoration(
          color: LiveColors.designStandardG2,
          borderRadius: BorderRadius.only(
              topLeft: Radius.circular(context.adapter.getWidth(20)),
              topRight: Radius.circular(context.adapter.getWidth(20))),
        ));
  }

  Widget _initAnchorAvatarWidget() {
    return Container(
        margin: EdgeInsets.only(
            left: context.adapter.getWidth(4),
            right: context.adapter.getWidth(8)),
        width: context.adapter.getWidth(56),
        height: context.adapter.getWidth(56),
        child: ClipOval(
          child: ValueListenableBuilder(
            valueListenable: manager.state.ownerAvatarUrl,
            builder: (context, ownerAvatarUrl, child) {
              return Image.network(
                ownerAvatarUrl,
                fit: BoxFit.cover,
                errorBuilder: (context, error, stackTrace) {
                  return Image.asset(
                    LiveImages.defaultAvatar,
                    package: Constants.pluginName,
                  );
                },
              );
            },
          ),
        ));
  }

  Widget _initAnchorNameWidget() {
    return Positioned(
      top: context.adapter.getHeight(64),
      child: ValueListenableBuilder(
        valueListenable: manager.state.ownerName,
        builder: (context, ownerName, child) {
          return Text(
            ownerName,
            overflow: TextOverflow.ellipsis,
            style: const TextStyle(
                fontSize: 16,
                fontStyle: FontStyle.normal,
                color: LiveColors.designStandardG7),
          );
        },
      ),
    );
  }

  _initLiveIDWidget() {
    return Positioned(
      top: context.adapter.getHeight(94),
      child: Text(
        LiveKitLocalizations.of(Global.appContext())!.live_room_info_liveroom_id +
            manager.state.roomId,
        overflow: TextOverflow.ellipsis,
        style: const TextStyle(
            fontSize: 12,
            fontStyle: FontStyle.normal,
            color: LiveColors.designStandardG7),
      ),
    );
  }

  Widget _initFansWidget() {
    return Positioned(
      top: context.adapter.getHeight(119),
      child: ValueListenableBuilder(
        valueListenable: manager.state.fansNumber,
        builder: (context, fansNumber, child) {
          return Text(
            fansNumber.toString() +
                LiveKitLocalizations.of(Global.appContext())!.live_fan_count,
            overflow: TextOverflow.ellipsis,
            style: const TextStyle(
                fontSize: 12,
                fontStyle: FontStyle.normal,
                color: LiveColors.designStandardG7),
          );
        },
      ),
    );
  }

  Widget _initFollowWidget() {
    return Positioned(
      top: context.adapter.getHeight(144),
      width: context.adapter.getWidth(275),
      height: context.adapter.getHeight(40),
      child: Visibility(
        visible: manager.state.ownerId.value != manager.state.selfUserId,
        child: ValueListenableBuilder(
          valueListenable: manager.state.followingList,
          builder: (BuildContext context, value, Widget? child) {
            return GestureDetector(
              onTap: () {
                _followAnchor();
              },
              child: Container(
                margin: EdgeInsets.only(
                    left: context.adapter.getWidth(14),
                    right: context.adapter.getWidth(4)),
                decoration: BoxDecoration(
                  borderRadius:
                      BorderRadius.circular(context.adapter.getWidth(20)),
                  color: _isFollow()
                      ? LiveColors.designStandardG3
                      : LiveColors.designStandardB1,
                ),
                alignment: Alignment.center,
                child: Text(
                  _isFollow()
                      ? LiveKitLocalizations.of(Global.appContext())!
                          .live_unfollow_anchor
                      : LiveKitLocalizations.of(Global.appContext())!
                          .live_follow_anchor,
                  style: const TextStyle(
                      fontSize: 16,
                      fontStyle: FontStyle.normal,
                      color: LiveColors.designStandardG7),
                ),
              ),
            );
          },
        ),
      ),
    );
  }
}

extension on _LiveInfoDetailWidgetState {
  void _getFansCount() {
    manager.getFansNumber();
  }

  bool _isFollow() {
    return manager.state.followingList.value
        .any((following) => following.userId == manager.state.ownerId.value);
  }

  void _followAnchor() {
    _isFollow()
        ? manager.unfollowUser(manager.state.ownerId.value)
        : manager.followUser(manager.state.ownerId.value);
  }
}
