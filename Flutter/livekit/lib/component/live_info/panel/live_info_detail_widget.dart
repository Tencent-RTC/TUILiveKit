import 'package:flutter/material.dart';

import '../../../../../common/constants/constants.dart';
import '../../../../../common/language/index.dart';
import '../../../../../common/resources/colors.dart';
import '../../../../../common/resources/images.dart';
import '../../../../../common/widget/index.dart';
import '../../../../../common/screen/index.dart';
import '../manager/live_info_manager.dart';

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
      height: 241.height,
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
        margin: EdgeInsets.only(top: 30.height),
        width: MediaQuery.of(context).size.width,
        height: 211.height,
        decoration: BoxDecoration(
          color: LiveColors.designStandardG2,
          borderRadius: BorderRadius.only(
              topLeft: Radius.circular(20.radius),
              topRight: Radius.circular(20.radius)),
        ));
  }

  Widget _initAnchorAvatarWidget() {
    return Container(
        margin: EdgeInsets.only(left: 4.width, right: 8.width),
        width: 56.radius,
        height: 56.radius,
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
      top: 64.height,
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
      top: 94.height,
      child: Text(
        LiveKitLocalizations.of(Global.appContext())!
                .common_room_info_liveroom_id +
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
      top: 119.height,
      child: ValueListenableBuilder(
        valueListenable: manager.state.fansNumber,
        builder: (context, fansNumber, child) {
          return Text(
            fansNumber.toString() +
                LiveKitLocalizations.of(Global.appContext())!.common_fan_count,
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
      top: 144.height,
      width: 275.width,
      height: 40.height,
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
                margin: EdgeInsets.only(left: 14.width, right: 4.width),
                decoration: BoxDecoration(
                  borderRadius: BorderRadius.circular(20.radius),
                  color: _isFollow()
                      ? LiveColors.designStandardG3
                      : LiveColors.designStandardB1,
                ),
                alignment: Alignment.center,
                child: Text(
                  _isFollow()
                      ? LiveKitLocalizations.of(Global.appContext())!
                          .common_unfollow_anchor
                      : LiveKitLocalizations.of(Global.appContext())!
                          .common_follow_anchor,
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
