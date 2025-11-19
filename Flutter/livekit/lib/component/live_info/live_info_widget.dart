import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_cloud_chat_sdk/tencent_im_sdk_plugin.dart';

import '../../../../common/index.dart';
import 'manager/live_info_engine_observer.dart';
import 'manager/live_info_im_observer.dart';
import 'manager/live_info_manager.dart';
import 'panel/live_info_detail_widget.dart';

class LiveInfoWidget extends StatefulWidget {
  final String roomId;
  final ValueListenable<bool>? isFloatWindowMode;
  const LiveInfoWidget({super.key, required this.roomId, this.isFloatWindowMode});

  @override
  State<LiveInfoWidget> createState() => _LiveInfoWidgetState();
}

class _LiveInfoWidgetState extends State<LiveInfoWidget> {
  final LiveInfoManager manager = LiveInfoManager();
  late final LiveInfoIMObserver imObserver = LiveInfoIMObserver(manager: WeakReference(manager));
  late final LiveInfoEngineObserver engineObserver = LiveInfoEngineObserver(manager: WeakReference(manager));
  bool _isShowingLiveInfoDetailWidget = false;
  late final VoidCallback _floatWindowModeChangedListener = _onFloatWindowModeChanged;

  @override
  void initState() {
    super.initState();
    manager.initRoomInfo(widget.roomId);
    TUIRoomEngine.sharedInstance().addObserver(engineObserver);
    TencentImSDKPlugin.v2TIMManager
        .getFriendshipManager()
        .addFriendListener(listener: imObserver);
    widget.isFloatWindowMode?.addListener(_floatWindowModeChangedListener);
  }

  @override
  void dispose() {
    _closeLiveInfoDetailWidget();
    super.dispose();
    TUIRoomEngine.sharedInstance().removeObserver(engineObserver);
    widget.isFloatWindowMode?.removeListener(_floatWindowModeChangedListener);
    manager.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final isOwner = manager.state.ownerId.value == manager.state.selfUserId;
    return GestureDetector(
      onTap: () {
        _showLiveInfoDetailWidget(context);
      },
      child: Container(
        constraints: BoxConstraints(maxWidth: isOwner ? 160.width : 200.width),
        height: 40.height,
        decoration: BoxDecoration(
          color: LiveColors.black.withAlpha(0x40),
          borderRadius: BorderRadius.all(Radius.circular(20.height)),
        ),
        child: Row(
          mainAxisAlignment: MainAxisAlignment.start,
          mainAxisSize: MainAxisSize.min,
          crossAxisAlignment: CrossAxisAlignment.center,
          children: [
            _initOwnerAvatarWidget(),
            _initOwnerNameWidget(),
            SizedBox(width: 12.width),
            _initFollowWidget(),
          ],
        ),
      ),
    );
  }

  Widget _initOwnerAvatarWidget() {
    return Container(
        margin: EdgeInsets.only(left: 4.width, right: 4.width),
        width: 32.radius,
        height: 32.radius,
        child: ClipOval(
          child: ValueListenableBuilder(
            valueListenable: manager.state.ownerAvatarUrl,
            builder: (context, avatarUrl, child) {
              return Image.network(
                avatarUrl,
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

  Widget _initOwnerNameWidget() {
    return Container(
      constraints: BoxConstraints(maxWidth: 96.width),
      child: ValueListenableBuilder(
          valueListenable: manager.state.ownerName,
          builder: (context, ownerName, child) {
            return Text(
              ownerName,
              overflow: TextOverflow.ellipsis,
              style: const TextStyle(
                  fontSize: 12,
                  fontWeight: FontWeight.w500,
                  fontStyle: FontStyle.normal,
                  color: LiveColors.designStandardG7),
            );
          }),
    );
  }

  Widget _initFollowWidget() {
    return ValueListenableBuilder(
      valueListenable: manager.state.ownerId,
      builder: (context, value, child) {
        return Visibility(
          visible: manager.state.ownerId.value != manager.state.selfUserId,
          child: ValueListenableBuilder(
            valueListenable: manager.state.followingList,
            builder: (BuildContext context, value, Widget? child) {
              return Container(
                margin: EdgeInsets.only(right: 8.width),
                width: 44.width,
                height: 24.height,
                decoration: BoxDecoration(
                  borderRadius: BorderRadius.circular(12.height),
                  color: _isFollow()
                      ? LiveColors.notStandardGreyC5
                      : LiveColors.notStandardBlue,
                ),
                alignment: Alignment.center,
                child: _isFollow()
                    ? GestureDetector(
                        onTap: () {
                          _unfollowAnchor();
                        },
                        child: Image.asset(
                          LiveImages.followed,
                          package: Constants.pluginName,
                          width: 16.radius,
                          height: 16.radius,
                        ),
                      )
                    : GestureDetector(
                        onTap: () {
                          _followAnchor();
                        },
                        child: Text(
                          LiveKitLocalizations.of(Global.appContext())!
                              .common_follow_anchor,
                          style: const TextStyle(
                              fontSize: 12,
                              fontStyle: FontStyle.normal,
                              color: LiveColors.designStandardG7),
                        ),
                      ),
              );
            },
          ),
        );
      },
    );
  }
}

extension on _LiveInfoWidgetState {
  bool _isFollow() {
    return manager.state.followingList.value
        .any((following) => following.userId == manager.state.ownerId.value);
  }

  void _followAnchor() {
    manager.followUser(manager.state.ownerId.value);
  }

  void _unfollowAnchor() {
    manager.unfollowUser(manager.state.ownerId.value);
  }

  void _showLiveInfoDetailWidget(BuildContext context) {
    if (MediaQuery.orientationOf(context) != Orientation.portrait) return;
    _popupWidget(LiveInfoDetailWidget(manager: manager));
  }

  void _popupWidget(Widget widget, {Color? barrierColor}) {
    _isShowingLiveInfoDetailWidget = true;
    showModalBottomSheet<void>(
      barrierColor: barrierColor,
      isScrollControlled: true,
      context: Global.appContext(),
      backgroundColor: LiveColors.designStandardTransparent,
      builder: (context) => Container(
        decoration: BoxDecoration(
          borderRadius: BorderRadius.only(
            topLeft: Radius.circular(20.width),
            topRight: Radius.circular(20.width),
          ),
          color: LiveColors.designStandardTransparent,
        ),
        child: widget,
      ),
    ).then((value){
      _isShowingLiveInfoDetailWidget = false;
    });
  }

  void _closeLiveInfoDetailWidget() {
    if (_isShowingLiveInfoDetailWidget) {
      Navigator.of(Global.appContext()).pop();
      _isShowingLiveInfoDetailWidget = false;
    }
  }

  void _onFloatWindowModeChanged() {
    if (widget.isFloatWindowMode != null) {
      if (widget.isFloatWindowMode!.value) {
        _closeLiveInfoDetailWidget();
      }
    }
  }
}
