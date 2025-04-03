import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/room/tui_room_engine.dart';
import 'package:tencent_cloud_chat_sdk/tencent_im_sdk_plugin.dart';
import 'package:tencent_live_uikit/voice_room/widget/component/live_info/manager/live_info_manager.dart';
import 'package:tencent_live_uikit/voice_room/widget/component/live_info/manager/live_info_engine_observer.dart';
import 'package:tencent_live_uikit/voice_room/widget/component/live_info/manager/live_info_im_observer.dart';

import '../../../../common/constants/constants.dart';
import '../../../../common/language/index.dart';
import '../../../../common/resources/colors.dart';
import '../../../../common/resources/images.dart';
import '../../../../common/widget/index.dart';
import '../../../../common/screen/index.dart';
import 'panel/live_info_detail_widget.dart';

class LiveInfoWidget extends StatefulWidget {
  final String roomId;

  const LiveInfoWidget({super.key, required this.roomId});

  @override
  State<LiveInfoWidget> createState() => _LiveInfoWidgetState();
}

class _LiveInfoWidgetState extends State<LiveInfoWidget> {
  final LiveInfoManager manager = LiveInfoManager();
  late final LiveInfoIMObserver imObserver =
      LiveInfoIMObserver(manager: WeakReference(manager));
  late final LiveInfoEngineObserver engineObserver =
      LiveInfoEngineObserver(manager: WeakReference(manager));

  @override
  void initState() {
    super.initState();
    manager.initRoomInfo(widget.roomId);
    TUIRoomEngine.sharedInstance().addObserver(engineObserver);
    TencentImSDKPlugin.v2TIMManager
        .getFriendshipManager()
        .addFriendListener(listener: imObserver);
  }

  @override
  void dispose() {
    super.dispose();
    TUIRoomEngine.sharedInstance().removeObserver(engineObserver);
    manager.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final isOwner = manager.state.ownerId.value == manager.state.selfUserId;
    return GestureDetector(
      onTap: () {
        _showLiveInfoDetailWidget();
      },
      child: Container(
        constraints: BoxConstraints(
            maxWidth: isOwner
                ? context.adapter.getWidth(160)
                : context.adapter.getWidth(200)),
        height: context.adapter.getHeight(40),
        decoration: BoxDecoration(
          color: LiveColors.black.withAlpha(0x40),
          borderRadius:
              BorderRadius.all(Radius.circular(context.adapter.getHeight(20))),
        ),
        child: Row(
          mainAxisAlignment: MainAxisAlignment.start,
          mainAxisSize: MainAxisSize.min,
          crossAxisAlignment: CrossAxisAlignment.center,
          children: [
            _initOwnerAvatarWidget(),
            _initOwnerNameWidget(),
            SizedBox(width: context.adapter.getWidth(12)),
            _initFollowWidget(),
          ],
        ),
      ),
    );
  }

  Widget _initOwnerAvatarWidget() {
    return Container(
        margin: EdgeInsets.only(
            left: context.adapter.getWidth(4),
            right: context.adapter.getWidth(4)),
        width: context.adapter.getWidth(32),
        height: context.adapter.getWidth(32),
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
      constraints: BoxConstraints(maxWidth: context.adapter.getWidth(96)),
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
                margin: EdgeInsets.only(right: context.adapter.getWidth(8)),
                width: context.adapter.getWidth(44),
                height: context.adapter.getHeight(24),
                decoration: BoxDecoration(
                  borderRadius:
                      BorderRadius.circular(context.adapter.getHeight(12)),
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
                          width: context.adapter.getWidth(16),
                          height: context.adapter.getWidth(16),
                        ),
                      )
                    : GestureDetector(
                        onTap: () {
                          _followAnchor();
                        },
                        child: Text(
                          LiveKitLocalizations.of(Global.appContext())!
                              .live_follow_anchor,
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

  void _showLiveInfoDetailWidget() {
    _popupWidget(LiveInfoDetailWidget(manager: manager));
  }

  void _popupWidget(Widget widget, {Color? barrierColor}) {
    showModalBottomSheet(
      barrierColor: barrierColor,
      isScrollControlled: true,
      context: Global.appContext(),
      backgroundColor: LiveColors.designStandardTransparent,
      builder: (context) => Container(
        decoration: BoxDecoration(
          borderRadius: BorderRadius.only(
            topLeft: Radius.circular(context.adapter.getWidth(20)),
            topRight: Radius.circular(context.adapter.getWidth(20)),
          ),
          color: LiveColors.designStandardTransparent,
        ),
        child: widget,
      ),
    );
  }
}
