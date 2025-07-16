import 'dart:collection';

import 'package:flutter/material.dart';
import 'package:live_stream_core/live_stream_core.dart';
import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/voice_room/index.dart';

class UserManagementPanelWidget extends StatefulWidget {
  final VoiceRoomManager manager;
  final SeatGridController seatGridController;
  final TUISeatInfo seatInfo;

  const UserManagementPanelWidget(
      {super.key,
      required this.manager,
      required this.seatGridController,
      required this.seatInfo});

  @override
  State<UserManagementPanelWidget> createState() =>
      _UserManagementPanelWidgetState();
}

class _UserManagementPanelWidgetState extends State<UserManagementPanelWidget> {
  late final VoiceRoomManager manager;
  late final SeatGridController seatGridController;
  late final TUISeatInfo seatInfo;
  late double _screenWidth;
  final ValueNotifier<bool> seatAudioLocked = ValueNotifier(false);

  @override
  void initState() {
    super.initState();
    manager = widget.manager;
    seatGridController = widget.seatGridController;
    seatInfo = widget.seatInfo;
    seatAudioLocked.value = seatInfo.isAudioLocked ?? false;
    _subscribeEmptySeatStatus();
  }

  @override
  void dispose() {
    _unsubscribeEmptySeatStatus();
    super.dispose();
  }

  void _subscribeEmptySeatStatus() {
    manager.seatState.seatList.addListener(_handleSeatStatusChanged);
  }

  void _unsubscribeEmptySeatStatus() {
    manager.seatState.seatList.removeListener(_handleSeatStatusChanged);
  }

  void _handleSeatStatusChanged() {
    final currentSeatInfo = manager.seatState.seatList.value
        .where((seat) => seat.index == seatInfo.index)
        .firstOrNull;
    if (currentSeatInfo != null && currentSeatInfo.userId.isEmpty) {
      Navigator.of(context).pop();
    }
  }

  @override
  Widget build(BuildContext context) {
    _screenWidth = MediaQuery.of(context).size.width;
    final isOwner =
        manager.roomState.ownerInfo.userId == manager.userState.selfInfo.userId;
    return Container(
      width: _screenWidth,
      constraints: BoxConstraints(minHeight: 88.height, maxHeight: 179.height),
      decoration: BoxDecoration(
          borderRadius: BorderRadius.only(
              topLeft: Radius.circular(15.width), topRight: Radius.circular(15.width))),
      height: isOwner ? 179.height : 99.height,
      child: Column(children: [
        SizedBox(height: 24.height),
        _initUserInfoWidget(),
        SizedBox(height: 20.height),
        _initSeatManagementWidget()
      ]),
    );
  }

  Widget _initUserInfoWidget() {
    return Padding(
      padding: EdgeInsets.symmetric(horizontal: 24.width),
      child: SizedBox(
        width: _screenWidth,
        child: Stack(
          children: [
            SizedBox(
              width: 40.radius,
              height: 40.radius,
              child: ClipOval(
                child: Image.network(
                  seatInfo.avatarUrl ?? '',
                  errorBuilder: (context, error, _) => Image.asset(
                      LiveImages.defaultAvatar,
                      package: Constants.pluginName),
                ),
              ),
            ),
            Positioned(
                top: 0,
                left: 52.width,
                child: Column(
                  mainAxisSize: MainAxisSize.min,
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Text(seatInfo.userName ?? '',
                        style: const TextStyle(
                            color: LiveColors.designStandardG6, fontSize: 16),
                        textAlign: TextAlign.left,
                        overflow: TextOverflow.ellipsis),
                    Text('Id: ${seatInfo.userId}',
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
                  valueListenable: manager.userState.myFollowingUserList,
                  builder: (context, followingUserList, child) {
                    final isFollowed = followingUserList.any((followingUser) =>
                        followingUser.userId == seatInfo.userId);
                    return GestureDetector(
                      onTap: () => _followUser(seatInfo.userId, !isFollowed),
                      child: Container(
                        width: 70.width,
                        height: 32.height,
                        decoration: BoxDecoration(
                            color: isFollowed
                                ? LiveColors.notStandardGreyC5
                                : LiveColors.notStandardBlue,
                            borderRadius: BorderRadius.circular(16.height)),
                        child: Center(
                          child: isFollowed
                              ? Image.asset(
                                  LiveImages.followed,
                                  package: Constants.pluginName,
                                  width: 16.radius,
                                  height: 16.radius,
                                )
                              : Text(
                                  LiveKitLocalizations.of(Global.appContext())!
                                      .common_follow_anchor,
                                  style: const TextStyle(
                                      fontSize: 12,
                                      fontStyle: FontStyle.normal,
                                      color: LiveColors.designStandardG7),
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

  Widget _initSeatManagementWidget() {
    final isOwner =
        manager.roomState.ownerInfo.userId == manager.userState.selfInfo.userId;
    return Visibility(
        visible: isOwner,
        child: Padding(
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
                  valueListenable: seatAudioLocked,
                  builder: (context, seatAudioLocked, child) {
                    return GestureDetector(
                      onTap: () => _lockAudio(),
                      child: Column(
                        children: [
                          Container(
                              decoration: BoxDecoration(
                                  color: LiveColors.designStandardG3,
                                  borderRadius: BorderRadius.circular(12.radius)),
                              width: 50.radius,
                              height: 50.radius,
                              child: Center(
                                child: Image.asset(
                                    seatAudioLocked
                                        ? LiveImages.unmuteMicrophone
                                        : LiveImages.muteMicrophone,
                                    package: Constants.pluginName,
                                    width: 25.radius,
                                    height: 25.radius),
                              )),
                          Text(
                            seatAudioLocked
                                ? LiveKitLocalizations.of(Global.appContext())!
                                    .common_voiceroom_unmuted_seat
                                : LiveKitLocalizations.of(Global.appContext())!
                                    .common_voiceroom_mute_seat,
                            style: const TextStyle(
                                color: LiveColors.designStandardG6,
                                fontSize: 12),
                          )
                        ],
                      ),
                    );
                  },
                ),
                GestureDetector(
                  onTap: () {
                    _kickOffSeat();
                    Navigator.of(context).pop();
                  },
                  child: Column(
                    children: [
                      Container(
                          decoration: BoxDecoration(
                              color: LiveColors.designStandardG3,
                              borderRadius: BorderRadius.circular(12.5.radius)),
                          width: 50.radius,
                          height: 50.radius,
                          child: Center(
                            child: Image.asset(LiveImages.anchorKickoff,
                                package: Constants.pluginName,
                                width: 25.radius,
                                height: 25.radius),
                          )),
                      Text(
                        LiveKitLocalizations.of(Global.appContext())!
                            .common_end_user,
                        style: const TextStyle(
                            color: LiveColors.designStandardG6, fontSize: 12),
                      )
                    ],
                  ),
                )
              ],
            ),
          ),
        ));
  }
}

extension on _UserManagementPanelWidgetState {
  void _followUser(String userId, bool isFollow) {
    final user = User();
    user.userId = userId;
    manager.followUser(user, isFollow);
  }

  void _lockAudio() async {
    final lockParam = TUISeatLockParams();
    lockParam.lockAudio = !seatAudioLocked.value;
    final result = await seatGridController.lockSeat(seatInfo.index, lockParam);
    if (result.code == TUIError.success) {
      seatAudioLocked.value = !seatAudioLocked.value;
    }
  }

  void _kickOffSeat() {
    seatGridController.kickUserOffSeatByAdmin(seatInfo.userId);
  }
}
