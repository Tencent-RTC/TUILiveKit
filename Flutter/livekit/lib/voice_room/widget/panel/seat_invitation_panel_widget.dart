import 'package:flutter/material.dart';
import 'package:live_stream_core/live_stream_core.dart';
import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:tencent_live_uikit/common/error/error_handler.dart';
import 'package:tencent_live_uikit/voice_room/index.dart';

import '../../../common/screen/index.dart';
import '../../../common/resources/index.dart';
import '../../../common/widget/index.dart';
import '../../../common/language/index.dart';
import '../../../common/constants/index.dart';

class SeatInvitationPanelWidget extends StatefulWidget {
  final VoiceRoomManager manager;
  final SeatGridController seatGridController;
  final int seatIndex;

  const SeatInvitationPanelWidget(
      {super.key,
      required this.manager,
      required this.seatGridController,
      required this.seatIndex});

  @override
  State<SeatInvitationPanelWidget> createState() =>
      _SeatInvitationPanelWidgetState();
}

class _SeatInvitationPanelWidgetState extends State<SeatInvitationPanelWidget> {
  late final VoiceRoomManager manager;
  late final SeatGridController seatGridController;
  late final int seatIndex;
  late double _screenWidth;

  final List<(User, bool)> audienceInvited = [];

  @override
  void initState() {
    super.initState();
    manager = widget.manager;
    seatGridController = widget.seatGridController;
    seatIndex = widget.seatIndex;
  }

  @override
  Widget build(BuildContext context) {
    _screenWidth = MediaQuery.of(context).size.width;
    return Container(
        decoration: BoxDecoration(
            borderRadius: BorderRadius.only(
                topLeft: Radius.circular(15.width),
                topRight: Radius.circular(15.width))),
        height: 722.height,
        child: Column(
          children: [
            SizedBox(height: 20.height),
            SizedBox(
              width: _screenWidth,
              height: 28.height,
              child: Center(
                child: Text(
                  LiveKitLocalizations.of(Global.appContext())!
                      .common_voiceroom_invite,
                  style: const TextStyle(
                      color: LiveColors.designStandardFlowkitWhite,
                      fontSize: 16),
                  textAlign: TextAlign.center,
                ),
              ),
            ),
            SizedBox(height: 24.height),
            SizedBox(
                width: _screenWidth,
                height: 28.height,
                child: Padding(
                  padding: EdgeInsets.only(left: 24.width),
                  child: Text(
                    LiveKitLocalizations.of(Global.appContext())!
                        .common_anchor_audience_list_panel_title,
                    style: const TextStyle(
                        color: LiveColors.designStandardFlowkitWhite,
                        fontSize: 16),
                    textAlign: TextAlign.left,
                  ),
                )),
            SizedBox(height: 16.height),
            Padding(
              padding: EdgeInsets.symmetric(horizontal: 24.width),
              child: Container(
                  color: LiveColors.designStandardG3.withAlpha(0x4D),
                  height: 1.height),
            ),
            _initAudienceInvitationListWidget()
          ],
        ));
  }

  Widget _initAudienceInvitationListWidget() {
    return ListenableBuilder(
      listenable: Listenable.merge([
        manager.userState.userList,
        manager.seatState.seatList,
        manager.seatState.invitedUserIds
      ]),
      builder: (context, child) {
        final seatUserIds =
            manager.seatState.seatList.value.map((seat) => seat.userId).toSet();
        final selfUserId = manager.userState.selfInfo.userId;
        final invitedIds = manager.seatState.invitedUserIds.value.toSet();
        final invitableUsers = manager.userState.userList.value
            .where((user) =>
                user.userId != selfUserId && !seatUserIds.contains(user.userId))
            .toList();

        audienceInvited
          ..clear()
          ..addAll(invitableUsers
              .map((user) => (user, invitedIds.contains(user.userId))));

        return Padding(
          padding: EdgeInsets.symmetric(horizontal: 24.width),
          child: Visibility(
            visible: audienceInvited.isNotEmpty,
            child: SizedBox(
              height: _calculateAudienceInvitationListHeight(),
              child: ListView.builder(
                  shrinkWrap: true,
                  physics: const ClampingScrollPhysics(),
                  scrollDirection: Axis.vertical,
                  itemCount: audienceInvited.length,
                  itemBuilder: (context, index) {
                    return _buildAudienceInvitationItem(index);
                  }),
            ),
          ),
        );
      },
    );
  }

  Widget _buildAudienceInvitationItem(int index) {
    return Container(
      height: 60.height,
      color: LiveColors.designStandardG2,
      child: Row(
        mainAxisAlignment: MainAxisAlignment.spaceBetween,
        crossAxisAlignment: CrossAxisAlignment.center,
        mainAxisSize: MainAxisSize.max,
        children: [
          Row(
            children: [
              SizedBox(
                width: 40.radius,
                height: 40.radius,
                child: Stack(
                  children: [
                    ClipOval(
                      child: Image.network(
                        audienceInvited[index].$1.avatarUrl,
                        fit: BoxFit.cover,
                        errorBuilder: (context, error, stackTrace) {
                          return Image.asset(
                            LiveImages.defaultAvatar,
                            package: Constants.pluginName,
                          );
                        },
                      ),
                    ),
                  ],
                ),
              ),
              SizedBox(width: 12.width),
              Container(
                alignment: Alignment.centerLeft,
                constraints: BoxConstraints(maxWidth: 135.width),
                child: Text(
                  audienceInvited[index].$1.name.isNotEmpty
                      ? audienceInvited[index].$1.name
                      : audienceInvited[index].$1.userId,
                  style: const TextStyle(
                      color: LiveColors.designStandardG6, fontSize: 16),
                  overflow: TextOverflow.ellipsis,
                ),
              ),
            ],
          ),
          DebounceGestureRecognizer(
            onTap: () {
              _handleAudienceInvitation(
                  audienceInvited[index].$1, audienceInvited[index].$2);
            },
            child: Container(
              width: 60.width,
              height: 24.height,
              decoration: BoxDecoration(
                color: audienceInvited[index].$2
                    ? LiveColors.designStandardTransparent
                    : LiveColors.notStandardBlue,
                border: Border.all(
                    color: audienceInvited[index].$2
                        ? LiveColors.notStandardRed
                        : LiveColors.notStandardBlue,
                    width: 1),
                borderRadius: BorderRadius.circular(12.radius),
              ),
              alignment: Alignment.center,
              child: Text(
                audienceInvited[index].$2
                    ? LiveKitLocalizations.of(Global.appContext())!
                        .common_cancel
                    : LiveKitLocalizations.of(Global.appContext())!
                        .common_voiceroom_invite,
                style: TextStyle(
                    color: audienceInvited[index].$2
                        ? LiveColors.notStandardRed
                        : LiveColors.designStandardFlowkitWhite,
                    fontSize: 12),
              ),
            ),
          )
        ],
      ),
    );
  }

  double _calculateAudienceInvitationListHeight() {
    double totalHeight = 0;
    if (audienceInvited.isNotEmpty) {
      totalHeight = audienceInvited.length * 60.height;
    }
    return totalHeight > 280.height ? 280.height : totalHeight;
  }

  void _handleAudienceInvitation(User audience, bool isInvited) async {
    if (!isInvited) {
      if (manager.seatState.invitedUserIds.value
          .any((userId) => userId == audience.userId)) {
        return;
      }

      manager.onSentSeatInvitation(audience.userId);
      final future = seatGridController.takeUserOnSeatByAdmin(
          seatIndex, audience.userId, defaultTimeout);

      if (seatIndex != -1 && mounted) {
        Navigator.of(context).pop();
      }

      final result = await future;
      if (result.code == TUIError.success) {
        switch (result.type) {
          case RequestResultType.onAccepted:
            manager.onRespondedSeatInvitation(audience.userId);
            break;
          case RequestResultType.onRejected:
            manager.onRespondedSeatInvitation(audience.userId);
            manager.toastSubject.add(
                LiveKitLocalizations.of(Global.appContext())!
                    .common_voiceroom_invite_seat_canceled);
            break;
          case RequestResultType.onCancelled:
            manager.onRespondedSeatInvitation(audience.userId);
            break;
          case RequestResultType.onTimeout:
            manager.onRespondedSeatInvitation(audience.userId);
            manager.toastSubject.add(
                LiveKitLocalizations.of(Global.appContext())!
                    .common_voiceroom_invite_seat_canceled);
            break;
          default:
            break;
        }
      } else {
        manager.toastSubject.add(ErrorHandler.convertToErrorMessage(
                result.code.rawValue, result.message) ??
            '');
      }
      return;
    }

    final result = await seatGridController.cancelRequest(audience.userId);
    if (result.code == TUIError.success) {
      manager.onRespondedSeatInvitation(audience.userId);
    } else {
      manager.toastSubject.add(ErrorHandler.convertToErrorMessage(
              result.code.rawValue, result.message) ??
          '');
    }
  }
}
