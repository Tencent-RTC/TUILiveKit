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
                topLeft: Radius.circular(context.adapter.getWidth(15)),
                topRight: Radius.circular(context.adapter.getWidth(15)))),
        height: context.adapter.getHeight(722),
        child: Column(
          children: [
            SizedBox(height: context.adapter.getHeight(20)),
            SizedBox(
              width: _screenWidth,
              height: context.adapter.getHeight(28),
              child: Center(
                child: Text(
                  LiveKitLocalizations.of(Global.appContext())!
                      .live_voiceroom_invite,
                  style: const TextStyle(
                      color: LiveColors.designStandardFlowkitWhite,
                      fontSize: 16),
                  textAlign: TextAlign.center,
                ),
              ),
            ),
            SizedBox(height: context.adapter.getHeight(24)),
            SizedBox(
                width: _screenWidth,
                height: context.adapter.getHeight(28),
                child: Padding(
                  padding: EdgeInsets.only(left: context.adapter.getWidth(24)),
                  child: Text(
                    LiveKitLocalizations.of(Global.appContext())!
                        .live_anchor_audience_list_panel_title,
                    style: const TextStyle(
                        color: LiveColors.designStandardFlowkitWhite,
                        fontSize: 16),
                    textAlign: TextAlign.left,
                  ),
                )),
            SizedBox(height: context.adapter.getHeight(16)),
            Padding(
              padding: EdgeInsets.symmetric(
                  horizontal: context.adapter.getWidth(24)),
              child: Container(
                  color: LiveColors.designStandardG3.withAlpha(0x4D),
                  height: context.adapter.getHeight(1)),
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
          padding:
              EdgeInsets.symmetric(horizontal: context.adapter.getWidth(24)),
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
      height: context.adapter.getHeight(60),
      color: LiveColors.designStandardG2,
      child: Row(
        mainAxisAlignment: MainAxisAlignment.spaceBetween,
        crossAxisAlignment: CrossAxisAlignment.center,
        mainAxisSize: MainAxisSize.max,
        children: [
          Row(
            children: [
              SizedBox(
                width: context.adapter.getWidth(40),
                height: context.adapter.getWidth(40),
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
              SizedBox(width: context.adapter.getHeight(12)),
              Container(
                alignment: Alignment.centerLeft,
                constraints:
                    BoxConstraints(maxWidth: context.adapter.getWidth(135)),
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
              width: context.adapter.getWidth(60),
              height: context.adapter.getHeight(24),
              decoration: BoxDecoration(
                color: audienceInvited[index].$2
                    ? LiveColors.designStandardTransparent
                    : LiveColors.notStandardBlue,
                border: Border.all(
                    color: audienceInvited[index].$2
                        ? LiveColors.notStandardRed
                        : LiveColors.notStandardBlue,
                    width: 1),
                borderRadius:
                    BorderRadius.circular(context.adapter.getHeight(12)),
              ),
              alignment: Alignment.center,
              child: Text(
                audienceInvited[index].$2
                    ? LiveKitLocalizations.of(Global.appContext())!.live_cancel
                    : LiveKitLocalizations.of(Global.appContext())!
                        .live_voiceroom_invite,
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
      totalHeight = audienceInvited.length * context.adapter.getHeight(60);
    }
    return totalHeight > context.adapter.getHeight(280)
        ? context.adapter.getHeight(280)
        : totalHeight;
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
                    .live_voiceroom_invite_seat_canceled);
            break;
          case RequestResultType.onCancelled:
            manager.onRespondedSeatInvitation(audience.userId);
            break;
          case RequestResultType.onTimeout:
            manager.onRespondedSeatInvitation(audience.userId);
            manager.toastSubject.add(
                LiveKitLocalizations.of(Global.appContext())!
                    .live_voiceroom_invite_seat_canceled);
            break;
          default:
            break;
        }
      } else {
        manager.toastSubject.add(ErrorHandler.convertToErrorMessage(
                result.code.value(), result.message) ??
            '');
      }
      return;
    }

    final result = await seatGridController.cancelRequest(audience.userId);
    if (result.code == TUIError.success) {
      manager.onRespondedSeatInvitation(audience.userId);
    } else {
      manager.toastSubject.add(ErrorHandler.convertToErrorMessage(
              result.code.value(), result.message) ??
          '');
    }
  }
}
