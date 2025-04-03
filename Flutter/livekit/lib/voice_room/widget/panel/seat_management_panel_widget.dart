import 'package:flutter/material.dart';
import 'package:live_stream_core/live_stream_core.dart';
import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/voice_room/index.dart';
import 'package:tencent_live_uikit/voice_room/widget/panel/seat_invitation_panel_widget.dart';

class SeatManagementPanelWidget extends StatefulWidget {
  final VoiceRoomManager manager;
  final SeatGridController seatGridController;

  const SeatManagementPanelWidget(
      {super.key, required this.manager, required this.seatGridController});

  @override
  State<SeatManagementPanelWidget> createState() =>
      _SeatManagementPanelWidgetState();
}

class _SeatManagementPanelWidgetState extends State<SeatManagementPanelWidget> {
  late final VoiceRoomManager manager;
  late final SeatGridController seatGridController;
  late double _screenWidth;
  final currentSeatList = <SeatInfo>[];

  @override
  void initState() {
    super.initState();
    manager = widget.manager;
    seatGridController = widget.seatGridController;
  }

  @override
  Widget build(BuildContext context) {
    _screenWidth = MediaQuery.of(context).size.width;
    return Container(
      width: _screenWidth,
      decoration: BoxDecoration(
          borderRadius: BorderRadius.only(
              topLeft: Radius.circular(context.adapter.getWidth(15)),
              topRight: Radius.circular(context.adapter.getWidth(15)))),
      height: context.adapter.getHeight(724),
      child: Column(
        children: [
          SizedBox(height: context.adapter.getHeight(20)),
          SizedBox(
              width: _screenWidth,
              height: context.adapter.getHeight(28),
              child: Stack(alignment: Alignment.center, children: [
                Text(
                  LiveKitLocalizations.of(Global.appContext())!
                      .live_link_mic_manager,
                  style: const TextStyle(
                      color: LiveColors.designStandardFlowkitWhite,
                      fontSize: 16),
                  textAlign: TextAlign.center,
                ),
                Positioned(
                  top: context.adapter.getHeight(2),
                  right: context.adapter.getWidth(26),
                  child: SizedBox(
                    width: context.adapter.getWidth(20),
                    height: context.adapter.getWidth(20),
                    child: GestureDetector(
                      onTap: () => _showSeatInvitationPanel(),
                      child: Image.asset(LiveImages.invitation,
                          package: Constants.pluginName),
                    ),
                  ),
                ),
              ])),
          SizedBox(height: context.adapter.getHeight(24)),
          SizedBox(
              width: _screenWidth,
              height: context.adapter.getHeight(26),
              child: Stack(alignment: Alignment.center, children: [
                Positioned(
                  left: context.adapter.getWidth(24),
                  child: Text(
                    LiveKitLocalizations.of(Global.appContext())!
                        .live_voiceroom_need_agree,
                    style: const TextStyle(
                        color: LiveColors.designStandardFlowkitWhite,
                        fontSize: 16),
                    textAlign: TextAlign.left,
                  ),
                ),
                Positioned(
                  right: context.adapter.getWidth(24),
                  child: ValueListenableBuilder(
                      valueListenable: manager.roomState.seatMode,
                      builder: (context, seatMode, child) {
                        return SizedBox(
                          width: context.adapter.getWidth(45),
                          height: context.adapter.getHeight(24),
                          child: Switch(
                              activeTrackColor: Colors.blue,
                              value: seatMode == TUISeatMode.applyToTake,
                              onChanged: (opened) {
                                final mode = opened
                                    ? TUISeatMode.applyToTake
                                    : TUISeatMode.freeToTake;
                                manager.setRoomSeatModeByAdmin(mode);
                              }),
                        );
                      }),
                )
              ])),
          SizedBox(height: context.adapter.getHeight(16)),
          Padding(
            padding:
                EdgeInsets.symmetric(horizontal: context.adapter.getWidth(24)),
            child: Container(
                color: LiveColors.designStandardG3.withAlpha(0x4D),
                height: context.adapter.getHeight(1)),
          ),
          ListenableBuilder(
              listenable: Listenable.merge([
                manager.seatState.seatApplicationList,
                manager.seatState.seatList
              ]),
              builder: (context, child) {
                final seatedCount = manager.seatState.seatList.value
                    .where((seatInfo) =>
                        seatInfo.userId.isNotEmpty &&
                        seatInfo.userId != manager.userState.selfInfo.userId)
                    .length;
                final isAllEmpty = (seatedCount == 0) &&
                    (manager.seatState.seatApplicationList.value.isEmpty);
                return isAllEmpty
                    ? _initEmptySeatManagement()
                    : _initSeatManagement();
              })
        ],
      ),
    );
  }

  Widget _initEmptySeatManagement() {
    return SizedBox(
        width: _screenWidth,
        height: context.adapter.getHeight(609),
        child: Column(
          children: [
            SizedBox(height: context.adapter.getHeight(217)),
            Text(
              LiveKitLocalizations.of(Global.appContext())!
                  .live_voiceroom_empty_view,
              style: const TextStyle(
                  color: LiveColors.designStandardG5, fontSize: 16),
              textAlign: TextAlign.center,
            ),
            SizedBox(height: context.adapter.getHeight(23)),
            GestureDetector(
              onTap: () => _showSeatInvitationPanel(),
              child: Container(
                width: context.adapter.getWidth(201),
                height: context.adapter.getHeight(40),
                alignment: Alignment.center,
                decoration: BoxDecoration(
                  borderRadius: BorderRadius.circular(20),
                  color: LiveColors.designStandardB1,
                ),
                child: Text(
                  LiveKitLocalizations.of(Global.appContext())!
                      .live_voiceroom_invite,
                  style: const TextStyle(
                      color: LiveColors.designStandardFlowkitWhite,
                      fontSize: 16,
                      fontWeight: FontWeight.w700),
                ),
              ),
            ),
          ],
        ));
  }

  Widget _initSeatManagement() {
    return Padding(
      padding: EdgeInsets.symmetric(horizontal: context.adapter.getWidth(24)),
      child: SingleChildScrollView(
          child: Column(
              mainAxisAlignment: MainAxisAlignment.start,
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
            SizedBox(height: context.adapter.getHeight(16)),
            _initMicUpTitleWidget(),
            _initMicUpUserListWidget(),
            _initSeparationWidget(),
            _initMicDownTitleWidget(),
            _initMicDownUserListWidget(),
          ])),
    );
  }

  Widget _initMicUpTitleWidget() {
    return ValueListenableBuilder(
      valueListenable: manager.seatState.seatList,
      builder: (context, seatList, child) {
        final seatedCount = seatList
            .where((seatInfo) =>
                seatInfo.userId.isNotEmpty &&
                seatInfo.userId != manager.userState.selfInfo.userId)
            .length;
        return Visibility(
          visible: seatedCount > 0,
          child: Text(
            "${LiveKitLocalizations.of(Global.appContext())!.live_on_seat_list}"
            "($seatedCount/9)",
            style: const TextStyle(
                color: LiveColors.designStandardG7, fontSize: 16),
          ),
        );
      },
    );
  }

  Widget _initMicUpUserListWidget() {
    return ValueListenableBuilder(
      valueListenable: manager.seatState.seatList,
      builder: (context, seatList, child) {
        final seatedList = seatList.where((seatInfo) =>
            seatInfo.userId.isNotEmpty &&
            seatInfo.userId != manager.userState.selfInfo.userId);
        currentSeatList.clear();
        currentSeatList.addAll(seatedList);
        return Visibility(
          visible: currentSeatList.isNotEmpty,
          child: Container(
            color: Colors.green,
            height: _calculateMicUpUserListHeight(),
            child: ListView.builder(
                shrinkWrap: true,
                physics: const ClampingScrollPhysics(),
                scrollDirection: Axis.vertical,
                itemCount: currentSeatList.length,
                itemBuilder: (context, index) {
                  return _buildMicUpUserItem(index);
                }),
          ),
        );
      },
    );
  }

  Widget _initSeparationWidget() {
    return ValueListenableBuilder(
      valueListenable: manager.seatState.seatApplicationList,
      builder: (context, seatApplicationList, child) {
        return ValueListenableBuilder(
          valueListenable: manager.seatState.seatList,
          builder: (context, seatList, child) {
            final seatedCount = seatList
                .where((seatInfo) =>
                    seatInfo.userId.isNotEmpty &&
                    seatInfo.userId != manager.userState.selfInfo.userId)
                .length;
            return Visibility(
              visible: seatedCount > 1 && seatApplicationList.isNotEmpty,
              child: Container(
                color: LiveColors.designStandardG3Divider,
                height: context.adapter.getHeight(7),
              ),
            );
          },
        );
      },
    );
  }

  Widget _initMicDownTitleWidget() {
    return ValueListenableBuilder(
      valueListenable: manager.seatState.seatApplicationList,
      builder: (context, seatApplicationList, child) {
        final filterApplications =
            List<SeatApplication>.from(seatApplicationList);
        filterApplications.removeWhere((application) =>
            application.userId == manager.userState.selfInfo.userId);
        return Visibility(
          visible: filterApplications.isNotEmpty,
          child: Text(
            "${LiveKitLocalizations.of(Global.appContext())!.live_application_list}"
            "(${filterApplications.length})",
            style: const TextStyle(
                color: LiveColors.designStandardG7, fontSize: 16),
          ),
        );
      },
    );
  }

  Widget _initMicDownUserListWidget() {
    return ValueListenableBuilder(
      valueListenable: manager.seatState.seatApplicationList,
      builder: (context, seatApplicationList, child) {
        final filterApplications =
            List<SeatApplication>.from(seatApplicationList);
        filterApplications.removeWhere((application) =>
            application.userId == manager.userState.selfInfo.userId);
        return Visibility(
          visible: filterApplications.isNotEmpty,
          child: SizedBox(
            height: _calculateMicDownUserListHeight(),
            child: ListView.builder(
                shrinkWrap: true,
                physics: const ClampingScrollPhysics(),
                scrollDirection: Axis.vertical,
                itemCount: filterApplications.length,
                itemBuilder: (context, index) {
                  return _buildMicDownUserItem(index);
                }),
          ),
        );
      },
    );
  }

  Widget _buildMicUpUserItem(int index) {
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
                        currentSeatList[index].avatarUrl,
                        fit: BoxFit.cover,
                        errorBuilder: (context, error, stackTrace) {
                          return Image.asset(
                            LiveImages.defaultAvatar,
                            package: Constants.pluginName,
                          );
                        },
                      ),
                    ),
                    Positioned(
                      bottom: 0,
                      right: 0,
                      child: Container(
                          width: context.adapter.getWidth(16),
                          height: context.adapter.getWidth(16),
                          decoration: BoxDecoration(
                              color: LiveColors.designStandardG1,
                              borderRadius: BorderRadius.circular(
                                  context.adapter.getWidth(8))),
                          child: Text('${currentSeatList[index].index + 1}',
                              style: TextStyle(
                                  color: LiveColors.designStandardFlowkitWhite
                                      .withAlpha(0xCC),
                                  fontSize: 12),
                              textAlign: TextAlign.center)),
                    )
                  ],
                ),
              ),
              SizedBox(width: context.adapter.getHeight(12)),
              Container(
                alignment: Alignment.centerLeft,
                constraints:
                    BoxConstraints(maxWidth: context.adapter.getWidth(135)),
                child: Text(
                  currentSeatList[index].userName.isNotEmpty
                      ? currentSeatList[index].userName
                      : currentSeatList[index].userId,
                  style: const TextStyle(
                      color: LiveColors.designStandardG6, fontSize: 16),
                  overflow: TextOverflow.ellipsis,
                ),
              ),
            ],
          ),
          GestureDetector(
            onTap: () {
              _hangUpLinkMic(currentSeatList[index].userId);
            },
            child: Container(
              width: context.adapter.getWidth(60),
              height: context.adapter.getHeight(24),
              decoration: BoxDecoration(
                border: Border.all(color: LiveColors.notStandardRed, width: 1),
                borderRadius:
                    BorderRadius.circular(context.adapter.getHeight(12)),
              ),
              alignment: Alignment.center,
              child: Text(
                LiveKitLocalizations.of(Global.appContext())!.live_hang_up,
                style: const TextStyle(
                    color: LiveColors.notStandardRed, fontSize: 12),
              ),
            ),
          )
        ],
      ),
    );
  }

  Widget _buildMicDownUserItem(int index) {
    final dataList = manager.seatState.seatApplicationList.value;
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
                child: ClipOval(
                  child: Image.network(
                    dataList[index].avatarUrl,
                    fit: BoxFit.cover,
                    errorBuilder: (context, error, stackTrace) {
                      return Image.asset(
                        LiveImages.defaultAvatar,
                        package: Constants.pluginName,
                      );
                    },
                  ),
                ),
              ),
              SizedBox(width: context.adapter.getWidth(10)),
              Container(
                alignment: Alignment.centerLeft,
                constraints:
                    BoxConstraints(maxWidth: context.adapter.getWidth(135)),
                child: Text(
                  dataList[index].userName.isNotEmpty
                      ? dataList[index].userName
                      : dataList[index].userId,
                  style: const TextStyle(
                      color: LiveColors.designStandardG7, fontSize: 16),
                  overflow: TextOverflow.ellipsis,
                ),
              ),
            ],
          ),
          Row(
            children: [
              GestureDetector(
                onTap: () {
                  _acceptLinkMicRequest(dataList[index].userId);
                },
                child: Container(
                  width: context.adapter.getWidth(60),
                  height: context.adapter.getHeight(24),
                  decoration: BoxDecoration(
                    color: LiveColors.designStandardB1,
                    borderRadius:
                        BorderRadius.circular(context.adapter.getHeight(12)),
                  ),
                  alignment: Alignment.center,
                  child: Text(
                    LiveKitLocalizations.of(Global.appContext())!.live_accept,
                    style: const TextStyle(
                        color: LiveColors.designStandardFlowkitWhite,
                        fontSize: 12),
                  ),
                ),
              ),
              8.horizontalSpace,
              GestureDetector(
                onTap: () {
                  _rejectLinkMicRequest(dataList[index].userId);
                },
                child: Container(
                  width: context.adapter.getWidth(60),
                  height: context.adapter.getHeight(24),
                  decoration: BoxDecoration(
                    border: Border.all(
                        color: LiveColors.designStandardB1,
                        width: context.adapter.getWidth(1)),
                    borderRadius:
                        BorderRadius.circular(context.adapter.getHeight(12)),
                  ),
                  alignment: Alignment.center,
                  child: Text(
                    LiveKitLocalizations.of(Global.appContext())!.live_reject,
                    style: TextStyle(
                        color: LiveColors.designStandardB1,
                        fontSize: context.adapter.getHeight(12)),
                  ),
                ),
              ),
            ],
          )
        ],
      ),
    );
  }
}

extension on _SeatManagementPanelWidgetState {
  void _showSeatInvitationPanel() {
    popupWidget(SeatInvitationPanelWidget(
        manager: manager,
        seatGridController: seatGridController,
        seatIndex: -1));
  }

  void _acceptLinkMicRequest(String userId) async {
    final result = await seatGridController.responseRemoteRequest(userId, true);
    if (result.code == TUIError.success) {
      return manager.fetchSeatApplicationList();
    }
    manager.toastSubject.add(ErrorHandler.convertToErrorMessage(
            result.code.value(), result.message) ??
        '');
  }

  void _rejectLinkMicRequest(String userId) async {
    final result =
        await seatGridController.responseRemoteRequest(userId, false);
    if (result.code == TUIError.success) {
      return manager.fetchSeatApplicationList();
    }
    final userInfo = TUIUserInfo(
        userId: userId,
        userName: '',
        avatarUrl: '',
        userRole: TUIRole.generalUser);
    manager.onSeatApplicationProcessed(userInfo);
    manager.toastSubject.add(ErrorHandler.convertToErrorMessage(
            result.code.value(), result.message) ??
        '');
  }

  void _hangUpLinkMic(String userId) async {
    final result = await seatGridController.kickUserOffSeatByAdmin(userId);
    if (result.code != TUIError.success) {
      return manager.toastSubject.add(ErrorHandler.convertToErrorMessage(
              result.code.value(), result.message) ??
          '');
    }
  }

  double _calculateMicUpUserListHeight() {
    double totalHeight = 0;
    if (manager.seatState.seatList.value.isNotEmpty) {
      final filterSeatList =
          List<SeatInfo>.from(manager.seatState.seatList.value);
      filterSeatList.removeWhere((seatInfo) =>
          seatInfo.userId.isEmpty ||
          seatInfo.userId == manager.userState.selfInfo.userId);
      totalHeight = (filterSeatList.length) * context.adapter.getHeight(60);
    }
    return totalHeight > context.adapter.getHeight(280)
        ? context.adapter.getHeight(280)
        : totalHeight;
  }

  double _calculateMicDownUserListHeight() {
    double totalHeight = 0;
    if (manager.seatState.seatApplicationList.value.isNotEmpty) {
      totalHeight = manager.seatState.seatApplicationList.value.length *
          context.adapter.getHeight(60);
    }
    return totalHeight > context.adapter.getHeight(280)
        ? context.adapter.getHeight(280)
        : totalHeight;
  }
}
