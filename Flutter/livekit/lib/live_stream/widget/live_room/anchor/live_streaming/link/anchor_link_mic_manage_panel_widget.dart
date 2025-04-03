import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

import '../../../../../state/index.dart';

class AnchorLinkMicManagePanelWidget extends BasicWidget {
  const AnchorLinkMicManagePanelWidget({super.key, required super.liveController});

  @override
  AnchorLinkMicManagePanelWidgetState getState() {
    return AnchorLinkMicManagePanelWidgetState();
  }
}

class AnchorLinkMicManagePanelWidgetState extends BasicState<AnchorLinkMicManagePanelWidget> {
  final currentSeatList = <SeatInfo>[];

  @override
  Widget build(BuildContext context) {
    return Container(
      width: screenWidth,
      height: 718,
      padding: const EdgeInsets.only(bottom: 20),
      decoration: const BoxDecoration(
        color: LiveColors.designStandardG2,
        borderRadius: BorderRadius.only(topLeft: Radius.circular(20), topRight: Radius.circular(20)),
      ),
      child: SingleChildScrollView(
        child:
            Column(mainAxisAlignment: MainAxisAlignment.start, crossAxisAlignment: CrossAxisAlignment.start, children: [
          _initTitleWidget(),
          _initMicUpTitleWidget(),
          _initMicUpUserListWidget(),
          _initSeparationWidget(),
          _initMicDownTitleWidget(),
          _initMicDownUserListWidget(),
        ]),
      ),
    );
  }

  _initTitleWidget() {
    return SizedBox(
      height: 44,
      width: screenWidth,
      child: Stack(
        children: [
          Positioned(
            left: 14,
            child: GestureDetector(
              onTap: () {
                Navigator.pop(context);
              },
              child: Container(
                width: 44,
                height: 44,
                padding: const EdgeInsets.all(10),
                child: Image.asset(
                  LiveImages.returnArrow,
                  package: Constants.pluginName,
                ),
              ),
            ),
          ),
          Center(
            child: Text(
              LiveKitLocalizations.of(Global.appContext())!.live_link_mic_manager,
              style: const TextStyle(color: LiveColors.designStandardG7, fontSize: 16),
            ),
          ),
        ],
      ),
    );
  }

  _initMicUpTitleWidget() {
    return ValueListenableBuilder(
      valueListenable: liveController.getSeatState().seatList,
      builder: (BuildContext context, List<SeatInfo> value, Widget? child) {
        return Visibility(
          visible: liveController.getSeatState().seatList.value.length > 1,
          child: Container(
            margin: const EdgeInsets.only(left: 24, top: 20),
            child: Text(
              "${LiveKitLocalizations.of(Global.appContext())!.live_link_mic_up_title}"
              "(${liveController.getSeatState().seatList.value.length - 1}/8)",
              style: const TextStyle(color: LiveColors.designStandardG7, fontSize: 12),
            ),
          ),
        );
      },
    );
  }

  _initMicUpUserListWidget() {
    return ValueListenableBuilder(
      valueListenable: liveController.getSeatState().seatList,
      builder: (BuildContext context, List<SeatInfo> value, Widget? child) {
        currentSeatList.clear();
        currentSeatList.addAll(liveController.getSeatState().seatList.value);
        currentSeatList.remove(SeatInfo.fromUserId(liveController.getUserState().selfInfo.userId));
        return Visibility(
          visible: currentSeatList.isNotEmpty,
          child: SizedBox(
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

  _initSeparationWidget() {
    return ValueListenableBuilder(
      valueListenable: liveController.getSeatState().seatApplicationList,
      builder: (BuildContext context, List<SeatApplication> value, Widget? child) {
        return ValueListenableBuilder(
          valueListenable: liveController.getSeatState().seatList,
          builder: (BuildContext context, List<SeatInfo> value, Widget? child) {
            return Visibility(
              visible: liveController.getSeatState().seatList.value.length > 1 &&
                  liveController.getSeatState().seatApplicationList.value.isNotEmpty,
              child: Container(
                color: LiveColors.designStandardG3Divider,
                height: 7,
              ),
            );
          },
        );
      },
    );
  }

  _initMicDownTitleWidget() {
    return ValueListenableBuilder(
      valueListenable: liveController.getSeatState().seatApplicationList,
      builder: (BuildContext context, List<SeatApplication> value, Widget? child) {
        return Visibility(
          visible: liveController.getSeatState().seatApplicationList.value.isNotEmpty,
          child: Container(
            margin: const EdgeInsets.only(left: 24, top: 20),
            child: Text(
              "${LiveKitLocalizations.of(Global.appContext())!.live_link_mic_down_title}"
              "(${liveController.getSeatState().seatApplicationList.value.length})",
              style: const TextStyle(color: LiveColors.designStandardG7, fontSize: 12),
            ),
          ),
        );
      },
    );
  }

  _initMicDownUserListWidget() {
    return ValueListenableBuilder(
      valueListenable: liveController.getSeatState().seatApplicationList,
      builder: (BuildContext context, List<SeatApplication> value, Widget? child) {
        return Visibility(
          visible: liveController.getSeatState().seatApplicationList.value.isNotEmpty,
          child: SizedBox(
            height: _calculateMicDownUserListHeight(),
            child: ListView.builder(
                shrinkWrap: true,
                physics: const ClampingScrollPhysics(),
                scrollDirection: Axis.vertical,
                itemCount: liveController.getSeatState().seatApplicationList.value.length,
                itemBuilder: (context, index) {
                  return _buildMicDownUserItem(index);
                }),
          ),
        );
      },
    );
  }

  _buildMicUpUserItem(int index) {
    return Container(
      height: 60,
      color: LiveColors.designStandardG2,
      padding: const EdgeInsets.only(left: 24, right: 24),
      child: Row(
        mainAxisAlignment: MainAxisAlignment.spaceBetween,
        crossAxisAlignment: CrossAxisAlignment.center,
        mainAxisSize: MainAxisSize.max,
        children: [
          Row(
            children: [
              SizedBox(
                width: 40,
                height: 40,
                child: ClipOval(
                  child: Image.network(
                    currentSeatList[index].avatarUrl.value ?? "",
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
              10.horizontalSpace,
              Container(
                width: 100,
                alignment: Alignment.centerLeft,
                child: Text(
                  currentSeatList[index].name.value != null && currentSeatList[index].name.value!.isNotEmpty
                      ? currentSeatList[index].name.value!
                      : currentSeatList[index].userId.value,
                  style: const TextStyle(color: LiveColors.designStandardG7, fontSize: 16),
                  overflow: TextOverflow.ellipsis,
                ),
              ),
            ],
          ),
          GestureDetector(
            onTap: () {
              _hangUpLinkMic(currentSeatList[index]);
            },
            child: Container(
              width: 64,
              height: 24,
              decoration: BoxDecoration(
                border: Border.all(color: LiveColors.notStandardRed, width: 1),
                borderRadius: BorderRadius.circular(12),
              ),
              alignment: Alignment.center,
              child: Text(
                LiveKitLocalizations.of(Global.appContext())!.live_hang_up,
                style: const TextStyle(color: LiveColors.notStandardRed, fontSize: 12),
              ),
            ),
          )
        ],
      ),
    );
  }

  _buildMicDownUserItem(int index) {
    final dataList = liveController.getSeatState().seatApplicationList.value;
    return Container(
      height: 60,
      color: LiveColors.designStandardG2,
      padding: const EdgeInsets.only(left: 24, right: 24),
      child: Row(
        mainAxisAlignment: MainAxisAlignment.spaceBetween,
        crossAxisAlignment: CrossAxisAlignment.center,
        mainAxisSize: MainAxisSize.max,
        children: [
          Row(
            children: [
              SizedBox(
                width: 40,
                height: 40,
                child: ClipOval(
                  child: Image.network(
                    dataList[index].avatarUrl ?? "",
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
              10.horizontalSpace,
              Container(
                width: 100,
                alignment: Alignment.centerLeft,
                child: Text(
                  dataList[index].userName != null && dataList[index].userName!.isNotEmpty
                      ? dataList[index].userName!
                      : dataList[index].userId,
                  style: const TextStyle(color: LiveColors.designStandardG7, fontSize: 16),
                  overflow: TextOverflow.ellipsis,
                ),
              ),
            ],
          ),
          Row(
            children: [
              GestureDetector(
                onTap: () {
                  _acceptLinkMicRequest(dataList[index].id);
                },
                child: Container(
                  width: 64,
                  height: 24,
                  decoration: BoxDecoration(
                    color: LiveColors.designStandardB1,
                    borderRadius: BorderRadius.circular(12),
                  ),
                  alignment: Alignment.center,
                  child: Text(
                    LiveKitLocalizations.of(Global.appContext())!.live_accept,
                    style: const TextStyle(color: LiveColors.designStandardFlowkitWhite, fontSize: 12),
                  ),
                ),
              ),
              8.horizontalSpace,
              GestureDetector(
                onTap: () {
                  _rejectLinkMicRequest(dataList[index].id);
                },
                child: Container(
                  width: 64,
                  height: 24,
                  decoration: BoxDecoration(
                    border: Border.all(color: LiveColors.designStandardB1, width: 1),
                    borderRadius: BorderRadius.circular(12),
                  ),
                  alignment: Alignment.center,
                  child: Text(
                    LiveKitLocalizations.of(Global.appContext())!.live_reject,
                    style: const TextStyle(color: LiveColors.designStandardB1, fontSize: 12),
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

extension AnchorLinkMicManagePanelWidgetStateLogicExtension on AnchorLinkMicManagePanelWidgetState {
  void _rejectLinkMicRequest(String requestId) {
    liveController.seatController.rejectRequest(requestId);
  }

  void _acceptLinkMicRequest(String requestId) {
    liveController.seatController.acceptRequest(requestId);
  }

  void _hangUpLinkMic(SeatInfo seatInfo) {
    liveController.seatController.kickUserOffSeatByAdmin(0, seatInfo);
  }

  double _calculateMicUpUserListHeight() {
    double totalHeight = 0;
    if (liveController.getSeatState().seatList.value.isNotEmpty) {
      totalHeight = (liveController.getSeatState().seatList.value.length - 1) * 60;
    }
    return totalHeight > 280 ? 280 : totalHeight;
  }

  double _calculateMicDownUserListHeight() {
    double totalHeight = 0;
    if (liveController.getSeatState().seatApplicationList.value.isNotEmpty) {
      totalHeight = liveController.getSeatState().seatApplicationList.value.length * 60;
    }
    return totalHeight > 280 ? 280 : totalHeight;
  }
}
