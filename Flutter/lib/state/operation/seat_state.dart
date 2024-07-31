import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';

class SeatState {
  bool filterEmptySeat = false;
  final ValueNotifier<List<SeatInfo>> seatList = ValueNotifier([]);
  final ValueNotifier<List<SeatApplication>> seatApplicationList = ValueNotifier([]);
  final ValueNotifier<String> mySeatApplicationId = ValueNotifier('');

  void reset() {
    seatList.value.clear();
    seatApplicationList.value.clear();
    mySeatApplicationId.value = '';
  }

  void setFilterEmptySeat(bool filterEmptySeat) {
    this.filterEmptySeat = filterEmptySeat;
  }

  String toString() {
    StringBuffer buffer = StringBuffer('{');
    buffer.write("object");
    buffer.write('filterEmptySeat:');
    buffer.write(filterEmptySeat);
    buffer.write(',');
    for (SeatInfo seatInfo in seatList.value) {
      buffer.write('[');
      buffer.write(seatInfo.index);
      buffer.write(',');
      buffer.write(seatInfo.userId.value);
      buffer.write(',');
      buffer.write(seatInfo.isLocked.value);
      buffer.write(',');
      buffer.write(seatInfo.isAudioLocked.value);
      buffer.write(']');
    }
    buffer.write('}');
    return buffer.toString();
  }

  void updateSeatList(List<TUISeatInfo> list) {
    List<SeatInfo> newList = [];
    for (TUISeatInfo info in list) {
      if (filterEmptySeat && info.userId.isEmpty) {
        continue;
      }
      SeatInfo seatInfo = SeatInfo();
      seatInfo.updateState(info);
      newList.add(seatInfo);
    }
    seatList.value = newList;
  }

  void initSeatApplicationList(List<TUIRequest> list) {
    seatApplicationList.value.clear();
    List<SeatApplication> newList = [];
    for (TUIRequest request in list) {
      SeatApplication application = SeatApplication(request.requestId);
      application.updateState(request);
      newList.add(application);
    }
    seatApplicationList.value = newList;
  }

  void addSeatApplication(TUIRequest request) {
    if (request.requestId.isEmpty) {
      return;
    }
    if (request.requestAction != TUIRequestAction.requestToTakeSeat) {
      return;
    }
    SeatApplication seatApplication = SeatApplication(request.requestId);
    seatApplication.updateState(request);
    seatApplicationList.value.add(seatApplication);
    final applicationList = <SeatApplication>[];
    applicationList.addAll(seatApplicationList.value);
    seatApplicationList.value = applicationList;
  }

  void removeSeatApplication(String id) {
    SeatApplication application = SeatApplication(id);
    seatApplicationList.value.remove(application);
    final applicationList = <SeatApplication>[];
    applicationList.addAll(seatApplicationList.value);
    seatApplicationList.value = applicationList;
  }

  void removeSeatInfo(SeatInfo seatInfo) {
    seatList.value.remove(seatInfo);
    final list = <SeatInfo>[];
    list.addAll(seatList.value);
    seatList.value = list;
  }
}

class SeatInfo {
  int index = 0;
  ValueNotifier<String> userId = ValueNotifier('');
  ValueNotifier<String?> name = ValueNotifier('');
  ValueNotifier<String?> avatarUrl = ValueNotifier('');
  ValueNotifier<bool?> isLocked = ValueNotifier(false);
  ValueNotifier<bool?> isAudioLocked = ValueNotifier(false);
  ValueNotifier<bool?> isVideoLocked = ValueNotifier(false);

  SeatInfo() {}

  SeatInfo.fromUserId(String userId) {
    this.userId.value = userId;
  }

  @override
  String toString() {
    return 'SeatInfo{index=$index, userId=${userId.value}, isLocked=${isLocked.value}, '
        'isAudioLocked=${isAudioLocked.value}, isVideoLocked=${isVideoLocked.value}}';
  }

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    return other is SeatInfo && other.userId.value == userId.value;
  }

  int get hashCode => userId.value.hashCode;

  void updateState(TUISeatInfo seatInfo) {
    index = seatInfo.index;
    userId.value = seatInfo.userId;
    if (seatInfo.userName != null && seatInfo.userName!.isNotEmpty) {
      name.value = seatInfo.userName;
    } else {
      name.value = userId.value;
    }
    avatarUrl.value = seatInfo.avatarUrl;
    isLocked.value = seatInfo.isLocked;
    isAudioLocked.value = seatInfo.isAudioLocked;
  }

  void updateNewState(SeatInfo newSeatInfo) {
    index = newSeatInfo.index;
    userId.value = newSeatInfo.userId.value;
    if (newSeatInfo.name.value != null && newSeatInfo.name.value!.isNotEmpty) {
      name.value = newSeatInfo.name.value;
    } else {
      name.value = userId.value;
    }
    avatarUrl.value = newSeatInfo.avatarUrl.value;
    isLocked.value = newSeatInfo.isLocked.value;
    isAudioLocked.value = newSeatInfo.isAudioLocked.value;
  }
}

class SeatApplication {
  String id = '';
  String userId = '';
  String? userName = '';
  String? avatarUrl = '';
  int timestamp = 0;

  SeatApplication(this.id);

  void updateState(TUIRequest request) {
    userName = request.userName;
    avatarUrl = request.avatarUrl;
    userId = request.userId;
    timestamp = request.timestamp;
  }

  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    return other is SeatApplication && other.id == id;
  }

  @override
  int get hashCode => id.hashCode;

  @override
  String toString() {
    return 'SeatApplication{id=\'$id\', userId=\'$userId\', timestamp=$timestamp}';
  }
}
