import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_live_uikit/common/logger/logger.dart';
import 'package:tencent_live_uikit/common/ui_component/room_list/store/room_list_state.dart';
import 'package:tencent_live_uikit/manager/error/error_handler.dart';

class RoomListService {
  static const String tag = 'RoomListService';
  late final int fetchListCount = 20;
  late final TUILiveListManager _liveListManager = TUIRoomEngine.sharedInstance().getExtension(TUIExtensionType.liveListManger);
  late final RoomListState roomListState = RoomListState();

  RoomListService();

  Future<void> refreshFetchList() async {
    if (roomListState.refreshStatus.value) {
      return;
    }
    roomListState.refreshStatus.value = true;
    roomListState.cursor = "";
    await _fetchLiveList();
  }

  Future<void> loadMoreData() async {
    if (roomListState.loadStatus.value || roomListState.refreshStatus.value || !roomListState.isHaveMoreData.value) {
      return;
    }
    roomListState.loadStatus.value = true;
    _loadMoreData();
  }
}

extension RoomListServiceLogicExtension on RoomListService {
  Future<void> _fetchLiveList() async {
    final String cursor = roomListState.cursor;
    TUIValueCallBack<TUILiveListResult> result = await _liveListManager.fetchLiveList(cursor, fetchListCount);
    if (result.code != TUIError.success) {
      LiveKitLogger.error("${RoomListService.tag} _initData [code:${result.code},message:${result.message}]");
      ErrorHandler.onError(result.code);
      roomListState.loadStatus.value = false;
      roomListState.refreshStatus.value = false;
      roomListState.isHaveMoreData.value = false;
    } else {
      final liveListResult = result.data as TUILiveListResult;
      roomListState.liveInfoList.value = liveListResult.liveInfoList;
      roomListState.cursor = liveListResult.cursor;
      roomListState.loadStatus.value = false;
      roomListState.refreshStatus.value = false;
      roomListState.isHaveMoreData.value = liveListResult.cursor.isNotEmpty;
    }
  }

  Future<void> _loadMoreData() async {
    final String cursor = roomListState.cursor;
    TUIValueCallBack<TUILiveListResult> result = await _liveListManager.fetchLiveList(cursor, fetchListCount);
    if (result.code != TUIError.success) {
      LiveKitLogger.error("${RoomListService.tag} _initData [code:${result.code},message:${result.message}]");
      ErrorHandler.onError(result.code);
      roomListState.loadStatus.value = false;
      roomListState.isHaveMoreData.value = false;
    } else {
      final liveListResult = result.data as TUILiveListResult;
      List<TUILiveInfo> liveInfoList = [...roomListState.liveInfoList.value, ...liveListResult.liveInfoList];
      roomListState.liveInfoList.value = liveInfoList;
      roomListState.cursor = liveListResult.cursor;
      roomListState.loadStatus.value = false;
      roomListState.isHaveMoreData.value = liveListResult.cursor.isNotEmpty;
    }
  }
}
