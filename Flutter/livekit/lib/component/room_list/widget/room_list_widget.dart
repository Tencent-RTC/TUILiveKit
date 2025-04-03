import 'package:flutter/material.dart';

import '../../../common/index.dart';
import '../../../tencent_live_uikit.dart';
import '../service/room_list_service.dart';
import '../store/room_list_state.dart';

class RoomListWidget extends StatefulWidget {
  const RoomListWidget({super.key});

  @override
  RoomListWidgetState createState() {
    return RoomListWidgetState();
  }
}

class RoomListWidgetState extends State<RoomListWidget> {
  late final double screenWidth = MediaQuery.of(context).size.width;
  late final double screenHeight = MediaQuery.of(context).size.height;
  late final cellWidth = (screenWidth - 40) * 0.5;
  late final cellHeight = cellWidth / _childAspectRatio;
  final int _column = 2;
  final double _childAspectRatio = 169.0 / 262.0;
  late final RoomListService _roomListService = RoomListService();
  late final RoomListState _roomListState = _roomListService.roomListState;
  final ScrollController _scrollController = ScrollController();
  late final VoidCallback _listener = _onLoginChange;

  @override
  void initState() {
    super.initState();
    _initData();
    _addListener();
  }

  @override
  void dispose() {
    _scrollController.removeListener(_scrollListener);
    _scrollController.dispose();
    _removeListener();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return RefreshIndicator(
      onRefresh: _onRefresh,
      child: SizedBox(
        height: screenHeight,
        child: CustomScrollView(
          controller: _scrollController,
          physics: const AlwaysScrollableScrollPhysics(),
          slivers: [
            _buildNoDataWidget(),
            _buildSliverGridWidget(),
            _buildBottomWidget(),
          ],
        ),
      ),
    );
  }

  void _initData() {
    _scrollController.addListener(_scrollListener);
    if (Boot().isLogin.value) {
      _onRefresh();
    }
  }

  void _addListener() {
    Boot().isLogin.addListener(_listener);
  }

  void _removeListener() {
    Boot().isLogin.removeListener(_listener);
  }

  void _onLoginChange() {
    if (Boot().isLogin.value) {
      _onRefresh();
    }
  }
}

extension RoomListWidgetStateLogicExtension on RoomListWidgetState {
  void _scrollListener() {
    if (_scrollController.position.pixels ==
        _scrollController.position.maxScrollExtent) {
      _roomListService.loadMoreData();
    }
  }

  Future<void> _onRefresh() async {
    await _roomListService.refreshFetchList();
  }

  Widget _buildSliverGridWidget() {
    return ValueListenableBuilder(
      valueListenable: _roomListState.liveInfoList,
      builder: (BuildContext context, value, Widget? child) {
        return SliverGrid(
          gridDelegate: SliverGridDelegateWithFixedCrossAxisCount(
            crossAxisCount: 2,
            childAspectRatio: _childAspectRatio,
          ),
          delegate: SliverChildBuilderDelegate(
            (BuildContext context, int index) {
              return Padding(
                padding: EdgeInsets.only(
                  left: index % _column == 0 ? 16 : 4,
                  right: index % _column == 1 ? 16 : 4,
                  top: 8,
                ),
                child: _buildItemWidget(index),
              );
            },
            childCount: _roomListState.liveInfoList.value.length,
          ),
        );
      },
    );
  }

  Widget _buildItemWidget(int index) {
    final item = _roomListState.liveInfoList.value[index];
    return GestureDetector(
      onTap: () {
        _clickItem(index);
      },
      child: ClipRRect(
        borderRadius: const BorderRadius.all(Radius.circular(8)),
        child: Container(
          width: cellWidth,
          color: LiveColors.designStandardFlowkitWhite,
          child: Stack(
            children: [
              Positioned(
                child: Container(
                  height: cellHeight - 40,
                  width: cellWidth,
                  padding: const EdgeInsets.all(0),
                  child: Image.network(
                    item.coverUrl.split(';').first,
                    fit: BoxFit.fill,
                    errorBuilder: (context, error, stackTrace) {
                      return Image.asset(
                        fit: BoxFit.fill,
                        LiveImages.streamDefaultCover,
                        package: Constants.pluginName,
                      );
                    },
                  ),
                ),
              ),
              Positioned(
                left: 2,
                top: 16,
                child: Row(
                  children: [
                    Container(
                      height: 20,
                      width: 20,
                      padding: const EdgeInsets.all(6),
                      child: Image.asset(
                        fit: BoxFit.fill,
                        LiveImages.roomListItemLiveStatus,
                        package: Constants.pluginName,
                      ),
                    ),
                    Text(
                      LiveKitLocalizations.of(Global.appContext())!
                          .live_audience_count_in_room
                          .replaceAll('%d', "${item.viewCount}"),
                      style: const TextStyle(
                          color: LiveColors.designStandardFlowkitWhite,
                          fontSize: 12),
                    )
                  ],
                ),
              ),
              Positioned(
                left: 8,
                bottom: 32,
                child: Text(
                  item.roomInfo.name ?? "",
                  style: const TextStyle(
                      color: LiveColors.designStandardG2, fontSize: 14),
                ),
              ),
              Positioned(
                left: 8,
                bottom: 8,
                child: Row(
                  children: [
                    ClipRRect(
                      borderRadius: const BorderRadius.all(Radius.circular(10)),
                      child: SizedBox(
                        height: 20,
                        width: 20,
                        child: Image.network(
                          item.roomInfo.ownerAvatarUrl ?? "",
                          fit: BoxFit.fill,
                          errorBuilder: (context, error, stackTrace) {
                            return Image.asset(
                              fit: BoxFit.fill,
                              LiveImages.defaultAvatar,
                              package: Constants.pluginName,
                            );
                          },
                        ),
                      ),
                    ),
                    4.horizontalSpace,
                    Text(
                      item.roomInfo.ownerName ?? item.roomInfo.ownerId,
                      style: const TextStyle(
                          color: LiveColors.designStandardG3, fontSize: 12),
                    )
                  ],
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }

  Widget _buildBottomWidget() {
    return ValueListenableBuilder(
        valueListenable: _roomListState.loadStatus,
        builder: (BuildContext context, value, Widget? child) {
          return SliverToBoxAdapter(
            child: (_roomListState.isHaveMoreData.value &&
                    _roomListState.loadStatus.value)
                ? Container(
                    padding: const EdgeInsets.all(16.0),
                    alignment: Alignment.center,
                    child: const CircularProgressIndicator(),
                  )
                : 12.verticalSpace,
          );
        });
  }

  Widget _buildNoDataWidget() {
    return ValueListenableBuilder(
        valueListenable: _roomListState.refreshStatus,
        builder: (BuildContext context, value, Widget? child) {
          final isShow = _roomListState.liveInfoList.value.isEmpty &&
              !_roomListState.refreshStatus.value;
          return SliverToBoxAdapter(
            child: isShow
                ? Container(
                    padding: const EdgeInsets.all(16.0),
                    alignment: Alignment.center,
                    child: Text(LiveKitLocalizations.of(Global.appContext())!
                        .live_no_room_tip),
                  )
                : 0.verticalSpace,
          );
        });
  }

  void _clickItem(int index) {
    final liveInfo = _roomListState.liveInfoList.value[index];
    final roomType =
        LiveIdentityGenerator.instance.getIDType(liveInfo.roomInfo.roomId);
    if (roomType == RoomType.voice) {
      TUILiveKitNavigatorObserver.instance.enterVoiceRoomAudiencePage(liveInfo);
    } else {
      TUILiveKitNavigatorObserver.instance.enterLiveRoomAudiencePage(liveInfo);
    }
  }
}
