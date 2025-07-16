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
  late final cellWidth = (1.screenWidth - 39.width) * 0.5;
  late final cellHeight = cellWidth / _childAspectRatio;
  final int _column = 2;
  final double _childAspectRatio = 168.width / 262.height;
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
        height: 1.screenHeight,
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
                  left: index % _column == 0 ? 16.width : 3.5.width,
                  right: index % _column == 1 ? 16.width : 3.5.width,
                  top: 8.height,
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
        borderRadius: BorderRadius.all(Radius.circular(12.radius)),
        child: SizedBox(
          width: cellWidth,
          child: Stack(
            children: [
              Positioned(
                child: Container(
                  width: cellWidth,
                  padding: const EdgeInsets.all(0),
                  child: Image.network(
                    item.coverUrl.split(';').first,
                    fit: BoxFit.fill,
                    errorBuilder: (context, error, stackTrace) {
                      return Image.network(
                        fit: BoxFit.fill,
                        Constants.defaultCoverUrl,
                      );
                    },
                  ),
                ),
              ),
              Positioned(
                left: 8.width,
                top: 6.height,
                child: Row(
                  children: [
                    SizedBox(
                      height: 8.radius,
                      width: 8.radius,
                      child: Image.asset(
                        fit: BoxFit.fill,
                        LiveImages.roomListItemLiveStatus,
                        package: Constants.pluginName,
                      ),
                    ),
                    SizedBox(width: 5.width),
                    Text(
                      LiveKitLocalizations.of(Global.appContext())!
                          .livelist_viewed_audience_count
                          .replaceAll('xxx', "${item.viewCount}"),
                      style: const TextStyle(
                          color: LiveColors.designStandardFlowkitWhite,
                          fontSize: 14,
                          fontWeight: FontWeight.w600),
                    )
                  ],
                ),
              ),
              Positioned(
                left: 8.width,
                bottom: 32.height,
                right: 8.width,
                child: Container(
                  constraints:
                      BoxConstraints(maxHeight: 22.height, maxWidth: 152.width),
                  child: Text(
                    item.roomInfo.name ?? "",
                    overflow: TextOverflow.ellipsis,
                    style: TextStyle(
                        color: LiveColors.designStandardFlowkitWhite
                            .withAlpha(0xE6),
                        fontSize: 16,
                        fontWeight: FontWeight.w600),
                  ),
                ),
              ),
              Positioned(
                left: 8.width,
                bottom: 10.height,
                right: 8.width,
                child: Row(
                  children: [
                    ClipRRect(
                      borderRadius: BorderRadius.all(Radius.circular(8.radius)),
                      child: SizedBox(
                        height: 16.radius,
                        width: 16.radius,
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
                    SizedBox(width: 4.width),
                    Container(
                      constraints: BoxConstraints(
                          maxWidth: 132.width, maxHeight: 20.height),
                      child: Text(
                        item.roomInfo.ownerName?.isNotEmpty ?? false
                            ? item.roomInfo.ownerName ?? ''
                            : item.roomInfo.ownerId,
                        overflow: TextOverflow.ellipsis,
                        style: TextStyle(
                            color: LiveColors.designStandardFlowkitWhite
                                .withAlpha(0x8C),
                            fontSize: 12),
                      ),
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
                    padding: EdgeInsets.all(16.radius),
                    alignment: Alignment.center,
                    child: const CircularProgressIndicator(),
                  )
                : SizedBox(height: 12.height),
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
                    padding: EdgeInsets.all(16.radius),
                    alignment: Alignment.center,
                    child: Text(LiveKitLocalizations.of(Global.appContext())!
                        .livelist_no_more_data),
                  )
                : const SizedBox.shrink(),
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
