import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';
import 'package:live_uikit_gift/common/screen/screen_adapter.dart';
import 'package:rtc_room_engine/api/extension/tui_live_gift_manager.dart';

import '../../../common/index.dart';
import '../../../state/index.dart';
import 'gift_list_controller.dart';

class GiftListWidget extends StatefulWidget {
  final GiftListController giftListController;

  const GiftListWidget({super.key, required this.giftListController});

  @override
  GiftListWidgetState createState() => GiftListWidgetState();
}

class GiftListWidgetState extends State<GiftListWidget> {
  List<PageWidget> pages = [];

  @override
  void initState() {
    super.initState();
    widget.giftListController.pageController = PageController();
    _initWidget();
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      height: context.adapter.getHeight(362),
      width: MediaQuery
          .sizeOf(context)
          .width,
      padding: EdgeInsets.only(
          left: context.adapter.getWidth(10),
          right: context.adapter.getWidth(10)),
      decoration: BoxDecoration(
          color: GiftColors.bgOperateColor,
          borderRadius: BorderRadius.only(
              topLeft: Radius.circular(context.adapter.getWidth(20)),
              topRight: Radius.circular(context.adapter.getWidth(20)))),
      child: Column(
        children: [
          SizedBox(height: context.adapter.getHeight(20)),
          Text(GiftLocalizations.of(context)!.gift_text,
              style: const TextStyle(
                  fontSize: 16,
                  fontWeight: FontWeight.w700,
                  color: GiftColors.designG7)),
          SizedBox(height: context.adapter.getHeight(20)),
          SizedBox(
            height: context.adapter.getHeight(258),
            width: MediaQuery
                .sizeOf(context)
                .width,
            child: PageView(
              controller: widget.giftListController.pageController,
              children: pages,
            ),
          ),
        ],
      ),
    );
  }

  @override
  void dispose() {
    widget.giftListController.pageController.dispose();
    super.dispose();
  }

  void _initWidget() {
    const int pagerCount = 8;
    final giftList =
    TUIGiftStore().giftListMap.value[widget.giftListController.roomId];
    if (giftList == null) {
      return;
    }
    int listLength = giftList.length;
    for (int i = 0; i < (listLength / pagerCount).ceil(); i++) {
      List<TUIGiftInfo> dataList = giftList.sublist(
          i * pagerCount,
          ((i + 1) * pagerCount < listLength)
              ? ((i + 1) * pagerCount)
              : listLength);
      pages.add(PageWidget(
        giftListController: widget.giftListController,
        dataList: dataList,
      ));
    }
  }
}

class PageWidget extends StatefulWidget {
  final GiftListController giftListController;
  final List<TUIGiftInfo> dataList;

  const PageWidget({
    super.key,
    required this.giftListController,
    required this.dataList,
  });

  @override
  PageWidgetState createState() => PageWidgetState();
}

class PageWidgetState extends State<PageWidget> {
  int _selectIndex = 0;
  DateTime? _lastClickTime;
  final int clickInterval = 1;

  @override
  Widget build(BuildContext context) {
    return GridView.builder(
      physics: const NeverScrollableScrollPhysics(),
      itemCount: widget.dataList.length,
      gridDelegate: const SliverGridDelegateWithFixedCrossAxisCount(
          crossAxisCount: 4, childAspectRatio: 0.67, crossAxisSpacing: 10),
      itemBuilder: (context, index) {
        final TUIGiftInfo giftInfo = widget.dataList[index];
        return GestureDetector(
          onTap: () {
            setState(() {
              _selectIndex = index;
            });
          },
          child: SizedBox(
            width: context.adapter.getWidth(80),
            height: context.adapter.getHeight(114),
            child: Column(
              mainAxisAlignment: MainAxisAlignment.start,
              crossAxisAlignment: CrossAxisAlignment.center,
              children: [
                Stack(
                  children: [
                    Container(
                        decoration: BoxDecoration(
                            color: _selectIndex == index
                                ? GiftColors.buttonPrimaryDefaultColor
                                : GiftColors.bgOperateColor,
                            borderRadius: BorderRadius.all(
                                Radius.circular(context.adapter.getWidth(10)))),
                        width: context.adapter.getWidth(80),
                        height: context.adapter.getHeight(100)),
                    Positioned(
                        top: 2,
                        left: 2,
                        child: Container(
                            width: context.adapter.getWidth(76),
                            height: context.adapter.getHeight(74),
                            decoration: BoxDecoration(
                                color: _selectIndex == index
                                    ? GiftColors.bgColorEntryCard
                                    : GiftColors.bgOperateColor,
                                borderRadius: BorderRadius.all(Radius.circular(
                                    context.adapter.getWidth(10)))))),
                    Positioned(
                      top: context.adapter.getHeight(10),
                      left: context.adapter.getWidth(10),
                      child: Container(
                        width: context.adapter.getWidth(60),
                        height: context.adapter.getWidth(60),
                        color: _selectIndex == index
                            ? GiftColors.bgColorEntryCard
                            : GiftColors.bgOperateColor,
                        child: CachedNetworkImage(
                          fit: BoxFit.fitWidth,
                          imageUrl: giftInfo.iconUrl,
                          placeholder: (context, url) => Container(),
                          errorWidget: (context, url, error) => Container(),
                        ),
                      ),
                    ),
                    Positioned(
                        top: context.adapter.getHeight(72),
                        child: Align(
                            alignment: Alignment.topCenter,
                            child: SizedBox(
                              width: context.adapter.getWidth(80),
                              height: context.adapter.getHeight(30),
                              child: _selectIndex == index
                                  ? TextButton(
                                  child: Text(
                                    GiftLocalizations.of(context)!
                                        .gift_send,
                                    style: const TextStyle(
                                        color: Colors.white, fontSize: 12),
                                  ),
                                  onPressed: () {
                                    _handleSendGift(
                                        giftInfo, clickInterval);
                                  })
                                  : Text(
                                giftInfo.name,
                                overflow: TextOverflow.ellipsis,
                                textAlign: TextAlign.center,
                                style: const TextStyle(
                                    color: Colors.white, fontSize: 12),
                              ),
                            )))
                  ],
                ),
                SizedBox(height: context.adapter.getHeight(3)),
                Text(
                  "${giftInfo.coins}",
                  style: const TextStyle(color: Colors.white54, fontSize: 10),
                ),
              ],
            ),
          ),
        );
      },
    );
  }

  void _handleSendGift(TUIGiftInfo giftInfo, intervalInSeconds) {
    if (_isEnableSinceLastClick(intervalInSeconds)) {
      _sendGift(giftInfo);
    }
  }

  bool _isEnableSinceLastClick(int intervalInSeconds) {
    final now = DateTime.now();
    if (_lastClickTime == null ||
        now.difference(_lastClickTime!) >
            Duration(seconds: intervalInSeconds)) {
      _lastClickTime = now;
      return true;
    }
    return false;
  }

  void _sendGift(TUIGiftInfo giftInfo) {
    const giftCount = 1;
    widget.giftListController.sendGift(giftInfo, giftCount);
  }
}
