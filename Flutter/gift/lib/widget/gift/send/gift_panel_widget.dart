import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';
import 'package:gift/common/index.dart';
import 'package:gift/state/index.dart';
import 'package:gift/widget/gift/send/gift_send_controller.dart';

class GiftPanelWidget extends StatefulWidget {
  final GiftSendController controller;

  const GiftPanelWidget({super.key, required this.controller});

  @override
  GiftPanelWidgetState createState() => GiftPanelWidgetState();
}

class GiftPanelWidgetState extends State<GiftPanelWidget> {
  List<PageWidget> pages = [];

  @override
  void initState() {
    super.initState();
    widget.controller.pageController = PageController();
    _initWidget();
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      height: 320,
      width: MediaQuery
          .of(context)
          .size
          .width,
      padding: const EdgeInsets.only(left: 10, right: 10),
      color: GiftColors.giftG2,
      child: Column(
        children: [
          const SizedBox(height: 8),
          Text(GiftLocalizations.of(context)!.gift_text,
              style: const TextStyle(fontSize: 16, fontWeight: FontWeight.w700, color: GiftColors.giftDesignG7)),
          const SizedBox(height: 5),
          SizedBox(
            height: 270,
            width: MediaQuery
                .of(context)
                .size
                .width,
            child: PageView(
              controller: widget.controller.pageController,
              children: pages,
            ),
          ),
        ],
      ),
    );
  }

  @override
  void dispose() {
    widget.controller.pageController.dispose();
    super.dispose();
  }

  void _initWidget() {
    const int pagerCount = 8;
    int listLength = GiftStore().giftModelList.length;
    for (int i = 0; i < (listLength / pagerCount).ceil(); i++) {
      List<GiftModel> dataList = GiftStore()
          .giftModelList
          .sublist(i * pagerCount, ((i + 1) * pagerCount < listLength) ? ((i + 1) * pagerCount) : listLength);
      pages.add(PageWidget(dataList: dataList));
    }
  }
}

class PageWidget extends StatefulWidget {
  const PageWidget({
    super.key,
    required this.dataList,
  });

  final List<GiftModel> dataList;

  @override
  PageWidgetState createState() => PageWidgetState();
}

class PageWidgetState extends State<PageWidget> {
  int _selectIndex = -1;
  DateTime? _lastClickTime;
  final int clickInterval = 1;

  @override
  Widget build(BuildContext context) {
    return GridView.builder(
      itemCount: widget.dataList.length,
      gridDelegate: const SliverGridDelegateWithFixedCrossAxisCount(
          crossAxisCount: 4, childAspectRatio: 0.7, crossAxisSpacing: 10),
      itemBuilder: (BuildContext context, int index) {
        final GiftModel model = widget.dataList[index];
        return GestureDetector(
          onTap: () {
            setState(() {
              _selectIndex = index;
            });
          },
          child: Column(
            mainAxisAlignment: MainAxisAlignment.start,
            crossAxisAlignment: CrossAxisAlignment.center,
            children: [
              Container(
                width: 74,
                height: 74,
                constraints: const BoxConstraints(maxHeight: 84),
                decoration: BoxDecoration(
                  color: GiftColors.giftDesignG3,
                  border: Border.all(
                      color: _selectIndex == index ? GiftColors.giftDesignB1 : GiftColors.giftDesignG3, width: 2.0),
                  borderRadius: BorderRadius.circular(10.0),
                ),
                child: CachedNetworkImage(
                  fit: BoxFit.fitWidth,
                  imageUrl: model.imageUrl ?? "",
                  placeholder: (context, url) => Container(),
                  errorWidget: (context, url, error) => Container(),
                ),
              ),
              const SizedBox(height: 3),
              Stack(
                children: _selectIndex == index
                    ? [
                  SizedBox(
                    width: 74,
                    height: 30,
                    child: ElevatedButton(
                        style: ButtonStyle(
                          padding: WidgetStateProperty.all(
                            const EdgeInsets.all(0),
                          ),
                          backgroundColor: WidgetStateProperty.all<Color>(GiftColors.giftDesignB1),
                          shape: WidgetStateProperty.all(
                            RoundedRectangleBorder(
                              borderRadius: BorderRadius.circular(15),
                            ),
                          ),
                        ),
                        child: Text(GiftLocalizations.of(context)!.gift_send, style: const TextStyle(color: Colors.white,
                            fontSize: 12),),
                        onPressed: () {
                          _handleSendGift(model, clickInterval);
                        }),
                  ),
                ]
                    : [
                  Column(
                    mainAxisAlignment: MainAxisAlignment.start,
                    crossAxisAlignment: CrossAxisAlignment.center,
                    children: [
                      Text(
                        model.giftName ?? "",
                        style: const TextStyle(color: Colors.white, fontSize: 12),
                      ),
                      Text(
                        "${model.price}",
                        style: const TextStyle(color: Colors.white, fontSize: 10),
                      ),
                    ],
                  ),
                ],
              )
            ],
          ),
        );
      },
    );
  }

  void _handleSendGift(GiftModel model, intervalInSeconds) {
    if (_isEnableSinceLastClick(intervalInSeconds)) {
        _sendGift(model);
    }
  }

  bool _isEnableSinceLastClick(int intervalInSeconds) {
    final now = DateTime.now();
    if (_lastClickTime == null || now.difference(_lastClickTime!) > Duration(seconds: intervalInSeconds)) {
      _lastClickTime = now;
      return true;
    }
    return false;
  }

  void _sendGift(GiftModel model) {
    GiftMessage giftMessage = GiftMessage();
    giftMessage.sender = GiftStore().selfInfo;
    giftMessage.receiver = GiftStore().ownerInfo;
    giftMessage.gift = model;
    giftMessage.giftCount = 1;
    GiftStore().giftManager.sendGift(giftMessage);
  }
}
