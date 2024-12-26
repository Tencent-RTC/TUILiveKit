import 'dart:math';

import 'package:barrage/barrage.dart';
import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';
import 'package:gift/gift.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/widget/live_room/audience/component/live_streaming/audience_function_widget.dart';
import 'package:tencent_live_uikit/widget/live_room/audience/component/live_streaming/audience_waiting_pass_widget.dart';

class AudienceLivingWidget extends BasicWidget {
  const AudienceLivingWidget({super.key, required super.liveController});

  @override
  BasicState getState() {
    return AudienceLivingWidgetState();
  }
}

class AudienceLivingWidgetState extends BasicState<AudienceLivingWidget> {
  BarrageDisplayController? _barrageDisplayController;
  GiftDisplayController? _giftDisplayController;

  @override
  void initState() {
    super.initState();
    liveController.getUserState().enterUserInfo.addListener(_onRemoteUserEnterRoom);
  }

  @override
  void dispose() {
    liveController.getUserState().enterUserInfo.removeListener(_onRemoteUserEnterRoom);
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
      valueListenable: liveController.getViewState().liveStatus,
      builder: (BuildContext context, value, Widget? child) {
        return Stack(
          children: [
            _initLiveInfoWidget(),
            _initAudienceListWidget(),
            _initCloseWidget(),
            _initAudienceWaitingPassWidget(),
            _initBarrageWidget(),
            _initFunctionWidget(),
            _initGiftDisplayWidget(),
          ],
        );
      },
    );
  }

  Widget _initLiveInfoWidget() {
    return Positioned(
        left: 16,
        top: 54,
        height: 32,
        width: 170,
        child: LiveInfoWidget(
          liveController: liveController,
        ));
  }

  Widget _initAudienceListWidget() {
    return ValueListenableBuilder(
      valueListenable: liveController.getUserState().userList,
      builder: (BuildContext context, value, Widget? child) {
        return Stack(
          children: [
            Positioned(
              top: 57,
              right: 48,
              child: AudienceListWidget(liveController: liveController),
            ),
          ],
        );
      },
    );
  }

  Widget _initCloseWidget() {
    return Positioned(
        top: 57,
        right: 14,
        width: 24,
        height: 24,
        child: GestureDetector(
          onTap: () {
            liveController.exit();
            Navigator.pop(context);
          },
          child: Image.asset(
            LivekitImages.livekitAudienceClose,
            package: Constants.pluginName,
          ),
        ));
  }

  Widget _initAudienceWaitingPassWidget() {
    return Positioned(right: 8, top: 90, child: AudienceWaitingPassWidget(liveController: liveController));
  }

  Widget _initBarrageWidget() {
    return Positioned(
        left: 16,
        bottom: 80,
        height: 212,
        width: screenWidth - 72,
        child: ValueListenableBuilder(
          valueListenable: liveController.getRoomSate().enterRoomSuccess,
          builder: (BuildContext context, bool value, Widget? child) {
            if (liveController.getRoomSate().enterRoomSuccess.value) {
              _barrageDisplayController ??= BarrageDisplayController(
                  roomId: liveController.getRoomSate().roomId,
                  ownerId: liveController.getRoomSate().ownerInfo.userId,
                  selfUserId: liveController.getUserState().selfInfo.userId,
                  selfName: liveController.getUserState().selfInfo.name.value);
              _barrageDisplayController?.setCustomBarrageBuilder(
                  GiftBarrageItemBuilder(selfUserId: liveController.getUserState().selfInfo.userId));
              return BarrageDisplayWidget(controller: _barrageDisplayController!);
            } else {
              return Container();
            }
          },
        ));
  }

  _initGiftDisplayWidget() {
    return Positioned(
        left: 0,
        top: 0,
        width: MediaQuery.of(context).size.width,
        height: MediaQuery.of(context).size.height,
        child: ValueListenableBuilder(
            valueListenable: liveController.getRoomSate().enterRoomSuccess,
            builder: (BuildContext context, bool value, Widget? child) {
              if (liveController.getRoomSate().enterRoomSuccess.value) {
                if (_giftDisplayController == null) {
                  GiftUser ownerInfo = GiftUser(
                      userId: liveController.getRoomSate().ownerInfo.userId,
                      avatarUrl: liveController.getRoomSate().ownerInfo.avatarUrl.value,
                      userName: liveController.getRoomSate().ownerInfo.name.value,
                      level: "66");

                  GiftUser selfInfo = GiftUser(
                      userId: liveController.getUserState().selfInfo.userId,
                      avatarUrl: liveController.getUserState().selfInfo.avatarUrl.value,
                      userName: liveController.getUserState().selfInfo.name.value,
                      level: "32");

                  _giftDisplayController = GiftDisplayController(
                      roomId: liveController.getRoomSate().roomId, owner: ownerInfo, self: selfInfo);
                  _giftDisplayController?.setGiftCallback(
                      onReceiveGiftCallback: _insertToBarrageMessage, onSendGiftCallback: _insertToBarrageMessage);
                }
                return GiftDisplayWidget(controller: _giftDisplayController!);
              } else {
                return Container();
              }
            }));
  }

  Widget _initFunctionWidget() {
    return Positioned(
      left: 0,
      bottom: 34,
      height: 36,
      width: screenWidth,
      child: AudienceFunctionWidget(
        liveController: liveController,
      ),
    );
  }
}

extension AudienceLivingWidgetStateExtension on AudienceLivingWidgetState {
  _onRemoteUserEnterRoom() {
    final userInfo = liveController.getUserState().enterUserInfo.value;
    BarrageUser barrageUser = BarrageUser();
    barrageUser.userId = userInfo.userId;
    barrageUser.userName = userInfo.name.value ?? userInfo.userId;
    barrageUser.avatarUrl = userInfo.avatarUrl.value ?? "";
    barrageUser.level = "66";

    Barrage barrage = Barrage();
    barrage.user = barrageUser;
    barrage.content = LiveKitLocalizations.of(Global.appContext())!.livekit_entered_room;
    _barrageDisplayController?.insertMessage(barrage);
  }

  void _insertToBarrageMessage(GiftMessage message) {
    Barrage barrage = Barrage();
    barrage.content = "gift";
    barrage.user.userId = message.sender?.userId ?? "";
    barrage.user.userName = message.sender?.userName ?? message.sender?.userId ?? "";
    barrage.user.avatarUrl = message.sender?.avatarUrl ?? "";
    barrage.user.level = message.sender?.level ?? "66";
    barrage.extInfo[keyGiftViewType] = valueGiftViewType;
    barrage.extInfo[keyGiftName] = message.gift?.giftName;
    barrage.extInfo[keyGiftCount] = message.giftCount;
    barrage.extInfo[keyGiftImage] = message.gift?.imageUrl;
    barrage.extInfo[keyGiftReceiverUserId] = message.receiver?.userId ?? "";
    barrage.extInfo[keyGiftReceiverUsername] = message.receiver?.userName ?? message.receiver?.userId ?? "";
    _barrageDisplayController?.insertMessage(barrage);
  }
}

class GiftBarrageItemBuilder extends CustomBarrageBuilder {
  final String selfUserId;

  List<Color> giftMessageColor = [
    LivekitColors.livekitBarrageColorMsg1,
    LivekitColors.livekitBarrageColorMsg2,
    LivekitColors.livekitBarrageColorMsg3,
    LivekitColors.livekitBarrageColorMsg4,
    LivekitColors.livekitBarrageColorMsg5,
    LivekitColors.livekitBarrageColorMsg6,
    LivekitColors.livekitBarrageColorMsg7
  ];

  GiftBarrageItemBuilder({required this.selfUserId});

  @override
  Widget buildWidget(BuildContext context, Barrage barrage) {
    String receiverUserId = barrage.extInfo[keyGiftReceiverUserId];
    String receiverUserName = barrage.extInfo[keyGiftReceiverUsername];
    String giftUrl = barrage.extInfo[keyGiftImage];
    String giftName = barrage.extInfo[keyGiftName];
    int giftCount = barrage.extInfo[keyGiftCount];
    String senderUserId = barrage.user.userId;
    String senderUserName = barrage.user.userName;
    if (senderUserId == selfUserId) {
      senderUserName = LiveKitLocalizations.of(context)!.livekit_gift_me;
    }
    if (receiverUserId == selfUserId) {
      receiverUserName = LiveKitLocalizations.of(context)!.livekit_gift_me;
    }
    return Wrap(
      children: [
        Container(
          margin: const EdgeInsets.only(top: 3, bottom: 3),
          padding: const EdgeInsets.only(left: 6, top: 4, right: 6, bottom: 4),
          decoration: BoxDecoration(
            color: LivekitColors.livekitNotStandard40G1,
            borderRadius: BorderRadius.circular(14),
          ),
          child: Row(
            crossAxisAlignment: CrossAxisAlignment.start,
            mainAxisSize: MainAxisSize.min,
            children: [
              Text(
                senderUserName,
                style: const TextStyle(
                    fontSize: 12, fontWeight: FontWeight.w700, color: LivekitColors.livekitBarrageUserNameColor),
              ),
              Text(
                LiveKitLocalizations.of(context)!.livekit_sent,
                style: const TextStyle(fontSize: 12, fontWeight: FontWeight.w700, color: Colors.white),
              ),
              Text(
                receiverUserName,
                style: const TextStyle(
                    fontSize: 12, fontWeight: FontWeight.w700, color: LivekitColors.livekitBarrageUserNameColor),
              ),
              const SizedBox(width: 4),
              Text(
                giftName,
                style:
                    TextStyle(fontSize: 12, fontWeight: FontWeight.w700, color: giftMessageColor[Random().nextInt(7)]),
              ),
              Padding(
                padding: const EdgeInsets.only(top: 3.0),
                child: CachedNetworkImage(
                  width: 13,
                  height: 13,
                  imageUrl: giftUrl,
                  fit: BoxFit.fitWidth,
                  placeholder: (context, url) => _buildDefaultGift(),
                  errorWidget: (context, url, error) => _buildDefaultGift(),
                ),
              ),
              Text(
                "x$giftCount",
                style: const TextStyle(fontSize: 12, fontWeight: FontWeight.w700, color: Colors.white),
              ),
            ],
          ),
        ),
      ],
    );
  }

  @override
  bool shouldCustomizeBarrageItem(Barrage barrage) {
    if (barrage.extInfo.containsKey(keyGiftViewType)) {
      return true;
    }
    return false;
  }

  _buildDefaultGift() {
    return Container(color: Colors.transparent);
  }
}

const String keyGiftViewType = "GIFT_VIEW_TYPE";
const String valueGiftViewType = "GIFT_VIEW_TYPE";
const String keyGiftName = "GIFT_NAME";
const String keyGiftCount = "GIFT_COUNT";
const String keyGiftImage = "GIFT_IMAGE";
const String keyGiftReceiverUsername = "GIFT_RECEIVER_USERNAME";
const String keyGiftReceiverUserId = "GIFT_RECEIVER_USER_ID";
