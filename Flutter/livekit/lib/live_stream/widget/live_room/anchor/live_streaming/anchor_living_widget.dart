import 'dart:math';

import 'package:live_uikit_barrage/live_uikit_barrage.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';

import '../../../../../common/index.dart';
import '../../../../manager/index.dart';
import '../../../../state/index.dart';
import 'anchor_living_function_widget.dart';
import 'link/apply_link_mic_float_widget.dart';

class AnchorLivingWidget extends BasicWidget {
  const AnchorLivingWidget({super.key, required super.liveController});

  @override
  AnchorLivingWidgetState getState() {
    return AnchorLivingWidgetState();
  }
}

class AnchorLivingWidgetState extends BasicState<AnchorLivingWidget> {
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
    return Stack(
      children: [
        _initBackWidget(),
        _initLiveInfoWidget(),
        _initAudienceListWidget(),
        _initFunctionWidget(),
        _initApplyLinkAudienceWidget(),
        _initBarrageWidget(),
        _initGiftDisplayWidget(),
      ],
    );
  }

  _initBackWidget() {
    return Positioned(
      right: 16,
      top: 58,
      width: 24,
      height: 24,
      child: GestureDetector(
        onTap: () {
          _closeLiveRoom();
        },
        child: Image.asset(
          LiveImages.close,
          package: Constants.pluginName,
        ),
      ),
    );
  }

  _initLiveInfoWidget() {
    return Positioned(
        left: 16,
        top: 54,
        height: 32,
        width: 170,
        child: LiveInfoWidget(
          liveController: liveController,
        ));
  }

  _initAudienceListWidget() {
    return Positioned(
        right: 48,
        top: 58,
        width: 122,
        height: 24,
        child: AudienceListWidget(
          liveController: liveController,
        ));
  }

  _initFunctionWidget() {
    return Positioned(
      left: 0,
      bottom: 34,
      height: 36,
      width: MediaQuery.sizeOf(Global.appContext()).width,
      child: AnchorLivingFunctionWidget(
        liveController: liveController,
      ),
    );
  }

  _initBarrageWidget() {
    return Positioned(
        left: 16,
        bottom: 80,
        height: 212,
        width: MediaQuery.sizeOf(Global.appContext()).width - 72,
        child: ValueListenableBuilder(
          valueListenable: liveController.getRoomSate().enterRoomSuccess,
          builder: (BuildContext context, bool value, Widget? child) {
            if (liveController.getRoomSate().enterRoomSuccess.value) {
              if (_barrageDisplayController == null) {
                _barrageDisplayController = BarrageDisplayController(
                    roomId: liveController.getRoomSate().roomId,
                    ownerId: liveController.getRoomSate().ownerInfo.userId,
                    selfUserId: liveController.getUserState().selfInfo.userId,
                    selfName: liveController.getUserState().selfInfo.name.value);
                _barrageDisplayController?.setCustomBarrageBuilder(
                    GiftBarrageItemBuilder(selfUserId: liveController.getUserState().selfInfo.userId));
              }
              return BarrageDisplayWidget(controller: _barrageDisplayController!);
            } else {
              return Container();
            }
          },
        ));
  }

  _initApplyLinkAudienceWidget() {
    return Positioned(
      right: 8,
      top: 90,
      height: 86,
      width: 114,
      child: ApplyLinkMicFloatWidget(
        liveController: liveController,
      ),
    );
  }

  _initGiftDisplayWidget() {
    return Positioned(
        left: 0,
        top: 0,
        width: MediaQuery.sizeOf(context).width,
        height: MediaQuery.sizeOf(context).height,
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
                _giftDisplayController?.setGiftCallback(onReceiveGiftCallback: _insertToBarrageMessage,
                    onSendGiftCallback: _insertToBarrageMessage);
              }
              return GiftDisplayWidget(controller: _giftDisplayController!);
            } else {
              return Container();
            }
          },
        ));
  }
}

extension AnchorLivingWidgetStateLogicExtension on AnchorLivingWidgetState {
  _closeLiveRoom() {
    liveController.viewController.updateLiveStatus(LiveStatus.dashboard);
    RoomController roomController = liveController.roomController;
    roomController.updateLikeNumber(0);
    roomController.updateMessageCount(0);
    roomController.exit();
    BarrageDisplayController.resetState();
    GiftDisplayController.resetState();
  }

  _onRemoteUserEnterRoom() {
    final userInfo = liveController.getUserState().enterUserInfo.value;
    BarrageUser barrageUser = BarrageUser();
    barrageUser.userId = userInfo.userId;
    barrageUser.userName = userInfo.name.value ?? userInfo.userId;
    barrageUser.avatarUrl = userInfo.avatarUrl.value ?? "";
    barrageUser.level = "66";

    Barrage barrage = Barrage();
    barrage.user = barrageUser;
    barrage.content = LiveKitLocalizations.of(Global.appContext())!.live_entered_room;
    _barrageDisplayController?.insertMessage(barrage);
  }

  void _insertToBarrageMessage(GiftMessage message) {
    Barrage barrage = Barrage();
    barrage.content = "gift";
    barrage.user.userId = message.sender?.userId ?? "";
    barrage.user.userName = message.sender?.userName ?? message.sender?.userId ?? "";
    barrage.user.avatarUrl = message.sender?.avatarUrl ?? "";
    barrage.user.level = message.sender?.level ?? "66";
    barrage.extInfo[Constants.keyGiftViewType] = Constants.valueGiftViewType;
    barrage.extInfo[Constants.keyGiftName] = message.gift?.giftName;
    barrage.extInfo[Constants.keyGiftCount] = message.giftCount;
    barrage.extInfo[Constants.keyGiftImage] = message.gift?.imageUrl;
    barrage.extInfo[Constants.keyGiftReceiverUserId] = message.receiver?.userId ?? "";
    barrage.extInfo[Constants.keyGiftReceiverUsername] = message.receiver?.userName ?? message.receiver?.userId ?? "";
    _barrageDisplayController?.insertMessage(barrage);
  }
}

class GiftBarrageItemBuilder extends CustomBarrageBuilder {
  final String selfUserId;

  List<Color> giftMessageColor = [
    LiveColors.barrageColorMsg1,
    LiveColors.barrageColorMsg2,
    LiveColors.barrageColorMsg3,
    LiveColors.barrageColorMsg4,
    LiveColors.barrageColorMsg5,
    LiveColors.barrageColorMsg6,
    LiveColors.barrageColorMsg7
  ];

  GiftBarrageItemBuilder({required this.selfUserId});

  @override
  Widget buildWidget(BuildContext context, Barrage barrage) {
    String receiverUserId = barrage.extInfo[Constants.keyGiftReceiverUserId];
    String receiverUserName = barrage.extInfo[Constants.keyGiftReceiverUsername];
    String giftUrl = barrage.extInfo[Constants.keyGiftImage];
    String giftName = barrage.extInfo[Constants.keyGiftName];
    int giftCount = barrage.extInfo[Constants.keyGiftCount];
    String senderUserId = barrage.user.userId;
    String senderUserName = barrage.user.userName;
    if (senderUserId == selfUserId) {
      senderUserName = LiveKitLocalizations.of(context)!.live_gift_me;
    }
    if (receiverUserId == selfUserId) {
      receiverUserName = LiveKitLocalizations.of(context)!.live_gift_me;
    }
    return Wrap(
      children: [
        Container(
          margin: const EdgeInsets.only(top: 3, bottom: 3),
          padding: const EdgeInsets.only(left: 6, top: 4, right: 6, bottom: 4),
          decoration: BoxDecoration(
            color: LiveColors.notStandard40G1,
            borderRadius: BorderRadius.circular(14),
          ),
          child: Row(
            crossAxisAlignment: CrossAxisAlignment.start,
            mainAxisSize: MainAxisSize.min,
            children: [
              SizedBox(width: context.adapter.getWidth(4)),
              Text(
                senderUserName,
                style: const TextStyle(
                    fontSize: 12, fontWeight: FontWeight.w700, color: LiveColors.barrageUserNameColor),
              ),
              SizedBox(width: context.adapter.getWidth(4)),
              Text(
                LiveKitLocalizations.of(context)!.live_sent,
                style: const TextStyle(fontSize: 12, fontWeight: FontWeight.w700, color: Colors.white),
              ),
              SizedBox(width: context.adapter.getWidth(4)),
              Text(
                receiverUserName,
                style: const TextStyle(
                    fontSize: 12, fontWeight: FontWeight.w700, color: LiveColors.barrageUserNameColor),
              ),
              SizedBox(width: context.adapter.getWidth(4)),
              Text(
                giftName,
                style:
                    TextStyle(fontSize: 12, fontWeight: FontWeight.w700, color: giftMessageColor[Random().nextInt(7)]),
              ),
              SizedBox(width: context.adapter.getWidth(4)),
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
              SizedBox(width: context.adapter.getWidth(4)),
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
    if (barrage.extInfo.containsKey(Constants.keyGiftViewType)) {
      return true;
    }
    return false;
  }

  _buildDefaultGift() {
    return Container(color: Colors.transparent);
  }
}