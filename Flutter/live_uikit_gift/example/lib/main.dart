import 'package:flutter/material.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_cloud_chat_sdk/enum/V2TimSDKListener.dart';
import 'package:tencent_cloud_chat_sdk/enum/log_level_enum.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_callback.dart';
import 'package:tencent_cloud_chat_sdk/tencent_im_sdk_plugin.dart';

import 'debug/generate_test_user_sig.dart';

void main() => runApp(const ExampleApp());

class ExampleApp extends StatelessWidget {
  const ExampleApp({super.key});

  @override
  Widget build(BuildContext context) {
    login();
    return MaterialApp(localizationsDelegates: const [
      ...GiftLocalizations.localizationsDelegates,
    ], supportedLocales: const [
      ...GiftLocalizations.supportedLocales,
    ], theme: ThemeData.dark(), home: const HomeScreen());
  }
}

class HomeScreen extends StatefulWidget {
  const HomeScreen({super.key});

  @override
  State<HomeScreen> createState() => _HomeScreenState();
}

class _HomeScreenState extends State<HomeScreen> {
  static const userId = '1236666';
  static const userName = 'X6666X';
  static const roomId = "live_1236666";
  final GiftUser ownerInfo =
      GiftUser(userId: userId, userName: userName, avatarUrl: '', level: '0');
  final GiftUser selfInfo =
      GiftUser(userId: userId, userName: userName, avatarUrl: '', level: '0');

  late GiftSendController sendController;
  late LikeSendController likeController;
  late GiftDisplayController displayController;

  @override
  void initState() {
    super.initState();
    likeController =
        LikeSendController(roomId: roomId, owner: ownerInfo, self: selfInfo);
    sendController =
        GiftSendController(roomId: roomId, owner: ownerInfo, self: selfInfo);

    displayController = GiftDisplayController(
        roomId: roomId,
        owner: ownerInfo,
        self: selfInfo,
        enablePreloading: true);
    displayController.setGiftCallback(
        onReceiveGiftCallback: _onReceiveGiftCallback);
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SafeArea(
        child: Stack(
          children: [
            Positioned(
              left: 20,
              bottom: 50,
              width: 40,
              height: 40,
              child: SizedBox(
                width: 40,
                height: 40,
                child: GiftSendWidget(
                  controller: sendController,
                ),
              ),
            ),
            Positioned(
              left: 80,
              bottom: 50,
              width: 40,
              height: 40,
              child: SizedBox(
                width: 40,
                height: 40,
                child: LikeSendWidget(
                  controller: likeController,
                ),
              ),
            ),
            Positioned(
              left: 0,
              top: 0,
              width: MediaQuery.of(context).size.width,
              height: MediaQuery.of(context).size.height,
              child: GiftDisplayWidget(controller: displayController),
            ),
          ],
        ),
      ),
    );
  }

  void _onReceiveGiftCallback(GiftMessage message) {
    debugPrint(
        "DemoOnReceiveGiftListener onReceiveGift message:${message.toString()}");
  }
}

bool isLogin = false;

void login() async {
  const userId = '1236666';
  const roomId = "live_1236666";
  if (isLogin) {
    return;
  }
  isLogin = true;
  var initResult = await TencentImSDKPlugin.v2TIMManager.initSDK(
    sdkAppID: GenerateTestUserSig.sdkAppId,
    loglevel: LogLevelEnum.V2TIM_LOG_INFO,
    listener: V2TimSDKListener(),
  );
  debugPrint("init login result：${initResult.code}-${initResult.desc}");

  if (initResult.code == 0) {
    V2TimCallback imLoginResult = await TencentImSDKPlugin.v2TIMManager.login(
      userID: userId,
      userSig: GenerateTestUserSig.genTestSig(userId),
    );
    debugPrint("im login result：${imLoginResult.code}-${imLoginResult.desc}");

    var result = await TUIRoomEngine.login(
      GenerateTestUserSig.sdkAppId,
      userId,
      GenerateTestUserSig.genTestSig(userId),
    );
    debugPrint("roomEngine login Result：${result.code}-${result.message}");

    TUIRoomInfo roomInfo = TUIRoomInfo(roomId: roomId);
    roomInfo.roomType = TUIRoomType.livingRoom;
    final createRoomResult =
        await TUIRoomEngine.sharedInstance().createRoom(roomInfo);
    debugPrint(
        "Create room result：${createRoomResult.code}-${createRoomResult.message}");
    final enterRoomResult = await TUIRoomEngine.sharedInstance()
        .enterRoom(roomId, roomType: TUIRoomType.livingRoom);
    debugPrint(
        'enter room result：${enterRoomResult.code}-${enterRoomResult.message}');
    TUILiveListManager listManager = await TUIRoomEngine.sharedInstance()
        .getExtension(TUIExtensionType.liveListManger);
    listManager.setLiveInfo(roomId, isPublicVisible: true);
  }
}
