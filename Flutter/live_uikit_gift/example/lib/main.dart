import 'package:flutter/material.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';

import 'debug/generate_test_user_sig.dart';

void main() => runApp(const ExampleApp());

class ExampleApp extends StatelessWidget {
  const ExampleApp({super.key});

  @override
  Widget build(BuildContext context) {
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
  static const roomId = "live_krab1";
  final ValueNotifier<bool> enterRoomSuccess = ValueNotifier(false);
  bool isLogin = false;

  @override
  void initState() {
    super.initState();
    login();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SafeArea(
        child: ValueListenableBuilder(
            valueListenable: enterRoomSuccess,
            builder: (context, success, _) {
              return Visibility(
                visible: success,
                child: GiftWidget(roomId: roomId),
              );
            }),
      ),
    );
  }

  void login() async {
    const userId = '12369999';
    const roomId = "live_krab1";
    if (isLogin) {
      return;
    }
    isLogin = true;

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
    if (enterRoomResult.code == TUIError.success) {
      enterRoomSuccess.value = true;
    }
    TUILiveListManager listManager = await TUIRoomEngine.sharedInstance()
        .getExtension(TUIExtensionType.liveListManager);
    listManager.setLiveInfo(roomId, isPublicVisible: true);
  }
}

class GiftWidget extends StatelessWidget {
  final String roomId;
  GiftSendController? sendController;
  LikeSendController? likeController;
  GiftDisplayController? displayController;

  GiftWidget({super.key, required this.roomId});

  @override
  Widget build(BuildContext context) {
    likeController ??= LikeSendController(roomId: roomId);
    sendController ??= GiftSendController(roomId: roomId);

    if (displayController == null) {
      displayController =
          GiftDisplayController(roomId: roomId, enablePreloading: true);
      displayController?.onReceiveGiftCallback = _onReceiveGiftCallback;
    }

    return Stack(
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
              controller: sendController!,
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
              controller: likeController!,
            ),
          ),
        ),
        Positioned(
          left: 0,
          top: 0,
          width: MediaQuery.sizeOf(context).width,
          height: MediaQuery.sizeOf(context).height,
          child: GiftDisplayWidget(giftDisplayController: displayController!),
        ),
      ],
    );
  }

  void _onReceiveGiftCallback(
      TUIGiftInfo giftInfo, int count, TUIUserInfo sender) {
    debugPrint(
        "DemoOnReceiveGiftListener onReceiveGift giftInfo:$giftInfo, count:$count, sender:$sender");
  }
}
