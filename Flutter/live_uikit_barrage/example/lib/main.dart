import 'package:live_uikit_barrage/live_uikit_barrage.dart';
import 'package:barrage_example/debug/generate_test_user_sig.dart';
import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:rtc_room_engine/api/room/tui_room_engine.dart';
import 'package:rtc_room_engine/api/room/tui_room_observer.dart';
import 'package:tencent_cloud_chat_sdk/enum/V2TimSDKListener.dart';
import 'package:tencent_cloud_chat_sdk/enum/log_level_enum.dart';
import 'package:tencent_cloud_chat_sdk/models/v2_tim_callback.dart';
import 'package:tencent_cloud_chat_sdk/tencent_im_sdk_plugin.dart';

void main() {
  runApp(MyApp());
}

class MyApp extends StatelessWidget {
  MyApp({super.key});

  late BarrageDisplayController displayController;
  late BarrageSendController sendController;

  @override
  Widget build(BuildContext context) {
    initialize();
    return MaterialApp(
      localizationsDelegates: const [
        ...BarrageLocalizations.localizationsDelegates,
      ],
      supportedLocales: const [
        ...BarrageLocalizations.supportedLocales,
      ],
      home: Scaffold(
        appBar: AppBar(
          title: const Text('Barrage example app'),
        ),
        backgroundColor: const Color(0x60C5CCDB),
        resizeToAvoidBottomInset: false,
        body: SafeArea(
          child: OrientationBuilder(
            builder: (context, orientation) {
              debugPrint(orientation.toString());
              const userId = '1239999';
              const userName = 'X666X';
              const roomId = 'live_$userId';
              sendController = BarrageSendController(
                  roomId: roomId,
                  ownerId: userId,
                  selfUserId: userId,
                  selfName: userName);
              displayController = BarrageDisplayController(
                  roomId: roomId,
                  ownerId: userId,
                  selfUserId: userId,
                  selfName: userName);
              displayController
                  .setCustomBarrageBuilder(GiftBarrageItemBuilder());
              return Stack(
                alignment: AlignmentDirectional.bottomStart,
                children: [
                  Center(
                    child: Container(
                        // color: Colors.green,
                        margin: const EdgeInsets.only(left: 16, right: 56),
                        constraints: const BoxConstraints(maxHeight: 280),
                        child: BarrageDisplayWidget(
                            controller: displayController)),
                  ),
                  Positioned(
                    bottom: 0,
                    left: 0,
                    child: SizedBox(
                        width: 130,
                        height: 36,
                        child: BarrageSendWidget(controller: sendController)),
                  ),
                  Positioned(
                      bottom: 0,
                      right: 0,
                      child: ElevatedButton(
                        onPressed: () {
                          Barrage barrage = Barrage();
                          barrage.content = "gift_item";
                          BarrageUser user = BarrageUser();
                          user.userId = "1238866";
                          user.userName = "xander";
                          user.level = "66";
                          barrage.user = user;
                          displayController.insertMessage(barrage);
                        },
                        child: const Text("send gift"),
                      )),
                ],
              );
            },
          ),
        ),
      ),
    );
  }

  Future<void> initialize() async {
    const userId = '1239999';
    const roomId = 'live_$userId';
    var initResult = await TencentImSDKPlugin.v2TIMManager.initSDK(
      sdkAppID: GenerateTestUserSig.sdkAppId,
      loglevel: LogLevelEnum.V2TIM_LOG_INFO,
      listener: V2TimSDKListener(),
    );
    debugPrint("init result：${initResult.code}-${initResult.desc}");

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

      final roomInfo = TUIRoomInfo(roomId: roomId);
      roomInfo.roomType = TUIRoomType.livingRoom;
      final createRoomResult =
      await TUIRoomEngine.sharedInstance().createRoom(roomInfo);
      debugPrint(
          "create room result：${createRoomResult.code}-${createRoomResult.message}");
      if (createRoomResult.code == TUIError.success) {
        final enterRoomResult =
        await TUIRoomEngine.sharedInstance().enterRoom(roomId);
        debugPrint(
            "create room result：${enterRoomResult.code}-${enterRoomResult.message}");
      }

      TUIRoomEngine.sharedInstance().addObserver(TUIRoomObserver(
          onRemoteUserEnterRoom: (String roomId, TUIUserInfo userInfo) {
        BarrageUser barrageUser = BarrageUser();
        barrageUser.userId = userInfo.userId;
        barrageUser.userId = userInfo.userName;
        barrageUser.avatarUrl = userInfo.avatarUrl;
        barrageUser.level = "66";

        Barrage barrage = Barrage();
        barrage.user = barrageUser;
        barrage.content = "enter room";
        displayController.insertMessage(barrage);
      }));
    }
  }
}

class GiftBarrageItemBuilder extends CustomBarrageBuilder {
  @override
  Widget buildWidget(BuildContext context, Barrage barrage) {
    return ShaderMask(
      shaderCallback: (Rect bounds) {
        return const LinearGradient(
          colors: [Colors.white, Colors.blue],
          begin: Alignment.topLeft,
          end: Alignment.bottomRight,
        ).createShader(bounds);
      },
      child: const Text(
        'receive a gift',
        style: TextStyle(
          fontSize: 20,
          fontWeight: FontWeight.bold,
          color: Colors.white,
        ),
      ),
    );
  }

  @override
  bool shouldCustomizeBarrageItem(Barrage barrage) {
    if (barrage.content == "gift_item") {
      return true;
    }
    return false;
  }
}
