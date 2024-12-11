import 'package:barrage/barrage.dart';
import 'package:barrage/state/barrage_user.dart';
import 'package:barrage_example/debug/generate_test_user_sig.dart';
import 'package:flutter/material.dart';
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
    login();
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
        body: OrientationBuilder(
          builder: (BuildContext context, Orientation orientation) {
            debugPrint(orientation.toString());
            sendController = BarrageSendController(
                roomId: "live_1239999", ownerId: "1239999", selfUserId: "1236666", selfName: "X6666X");
            displayController = BarrageDisplayController(
                roomId: "live_1239999", ownerId: "1239999", selfUserId: "1236666", selfName: "X6666X");
            displayController.setCustomBarrageBuilder(GiftBarrageItemBuilder());
            return Stack(
              children: [
                Center(
                  child: Container(
                      margin: const EdgeInsets.only(left: 16, right: 56),
                      constraints: const BoxConstraints(maxHeight: 280),
                      child: BarrageDisplayWidget(controller: displayController)),
                ),
                SizedBox(width: 130, height: 36, child: BarrageSendWidget(controller: sendController)),
                Positioned(
                    top: 0,
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
                      child: const Text("发送礼物"),
                    )),
              ],
            );
          },
        ),
      ),
    );
  }

  void login() async {
    // 初始化
    var initResult = await TencentImSDKPlugin.v2TIMManager.initSDK(
      sdkAppID: GenerateTestUserSig.sdkAppId, // 请替换为您的SDKAPPID
      loglevel: LogLevelEnum.V2TIM_LOG_INFO, // 日志登记等级
      listener: V2TimSDKListener(), // 事件监听器。使用弹幕聊天时，这里传空对象即可。
    );
    debugPrint("BarrageManager init结果：${initResult.code}-${initResult.desc}");

    if (initResult.code == 0) {
      V2TimCallback imLoginResult = await TencentImSDKPlugin.v2TIMManager.login(
        userID: '1236666',
        userSig: GenerateTestUserSig.genTestSig('1236666'),
      );
      debugPrint("BarrageManager im登陆结果：${imLoginResult.code}-${imLoginResult.desc}");

      var result = await TUIRoomEngine.login(
        GenerateTestUserSig.sdkAppId,
        '1236666',
        GenerateTestUserSig.genTestSig('1236666'),
      );
      debugPrint("BarrageManager roomEngine login Result：${result.code}-${result.message}");

      final enterRoomResult =
          await TUIRoomEngine.sharedInstance().enterRoom("live_1239999", roomType: TUIRoomType.livingRoom);
      debugPrint("BarrageManager enter room result：${enterRoomResult.code}-${enterRoomResult.message}");

      TUIRoomEngine.sharedInstance()
          .addObserver(TUIRoomObserver(onRemoteUserEnterRoom: (String roomId, TUIUserInfo userInfo) {
        BarrageUser barrageUser = BarrageUser();
        barrageUser.userId = userInfo.userId;
        barrageUser.userId = userInfo.userName;
        barrageUser.avatarUrl = userInfo.avatarUrl;
        barrageUser.level = "66";

        Barrage barrage = Barrage();
        barrage.user = barrageUser;
        barrage.content = "进入房间";
        displayController.insertMessage(barrage);
      }));
    }
  }
}

class GiftBarrageItemBuilder extends CustomBarrageBuilder {
  @override
  Widget buildWidget(BuildContext context, Barrage barrage) {
    return const Text(
      "收到直播间礼物",
      style: TextStyle(fontSize: 18, fontWeight: FontWeight.w700, color: Colors.red),
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
