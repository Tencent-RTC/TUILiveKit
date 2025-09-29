import 'dart:math';

import 'package:flutter/material.dart';
import 'package:tencent_live_uikit_example/generated/l10n.dart';
import 'package:tencent_live_uikit_example/src/service/app_manager.dart';
import 'package:tencent_live_uikit_example/src/store/app_store.dart';

import '../main/main_widget.dart';

class ProfileWidget extends StatefulWidget {
  const ProfileWidget({super.key});

  @override
  State<ProfileWidget> createState() => _ProfileWidgetState();
}

class _ProfileWidgetState extends State<ProfileWidget> {
  bool _isButtonEnabled = true;

  @override
  Widget build(BuildContext context) {
    return PopScope(
        canPop: false,
        child: Scaffold(
            resizeToAvoidBottomInset: false,
            body: SizedBox(
              width: MediaQuery.sizeOf(context).width,
              child: Stack(
                children: [_buildAppInfo(), _buildSetInfo()],
              ),
            )));
  }

  _buildAppInfo() {
    return Positioned(
        left: 0,
        top: MediaQuery.sizeOf(context).height / 6,
        width: MediaQuery.sizeOf(context).width,
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            Row(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                Image.asset(
                  'assets/qcloudlog.png',
                  width: 70,
                ),
                const SizedBox(width: 20),
                Column(
                  children: [
                    SizedBox(
                        width: _calculateTextWidth(S.current.app_trtc,
                                    const TextStyle(fontSize: 32)) >
                                (MediaQuery.sizeOf(context).width - 70 - 10)
                            ? _calculateTextWidth(S.current.app_trtc,
                                    const TextStyle(fontSize: 32)) /
                                2
                            : _calculateTextWidth(S.current.app_trtc,
                                const TextStyle(fontSize: 32)),
                        child: Text(
                          S.current.app_trtc,
                          maxLines: 3,
                          overflow: TextOverflow.ellipsis,
                          style: const TextStyle(
                              fontSize: 30,
                              fontStyle: FontStyle.normal,
                              fontWeight: FontWeight.w400,
                              color: Colors.black),
                        ))
                  ],
                )
              ],
            ),
          ],
        ));
  }

  _buildSetInfo() {
    return Positioned(
        left: 0,
        top: MediaQuery.sizeOf(context).height * 2 / 5,
        width: MediaQuery.sizeOf(context).width,
        child: Column(
          children: [
            Container(
              width: MediaQuery.sizeOf(context).width - 60,
              height: 52,
              decoration: BoxDecoration(
                color: Colors.transparent,
                border: Border.all(),
                borderRadius: const BorderRadius.all(Radius.circular(8)),
              ),
              child: Row(
                mainAxisAlignment: MainAxisAlignment.start,
                children: [
                  const SizedBox(width: 10),
                  Text(
                    S.current.app_nick_name,
                    style: const TextStyle(
                        fontSize: 16,
                        fontStyle: FontStyle.normal,
                        color: Colors.black),
                  ),
                  const SizedBox(width: 10),
                  SizedBox(
                    width: MediaQuery.sizeOf(context).width - 200,
                    child: TextField(
                      autofocus: true,
                      decoration: const InputDecoration(
                        border: InputBorder.none,
                        labelStyle: TextStyle(fontSize: 16),
                      ),
                      onChanged: ((value) => AppStore.userName.value = value),
                    ),
                  )
                ],
              ),
            ),
            const SizedBox(height: 40),
            SizedBox(
              height: 52,
              width: MediaQuery.sizeOf(context).width - 60,
              child: ElevatedButton(
                onPressed: () => _isButtonEnabled ? _setUserInfo() : null,
                style: ButtonStyle(
                  backgroundColor:
                      WidgetStateProperty.all(const Color(0xff056DF6)),
                  shape: WidgetStateProperty.all(RoundedRectangleBorder(
                      borderRadius: BorderRadius.circular(15))),
                ),
                child: Text(S.current.app_confirm,
                    style: const TextStyle(
                        fontSize: 16,
                        fontStyle: FontStyle.normal,
                        fontWeight: FontWeight.w500,
                        color: Colors.white)),
              ),
            )
          ],
        ));
  }

  double _calculateTextWidth(String text, TextStyle textStyle) {
    TextPainter textPainter = TextPainter(
      text: TextSpan(text: text, style: textStyle),
      textDirection: TextDirection.ltr,
    );
    textPainter.layout();
    return textPainter.width;
  }

  _setUserInfo() async {
    _isButtonEnabled = false;
    if (AppStore.userName.value.isNotEmpty) {
      int index = Random().nextInt(_userAvatarArray.length);
      var result = await AppManager.setSelfInfo(
          _userAvatarArray[index], AppStore.userName.value);
      if (result.code == 0) {
        _enterMainWidget();
      } else {
        _showDialog(result);
      }
    }
    _isButtonEnabled = true;
  }

  _enterMainWidget() {
    Navigator.of(context).pushAndRemoveUntil(MaterialPageRoute(
      builder: (context) {
        return const MainWidget();
      },
    ), (route) => false);
  }

  _showDialog(TUIResult result) {
    return showDialog(
      context: context,
      builder: (BuildContext context) {
        return AlertDialog(
          title: Text(S.current.app_login_fail),
          content: Text(
              "result.code:${result.code}, result.message: ${result.message}？"),
          actions: [
            TextButton(
              onPressed: () {
                Navigator.of(context).pop();
              },
              child: Text(S.current.app_next),
            ),
          ],
        );
      },
    );
  }

  final List<String> _userAvatarArray = [
    "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar1.png",
    "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar2.png",
    "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar3.png",
    "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar4.png",
    "https://liteav.sdk.qcloud.com/app/res/picture/voiceroom/avatar/user_avatar5.png",
  ];
}
