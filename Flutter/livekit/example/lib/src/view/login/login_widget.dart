import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:tencent_cloud_uikit_core/tencent_cloud_uikit_core.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit_example/debug/generate_test_user_sig.dart';
import 'package:tencent_live_uikit_example/generated/l10n.dart';
import 'package:tencent_live_uikit_example/src/service/app_manager.dart';
import 'package:tencent_live_uikit_example/src/store/app_store.dart';
import 'package:tencent_live_uikit_example/src/view/login/profile_widget.dart';
import 'package:tencent_live_uikit_example/src/view/main/main_widget.dart';
import 'package:rtc_room_engine/rtc_room_engine.dart';
import 'package:tencent_cloud_chat_sdk/tencent_im_sdk_plugin.dart';

class LoginWidget extends StatefulWidget {
  const LoginWidget({super.key});

  @override
  State<LoginWidget> createState() => _LoginWidgetState();
}

class _LoginWidgetState extends State<LoginWidget> {
  var _userId = '';

  bool _isButtonEnabled = true;
  bool _isTestEnvironment = false;

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      resizeToAvoidBottomInset: false,
      appBar: AppBar(
        backgroundColor: Colors.transparent,
      ),
      body: SizedBox(
        width: MediaQuery.sizeOf(context).width,
        child: Stack(
          children: [_buildAppInfo(), _buildLogin()],
        ),
      ),
    );
  }

  Widget _buildAppInfo() {
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
                        width: _calculateTextWidth(S.current.app_trtc, const TextStyle(fontSize: 32)) >
                                (MediaQuery.of(context).size.width - 70 - 10)
                            ? _calculateTextWidth(S.current.app_trtc, const TextStyle(fontSize: 32)) / 2
                            : _calculateTextWidth(S.current.app_trtc, const TextStyle(fontSize: 32)),
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

  Widget _buildLogin() {
    return Positioned(
        left: 0,
        top: MediaQuery.sizeOf(context).height * 2 / 5,
        width: MediaQuery.sizeOf(context).width,
        child: Column(mainAxisAlignment: MainAxisAlignment.center, children: [
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
                  S.current.app_user_id,
                  style: const TextStyle(fontSize: 16, fontStyle: FontStyle.normal, color: Colors.black),
                ),
                const SizedBox(width: 10),
                SizedBox(
                    width: MediaQuery.sizeOf(context).width - 160,
                    child: TextField(
                        autofocus: true,
                        decoration: const InputDecoration(
                          border: InputBorder.none,
                          labelStyle: TextStyle(fontSize: 16),
                        ),
                        onChanged: ((value) => _userId = value)))
              ],
            ),
          ),
          const SizedBox(height: 40),
          SizedBox(
            height: 52,
            width: MediaQuery.sizeOf(context).width - 60,
            child: ElevatedButton(
              onPressed: () => _isButtonEnabled ? _login() : null,
              style: ButtonStyle(
                backgroundColor: WidgetStateProperty.all(const Color(0xff056DF6)),
                shape: WidgetStateProperty.all(RoundedRectangleBorder(borderRadius: BorderRadius.circular(8))),
              ),
              child: Text(
                S.current.app_login,
                style: const TextStyle(
                    fontSize: 16, fontStyle: FontStyle.normal, fontWeight: FontWeight.w500, color: Colors.white),
              ),
            ),
          ),
          const SizedBox(height: 24),
          Row(mainAxisAlignment: MainAxisAlignment.end,children: [
            const Text('Test Environment'),
            const SizedBox(width: 24),
            Switch(value: _isTestEnvironment, onChanged: (value){
              if (value == _isTestEnvironment) { return; }
             setState(() {
               _isTestEnvironment = value;
             });
            }),
            const SizedBox(width: 24)
          ],)
        ]));
  }

  void _login() async {
    _isButtonEnabled = false;

    if (_isTestEnvironment) {
      await _switchTestEnvironment();
    }

    await TUILogin.instance.login(
      GenerateTestUserSig.sdkAppId,
      _userId,
      GenerateTestUserSig.genTestSig(_userId),
      TUICallback(
        onError: (code, message) {
          LiveKitLogger.error("TUILogin login fail, {code:$code, message:$message}");
          makeToast(msg: "code:$code message:$message");
        },
        onSuccess: () async {
          LiveKitLogger.info("TUILogin login success");
          AppStore.userId = _userId;
          await AppManager.getUserInfo(_userId);
          if (AppStore.userName.value.isEmpty || AppStore.userAvatar.isEmpty) {
            _enterProfileWidget();
          } else {
            _enterMainWidget();
          }
        },
      ),
    );
    _isButtonEnabled = true;
  }

  Future<void> _switchTestEnvironment() async {
    final Map<String, dynamic> params = {'enableRoomTestEnv': true};

    final Map<String, dynamic> jsonObject = {
      'api': 'setTestEnvironment',
      'params': params
    };

    final jsonString = jsonEncode(jsonObject);
    TUIRoomEngine.sharedInstance().invokeExperimentalAPI(jsonString);

    final result = await TencentImSDKPlugin.v2TIMManager
        .callExperimentalAPI(api: 'setTestEnvironment', param: true);
    if (result.code == 0) {
      debugPrint('switchTestEnvironment success');
    }
  }

  void _enterProfileWidget() {
    Navigator.of(context).pushAndRemoveUntil(MaterialPageRoute(
      builder: (context) {
        return const ProfileWidget();
      },
    ), (route) => false);
  }

  void _enterMainWidget() {
    Navigator.of(context).pushAndRemoveUntil(MaterialPageRoute(
      builder: (context) {
        return const MainWidget();
      },
    ), (route) => false);
  }

  double _calculateTextWidth(String text, TextStyle textStyle) {
    TextPainter textPainter = TextPainter(
      text: TextSpan(text: text, style: textStyle),
      textDirection: TextDirection.ltr,
    );
    textPainter.layout();
    return textPainter.width;
  }
}
