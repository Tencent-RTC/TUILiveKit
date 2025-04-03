import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit_example/generated/l10n.dart';
import 'package:tencent_live_uikit_example/src/service/app_manager.dart';
import 'package:tencent_live_uikit_example/src/store/app_store.dart';

class UpdateNicknameWidget extends StatefulWidget {
  const UpdateNicknameWidget({super.key});

  @override
  State<UpdateNicknameWidget> createState() => _UpdateNicknameWidgetState();
}

class _UpdateNicknameWidgetState extends State<UpdateNicknameWidget> {
  late double _screenWidth;
  String _inputNickname = AppStore.userName.value;
  final TextEditingController _controller = TextEditingController();

  @override
  void initState() {
    super.initState();
    _controller.text = AppStore.userName.value;
  }

  @override
  Widget build(BuildContext context) {
    _screenWidth = MediaQuery.of(context).size.width;

    return SizedBox(
      width: _screenWidth,
      height: 600,
      child: Column(
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.center,
        children: [
          _initTitleWidget(),
          _initInputNameWidget(),
          _initSubmitWidget()
        ],
      ),
    );
  }

  _initTitleWidget() {
    return SizedBox(
      height: 44,
      width: _screenWidth,
      child: Stack(
        children: [
          Positioned(
            left: 14,
            child: GestureDetector(
              onTap: () {
                Navigator.pop(context);
              },
              child: Container(
                width: 44,
                height: 44,
                padding: const EdgeInsets.all(10),
                child: Image.asset(
                  "assets/app_cancel_modification.png",
                ),
              ),
            ),
          ),
          Center(
            child: Text(
              S.current.app_set_nickname,
              style: const TextStyle(
                  color: Color(0xFF0F1014),
                  fontWeight: FontWeight.w500,
                  fontSize: 16),
            ),
          ),
        ],
      ),
    );
  }

  _initInputNameWidget() {
    return Container(
      margin: const EdgeInsets.only(left: 30, top: 32, right: 30),
      padding: const EdgeInsets.only(left: 10),
      width: MediaQuery.of(context).size.width - 60,
      height: 52,
      decoration: BoxDecoration(
        color: Colors.transparent,
        border: Border.all(),
        borderRadius: const BorderRadius.all(Radius.circular(8)),
      ),
      child: Row(
        mainAxisAlignment: MainAxisAlignment.spaceBetween,
        mainAxisSize: MainAxisSize.max,
        children: [
          SizedBox(
            width: MediaQuery.of(context).size.width - 150,
            child: TextField(
              controller: _controller,
              decoration: const InputDecoration(
                border: InputBorder.none,
                labelStyle: TextStyle(fontSize: 16),
              ),
              onChanged: ((value) => _inputNickname = value),
            ),
          ),
          GestureDetector(
            onTap: () {
              _clearInput();
            },
            child: Container(
              width: 40,
              height: 40,
              padding: const EdgeInsets.all(10),
              color: Colors.transparent,
              child: Image.asset(
                "assets/app_clear_content.png",
              ),
            ),
          ),
        ],
      ),
    );
  }

  _initSubmitWidget() {
    return Container(
        width: 335,
        height: 48,
        margin: const EdgeInsets.only(
          top: 350,
        ),
        child: ElevatedButton(
            onPressed: () => _updateNickname(),
            style: ButtonStyle(
              backgroundColor: WidgetStateProperty.all(const Color(0xff056DF6)),
              shape: WidgetStateProperty.all(RoundedRectangleBorder(
                  borderRadius: BorderRadius.circular(24))),
            ),
            child: Text(
              S.current.app_save,
              style: const TextStyle(
                  fontSize: 16,
                  fontStyle: FontStyle.normal,
                  fontWeight: FontWeight.w500,
                  color: Colors.white),
            )));
  }
}

extension _UpdateNicknameWidgetStateLogicExtension
    on _UpdateNicknameWidgetState {
  void _clearInput() {
    _controller.text = "";
  }

  void _updateNickname() async {
    if (_inputNickname.isEmpty) {
      makeToast(msg: S.current.app_enter_nickname);
      return;
    } else if (_inputNickname == AppStore.userName.value) {
      makeToast(msg: S.current.app_enter_nickname);
      return;
    }
    final result =
        await AppManager.setSelfInfo(AppStore.userAvatar, _inputNickname);
    if (result.code == 0) {
      Navigator.of(context).pop();
    }
  }
}
