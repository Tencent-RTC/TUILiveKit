import 'package:flutter/cupertino.dart';
import 'package:flutter/material.dart';
import 'package:tencent_live_uikit_example/generated/l10n.dart';
import 'package:tencent_live_uikit_example/src/store/app_store.dart';
import 'package:tencent_live_uikit_example/src/view/login/login_widget.dart';
import 'package:tencent_live_uikit_example/src/view/main/update_nickname_widget.dart';

class MeWidget extends StatefulWidget {
  const MeWidget({super.key});

  @override
  State<MeWidget> createState() => _MeWidgetState();
}

class _MeWidgetState extends State<MeWidget> {
  late double _screenWidth;

  @override
  Widget build(BuildContext context) {
    _screenWidth = MediaQuery.of(context).size.width;

    return SizedBox(
      width: _screenWidth,
      child: Column(
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.center,
        children: [_initUserAvatarWidget(), _initUserNameWidget(), _initFollowWidget()],
      ),
    );
  }

  _initUserAvatarWidget() {
    return Container(
        margin: const EdgeInsets.only(top: 27),
        child: GestureDetector(
          onTap: () => _showDialog(),
          child: Container(
              width: 100,
              height: 100,
              clipBehavior: Clip.hardEdge,
              decoration: const BoxDecoration(
                borderRadius: BorderRadius.all(Radius.circular(27.5)),
              ),
              child: ClipOval(
                child: Image(
                  image: NetworkImage(AppStore.userAvatar.isNotEmpty ? AppStore.userAvatar : AppStore.defaultAvatar),
                  fit: BoxFit.cover,
                  errorBuilder: (ctx, err, stackTrace) => Image.asset('assets/people.webp'),
                ),
              )),
        ));
  }

  _initUserNameWidget() {
    return Container(
        margin: const EdgeInsets.only(top: 16),
        child: GestureDetector(
          onTap: () {
            _showUpdateNicknameWidget();
          },
          child: ValueListenableBuilder(
            valueListenable: AppStore.userName,
            builder: (BuildContext context, String value, Widget? child) {
              return Text(
                AppStore.userName.value.isNotEmpty ? AppStore.userName.value : AppStore.userId,
                style: const TextStyle(fontSize: 22, fontWeight: FontWeight.w500, color: Color(0xFF0F1014)),
              );
            },
          ),
        ));
  }

  _initFollowWidget() {
    return Container(
      margin: const EdgeInsets.only(top: 20),
      alignment: Alignment.center,
      child: Row(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          Text(
            S.current.app_follow_count,
            style: const TextStyle(fontSize: 14, color: Color(0xFF99A2B2)),
          ),
          const SizedBox(
            width: 12,
          ),
          ValueListenableBuilder(
            valueListenable: AppStore.followCount,
            builder: (BuildContext context, int value, Widget? child) {
              return Text(
                "${AppStore.followCount.value}",
                style: const TextStyle(fontSize: 14, color: Colors.black),
              );
            },
          ),
          const SizedBox(
            width: 30,
          ),
          Text(
            S.current.app_fans_count,
            style: const TextStyle(fontSize: 14, color: Color(0xFF99A2B2)),
          ),
          const SizedBox(
            width: 12,
          ),
          ValueListenableBuilder(
            valueListenable: AppStore.fansCount,
            builder: (BuildContext context, int value, Widget? child) {
              return Text(
                "${AppStore.fansCount.value}",
                style: const TextStyle(fontSize: 14, color: Colors.black),
              );
            },
          ),
        ],
      ),
    );
  }

  _showDialog() {
    showDialog(
        context: context,
        builder: (BuildContext context) {
          return CupertinoAlertDialog(
            title: Text(S.current.app_login),
            actions: [
              CupertinoDialogAction(child: Text(S.current.app_cancel), onPressed: () => Navigator.of(context).pop()),
              CupertinoDialogAction(
                  child: Text(S.current.app_confirm),
                  onPressed: () {
                    _logout();
                  })
            ],
          );
        });
  }
}

extension _MeWidgetStateLogicExtension on _MeWidgetState {
  void _showUpdateNicknameWidget() {
    showModalBottomSheet(
      isScrollControlled: true,
      context: context,
      builder: (context) => Container(
        decoration: const BoxDecoration(
          borderRadius: BorderRadius.only(
            topLeft: Radius.circular(20),
            topRight: Radius.circular(20),
          ),
          color: Colors.white,
        ),
        child: const UpdateNicknameWidget(),
      ),
    );
  }

  _logout() {
    Navigator.of(context).pop();
    Navigator.of(context).pushAndRemoveUntil(MaterialPageRoute(builder: (widget) {
      return const LoginWidget();
    }), (route) => false);
  }
}
