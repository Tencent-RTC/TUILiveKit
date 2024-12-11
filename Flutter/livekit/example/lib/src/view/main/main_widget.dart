import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/tencent_live_uikit.dart';
import 'package:tencent_live_uikit_example/generated/l10n.dart';
import 'package:tencent_live_uikit_example/src/service/app_manager.dart';
import 'package:tencent_live_uikit_example/src/store/app_store.dart';
import 'package:tencent_live_uikit_example/src/view/main/me_widget.dart';
import 'package:url_launcher/url_launcher.dart';

class MainWidget extends StatefulWidget {
  const MainWidget({super.key});

  @override
  State<MainWidget> createState() => _MainWidgetState();
}

class _MainWidgetState extends State<MainWidget> {
  List<Widget> _contentWidgetList = [];
  late double _screenWidth;

  @override
  void initState() {
    super.initState();
    _initWidget();
  }

  @override
  Widget build(BuildContext context) {
    _screenWidth = MediaQuery.of(context).size.width;

    return PopScope(
        canPop: false,
        child: ValueListenableBuilder(
          valueListenable: AppStore.currentFragmentIndex,
          builder: (BuildContext context, int value, Widget? child) {
            return Scaffold(
              body: SizedBox(
                width: _screenWidth,
                height: double.infinity,
                child: Stack(
                  children: [
                    _initTopBackgroundWidget(),
                    _initAppBarWidget(),
                    _initContentWidget(),
                  ],
                ),
              ),
              bottomNavigationBar: _initBottomNavigationBar(),
            );
          },
        ));
  }

  _initTopBackgroundWidget() {
    return SizedBox(
        width: _screenWidth,
        height: 200,
        child: Image.asset(
          'assets/app_top_background.png',
          fit: BoxFit.fill,
        ));
  }

  _initAppBarWidget() {
    return Positioned(
      left: 10,
      top: 40,
      right: 10,
      child: Visibility(
        visible: AppStore.currentFragmentIndex.value == AppStore.fragmentIndexRoomList,
        child: Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          crossAxisAlignment: CrossAxisAlignment.center,
          mainAxisSize: MainAxisSize.max,
          children: [
            Container(
              width: 23,
            ),
            Text(
              S.current.app_live,
              style: const TextStyle(
                  fontSize: 18, fontStyle: FontStyle.normal, fontWeight: FontWeight.w500, color: Color(0xFF000000)),
            ),
            GestureDetector(
              onTap: () {
                _launchUrl(AppStore.tuiLiveKitDocumentUrl);
              },
              child: Container(
                width: 40,
                height: 40,
                padding: const EdgeInsets.all(8),
                color: Colors.transparent,
                child: Image.asset(
                  'assets/app_question_link.png',
                  width: 24,
                  height: 24,
                ),
              ),
            ),
          ],
        ),
      ),
    );
  }

  _initContentWidget() {
    return Positioned(
      top: 80,
      left: 0,
      right: 0,
      bottom: 0,
      child: _contentWidgetList[AppStore.currentFragmentIndex.value],
    );
  }

  _initBottomNavigationBar() {
    return Container(
      color: LivekitColors.livekitDesignStandardFlowkitWhite,
      width: double.infinity,
      height: 80,
      child: Row(
        mainAxisAlignment: MainAxisAlignment.spaceAround,
        crossAxisAlignment: CrossAxisAlignment.center,
        children: [
          GestureDetector(
            onTap: () {
              _startRoomListWidget();
            },
            child: Container(
              height: 80,
              width: 80,
              color: Colors.transparent,
              padding: const EdgeInsets.only(top: 20),
              child: Column(
                children: [
                  Image.asset(
                    AppStore.currentFragmentIndex.value == AppStore.fragmentIndexRoomList
                        ? 'assets/app_live_hall_blue.png'
                        : 'assets/app_live_hall_black.png',
                    width: 25,
                    height: 25,
                  ),
                  Text(
                    S.current.app_live,
                    style: TextStyle(
                        fontSize: 12,
                        fontStyle: FontStyle.normal,
                        fontWeight: FontWeight.w500,
                        color: AppStore.currentFragmentIndex.value == AppStore.fragmentIndexRoomList
                            ? const Color(0xFF0099FF)
                            : const Color(0xFF000000)),
                  ),
                ],
              ),
            ),
          ),
          GestureDetector(
            onTap: () {
              _startAnchorWidget();
            },
            child: Image.asset(
              'assets/app_start_live.png',
              width: 40,
              height: 40,
            ),
          ),
          GestureDetector(
            onTap: () {
              _startMeWidget();
            },
            child: Container(
              height: 80,
              width: 80,
              color: Colors.transparent,
              padding: const EdgeInsets.only(top: 20),
              child: Column(
                children: [
                  Image.asset(
                    AppStore.currentFragmentIndex.value == AppStore.fragmentIndexMe
                        ? 'assets/app_me_blue.png'
                        : 'assets/app_me_black.png',
                    width: 25,
                    height: 25,
                  ),
                  Text(
                    S.current.app_me,
                    style: TextStyle(
                        fontSize: 12,
                        fontStyle: FontStyle.normal,
                        fontWeight: FontWeight.w500,
                        color: AppStore.currentFragmentIndex.value == AppStore.fragmentIndexMe
                            ? const Color(0xFF0099FF)
                            : const Color(0xFF000000)),
                  ),
                ],
              ),
            ),
          ),
        ],
      ),
    );
  }
}

extension _MainWidgetStateLogicExtension on _MainWidgetState {
  _initWidget() {
    _contentWidgetList.clear();
    _contentWidgetList.add(LiveListWidget());
    _contentWidgetList.add(Container(
      color: Colors.transparent,
    ));
    _contentWidgetList.add(const MeWidget());
  }

  _startAnchorWidget() async {
    Navigator.push(context, MaterialPageRoute(
      builder: (context) {
        return TUILiveRoomAnchorWidget(
            roomId: LiveIdentityGenerator.instance.generateId(AppStore.userId, RoomType.live));
      },
    ));
  }

  _launchUrl(String url) async {
    await launchUrl(Uri.parse(url));
  }

  void _startRoomListWidget() {
    AppStore.currentFragmentIndex.value = AppStore.fragmentIndexRoomList;
  }

  void _startMeWidget() {
    AppStore.currentFragmentIndex.value = AppStore.fragmentIndexMe;
    AppManager.getUserFollowInfo(AppStore.userId);
  }
}
