import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/tencent_live_uikit.dart';
import 'package:tencent_live_uikit_example/generated/l10n.dart';
import 'package:tencent_live_uikit_example/src/store/app_store.dart';
import 'package:url_launcher/url_launcher.dart';

class VideoLiveWidget extends StatefulWidget {
  const VideoLiveWidget({super.key});

  @override
  State<VideoLiveWidget> createState() => _VideoLiveWidgetState();
}

class _VideoLiveWidgetState extends State<VideoLiveWidget> {
  late double _screenWidth;

  @override
  void initState() {
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    _screenWidth = MediaQuery.sizeOf(context).width;

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
                    _initBroadcastWidget()
                  ],
                ),
              ),
            );
          },
        ));
  }

  Widget _initTopBackgroundWidget() {
    return SizedBox(
        width: _screenWidth,
        height: 200,
        child: Image.asset(
          'assets/app_top_background.png',
          fit: BoxFit.fill,
        ));
  }

  Widget _initAppBarWidget() {
    return Positioned(
      left: 10,
      top: 40,
      right: 10,
      child: Row(
        mainAxisAlignment: MainAxisAlignment.spaceBetween,
        crossAxisAlignment: CrossAxisAlignment.center,
        mainAxisSize: MainAxisSize.max,
        children: [
          IconButton(
              onPressed: () => Navigator.of(context).pop(),
              icon: Image.asset('assets/app_back.png', width: 24, height: 24)),
          Text(
            S.current.app_video,
            style: const TextStyle(
                fontSize: 18,
                fontStyle: FontStyle.normal,
                fontWeight: FontWeight.w500,
                color: Color(0xFF000000)),
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
    );
  }

  Widget _initContentWidget() {
    return const Positioned(
      top: 80,
      left: 0,
      right: 0,
      bottom: 0,
      child: LiveListWidget(),
    );
  }

  Widget _initBroadcastWidget() {
    return Positioned(
      bottom: 10,
      left: 0,
      right: 0,
      child: SizedBox(
        width: double.infinity,
        height: 80,
        child: Container(
          alignment: Alignment.topCenter,
          child: GestureDetector(
              onTap: () {
                _startAnchorWidget();
              },
              child: Container(
                width: 154,
                height: 48,
                alignment: Alignment.center,
                decoration: BoxDecoration(
                    color: const Color(0xFF1C66E5),
                    borderRadius: BorderRadius.circular(24)),
                child: Text(
                  S.current.app_broadcast('+'),
                  style: const TextStyle(fontSize: 20, color: Colors.white),
                ),
              )),
        ),
      ),
    );
  }
}

extension _VideoLiveWidgetStateLogicExtension on _VideoLiveWidgetState {
  void _startAnchorWidget() async {
    Navigator.push(context, MaterialPageRoute(
      builder: (context) {
        return TUILiveRoomAnchorWidget(
            roomId: LiveIdentityGenerator.instance
                .generateId(AppStore.userId, RoomType.live));
      },
    ));
  }

  void _launchUrl(String url) async {
    await launchUrl(Uri.parse(url));
  }
}
