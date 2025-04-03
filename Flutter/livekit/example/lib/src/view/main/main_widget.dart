import 'dart:io';

import 'package:flutter/material.dart';
import 'package:path_provider/path_provider.dart';
import 'package:tencent_live_uikit_example/src/view/login/log/log_file_browser.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit_example/src/store/app_store.dart';
import 'package:tencent_live_uikit_example/src/view/index.dart';
import 'package:tencent_live_uikit_example/src/view/main/me_widget.dart';
import 'package:tencent_live_uikit_example/src/view/scene/voice_room_widget.dart';

import '../../../generated/l10n.dart';

class MainWidget extends StatefulWidget {
  const MainWidget({super.key});

  @override
  State<MainWidget> createState() => _MainWidgetState();
}

class _MainWidgetState extends State<MainWidget> {
  late double _screenWidth;

  @override
  Widget build(BuildContext context) {
    _screenWidth = MediaQuery.of(context).size.width;
    return Scaffold(
        appBar: AppBar(
          backgroundColor: Colors.white,
          title: Row(
            children: [
              Image.asset(
                'assets/app_icon.png',
                width: context.adapter.getWidth(30),
                height: context.adapter.getWidth(30),
                fit: BoxFit.fitWidth,
              ),
              SizedBox(width: context.adapter.getWidth(4)),
              const Text(
                'TUILiveKit',
                style: TextStyle(
                  fontSize: 24,
                  fontWeight: FontWeight.w400,
                  color: Colors.black,
                ),
              ),
              IconButton(
                onPressed: () => _showFileBrowser(),
                icon: Image.asset(
                  'assets/debug.png',
                  width: context.adapter.getWidth(30),
                  height: context.adapter.getWidth(30),
                ),
              ),
              Expanded(
                child: Align(
                  alignment: Alignment.centerRight,
                  child: GestureDetector(
                    onTap: () => _enterMeWidget(),
                    child: ClipOval(
                      child: Image.network(AppStore.userAvatar,
                          width: context.adapter.getWidth(30)),
                    ),
                  ),
                ),
              )
            ],
          ),
        ),
        body: Container(
          color: Colors.white,
          width: _screenWidth,
          height: double.infinity,
          child: Column(
            children: [
              Row(
                mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                children: [
                  Card(
                    child: SizedBox(
                      width: context.adapter.getWidth(165),
                      child: MenuItemWidget(
                        iconUrl: 'assets/app_video_live.png',
                        title: S.current.app_video,
                        description: S.current.app_video_description,
                        onTap: () => _enterVideoLiveWidget(),
                      ),
                    ),
                  ),
                  Card(
                    child: SizedBox(
                      width: context.adapter.getWidth(165),
                      child: MenuItemWidget(
                        iconUrl: 'assets/app_voice_room.png',
                        title: S.current.app_voice,
                        description: S.current.app_voice_description,
                        onTap: () => _enterVoiceRoomWidget(),
                      ),
                    ),
                  ),
                ],
              ),
            ],
          ),
        ));
  }

  void _showFileBrowser() async {
    Directory? startDirectory;
    if (Platform.isIOS) {
      startDirectory = await getApplicationDocumentsDirectory();
    } else {
      startDirectory = await getExternalStorageDirectory();
    }
    if (mounted) {
      Navigator.of(context).push(MaterialPageRoute(
        builder: (context) {
          return LogFileBrowser(startDirectory: startDirectory);
        },
      ));
    }
  }

  _enterMeWidget() {
    Navigator.of(context).push(MaterialPageRoute(
      builder: (context) {
        return const MeWidget();
      },
    ));
  }

  void _enterVideoLiveWidget() {
    Navigator.of(context).push(MaterialPageRoute(
      builder: (context) {
        return const VideoLiveWidget();
      },
    ));
  }

  void _enterVoiceRoomWidget() {
    Navigator.of(context).push(MaterialPageRoute(
      builder: (context) {
        return const VoiceRoomWidget();
      },
    ));
  }
}

class MenuItemWidget extends StatelessWidget {
  final String iconUrl;
  final String title;
  final String description;
  final void Function()? onTap;

  const MenuItemWidget(
      {super.key,
      required this.iconUrl,
      required this.title,
      required this.description,
      this.onTap});

  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      onTap: () => onTap?.call(),
      child: ClipRRect(
          borderRadius: BorderRadius.circular(context.adapter.getWidth(10)),
          child: Container(
            decoration: const BoxDecoration(
                gradient: LinearGradient(
                    colors: [Color(0xFFD9E8FE), Colors.white],
                    begin: Alignment.topCenter,
                    end: Alignment.bottomCenter)),
            child: Column(
              children: [
                Row(
                  children: [
                    Padding(
                      padding: EdgeInsets.only(
                          top: context.adapter.getHeight(16),
                          left: context.adapter.getWidth(16)),
                      child: Image.asset(
                        iconUrl,
                        width: context.adapter.getWidth(24),
                        height: context.adapter.getWidth(24),
                      ),
                    ),
                    SizedBox(width: context.adapter.getWidth(6)),
                    SizedBox(
                      width: context.adapter.getWidth(60),
                      child: Padding(
                        padding:
                            EdgeInsets.only(top: context.adapter.getHeight(16)),
                        child: Text(
                          title,
                          style: const TextStyle(
                            color: Color(0xFF262b32),
                          ),
                        ),
                      ),
                    ),
                    Expanded(
                      child: Padding(
                        padding: EdgeInsets.only(
                            top: context.adapter.getHeight(16),
                            left: context.adapter.getWidth(16)),
                        child: Image.asset(
                          'assets/app_arrow.png',
                          width: context.adapter.getWidth(16),
                          height: context.adapter.getWidth(16),
                        ),
                      ),
                    )
                  ],
                ),
                SizedBox(height: context.adapter.getHeight(20)),
                Padding(
                  padding: EdgeInsets.symmetric(horizontal: context.adapter.getWidth(16)),
                  child: Text(
                    description,
                    style: const TextStyle(color: Color(0xFF626e84)),
                  ),
                )
              ],
            ),
          )),
    );
  }
}
