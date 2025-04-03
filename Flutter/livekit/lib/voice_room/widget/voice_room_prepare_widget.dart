import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';

import '../../common/index.dart';
import '../index.dart';

class VoiceRoomPrepareWidget extends StatefulWidget {
  final VoiceRoomManager manager;
  final void Function()? didClickStart;

  const VoiceRoomPrepareWidget(
      {super.key, required this.manager, this.didClickStart});

  @override
  State<VoiceRoomPrepareWidget> createState() => _VoiceRoomPrepareWidgetState();
}

class _VoiceRoomPrepareWidgetState extends State<VoiceRoomPrepareWidget> {
  late final VoiceRoomManager manager;
  late double _screenWidth;

  @override
  void initState() {
    super.initState();
    manager = widget.manager;
  }

  @override
  Widget build(BuildContext context) {
    _screenWidth = MediaQuery.of(context).size.width;
    return PopScope(
      canPop: false,
      child: SizedBox(
        width: _screenWidth,
        child: Stack(
          children: [
            _initBackgroundWidget(manager),
            _initLiveInfoEditWidget(manager),
            _initSeatPreviewWidget(),
            _initFunctionWidget(),
            _initBackWidget(),
            _initStartLiveWidget()
          ],
        ),
      ),
    );
  }

  Widget _initBackgroundWidget(VoiceRoomManager manager) {
    return SizedBox(
      width: _screenWidth,
      child: ValueListenableBuilder(
          valueListenable: manager.roomState.backgroundUrl,
          builder: (context, value, child) {
            return CachedNetworkImage(imageUrl: value, fit: BoxFit.cover);
          }),
    );
  }

  Widget _initLiveInfoEditWidget(VoiceRoomManager manager) {
    return Positioned(
        top: context.adapter.getHeight(96),
        left: context.adapter.getWidth(16),
        right: context.adapter.getWidth(16),
        height: context.adapter.getHeight(112),
        child: LivePrepareInfoEditWidget(manager: manager));
  }

  Widget _initSeatPreviewWidget() {
    return Positioned(
        top: context.adapter.getHeight(212),
        left: 0,
        bottom: 0,
        right: 0,
        child: const SeatPreviewWidget());
  }

  Widget _initFunctionWidget() {
    return Positioned(
        left: 0,
        bottom: context.adapter.getHeight(134),
        width: context.adapter.getWidth(375),
        height: context.adapter.getHeight(62),
        child: LivePrepareFunctionWidget(manager: manager));
  }

  Widget _initBackWidget() {
    return Positioned(
        left: context.adapter.getWidth(16),
        top: context.adapter.getHeight(56),
        width: context.adapter.getWidth(24),
        height: context.adapter.getWidth(24),
        child: GestureDetector(
            onTap: () {
              _closeWidget();
            },
            child: Image.asset(LiveImages.returnArrow,
                package: Constants.pluginName)));
  }

  Widget _initStartLiveWidget() {
    return Positioned(
      left: context.adapter.getWidth(50),
      right: context.adapter.getWidth(50),
      bottom: context.adapter.getHeight(64),
      height: context.adapter.getHeight(40),
      child: GestureDetector(
        onTap: () {
          _createRoom();
        },
        child: Container(
          alignment: Alignment.center,
          decoration: BoxDecoration(
            borderRadius: BorderRadius.circular(context.adapter.getHeight(20)),
            color: LiveColors.designStandardB1,
          ),
          child: Text(
            LiveKitLocalizations.of(Global.appContext())!.live_start_live,
            style: const TextStyle(
                color: LiveColors.designStandardFlowkitWhite,
                fontSize: 16,
                fontWeight: FontWeight.w700),
          ),
        ),
      ),
    );
  }
}

extension on _VoiceRoomPrepareWidgetState {
  void _createRoom() {
    widget.didClickStart?.call();
  }

  void _closeWidget() {
    Navigator.pop(Global.appContext());
  }
}
