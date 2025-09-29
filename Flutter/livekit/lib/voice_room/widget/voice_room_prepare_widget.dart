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
    _screenWidth = MediaQuery.sizeOf(context).width;
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
        top: 96.height,
        left: 16.width,
        right: 16.width,
        height: 112.height,
        child: LivePrepareInfoEditWidget(manager: manager));
  }

  Widget _initSeatPreviewWidget() {
    return Positioned(
        top: 212.height,
        left: 0,
        bottom: 0,
        right: 0,
        child: const SeatPreviewWidget());
  }

  Widget _initFunctionWidget() {
    return Positioned(
        left: 0,
        bottom: 134.height,
        width: 375.width,
        height: 62.height,
        child: LivePrepareFunctionWidget(manager: manager));
  }

  Widget _initBackWidget() {
    return Positioned(
        left: 16.width,
        top: 56.height,
        width: 24.radius,
        height: 24.radius,
        child: GestureDetector(
            onTap: () {
              _closeWidget();
            },
            child: Image.asset(LiveImages.returnArrow,
                package: Constants.pluginName)));
  }

  Widget _initStartLiveWidget() {
    return Positioned(
      left: 50.width,
      right: 50.width,
      bottom: 64.height,
      height: 40.height,
      child: GestureDetector(
        onTap: () {
          _createRoom();
        },
        child: Container(
          alignment: Alignment.center,
          decoration: BoxDecoration(
            borderRadius: BorderRadius.circular(20.height),
            color: LiveColors.designStandardB1,
          ),
          child: Text(
            LiveKitLocalizations.of(Global.appContext())!.common_start_live,
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
