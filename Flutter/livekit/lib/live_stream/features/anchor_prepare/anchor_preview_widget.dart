import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/live_core_widget.dart';
import 'package:rtc_room_engine/api/common/tui_common_define.dart';
import 'package:tencent_live_uikit/common/error/error_handler.dart';
import 'package:tencent_live_uikit/common/screen/index.dart';
import 'package:tencent_live_uikit/live_stream/features/anchor_prepare/anchor_preview_widget_define.dart';
import 'package:tencent_live_uikit/live_stream/live_define.dart';
import 'package:rtc_room_engine/api/common/tui_video_view.dart';

import '../../../common/constants/constants.dart';
import '../../../common/language/index.dart';
import '../../../common/resources/index.dart';
import '../../../common/widget/index.dart';
import '../../manager/live_stream_manager.dart';
import 'widgets/anchor_preview_info_edit_widget.dart';
import 'widgets/anchor_preview_function_widget.dart';

class AnchorPreviewWidget extends StatefulWidget {
  final LiveStreamManager liveStreamManager;
  final LiveCoreController liveCoreController;
  final DidClickBack? didClickBack;
  final DidClickStart? didClickStart;

  const AnchorPreviewWidget(
      {super.key,
      required this.liveStreamManager,
      required this.liveCoreController,
      this.didClickBack,
      this.didClickStart});

  @override
  State<AnchorPreviewWidget> createState() => _AnchorPreviewWidgetState();
}

class _AnchorPreviewWidgetState extends State<AnchorPreviewWidget> {
  late final LiveStreamManager liveStreamManager;
  late final LiveCoreController liveCoreController;
  late final EditInfo _editInfo;

  @override
  void initState() {
    super.initState();
    liveStreamManager = widget.liveStreamManager;
    liveCoreController = widget.liveCoreController;

    _initEditInfo();
    _startCameraAndMicrophone();
  }

  @override
  Widget build(BuildContext context) {
    return PopScope(
      canPop: false,
      child: Container(
        color: LiveColors.notStandardPureBlack,
        child: Stack(children: [
          _buildVideoWidget(),
          _buildBackWidget(),
          _buildLiveInfoEditWidget(liveStreamManager),
          _buildFunctionWidget(),
          _buildStartLiveWidget()
        ]),
      ),
    );
  }

  Widget _buildVideoWidget() {
    return Padding(
      padding: EdgeInsets.only(top: 36.height, bottom: 96.height),
      child: ClipRRect(
          borderRadius: BorderRadius.circular(16.radius),
          child: VideoView(
            onViewCreated: (id) {
              widget.liveCoreController.setLocalVideoView(id);
            },
          )
      ),
    );
  }

  Widget _buildBackWidget() {
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

  Widget _buildLiveInfoEditWidget(LiveStreamManager manager) {
    return Positioned(
        top: 96.height,
        left: 16.width,
        right: 16.width,
        height: 112.height,
        child: AnchorPreviewInfoEditWidget(
            editInfo: _editInfo,
            liveStreamManager: liveStreamManager,
            liveCoreController: liveCoreController));
  }

  Widget _buildFunctionWidget() {
    return Positioned(
        left: 0,
        bottom: 134.height,
        width: 375.width,
        height: 62.height,
        child: AnchorPreviewFunctionWidget(
            editInfo: _editInfo,
            liveStreamManager: liveStreamManager,
            liveCoreController: liveCoreController));
  }

  Widget _buildStartLiveWidget() {
    return Positioned(
      left: 50.width,
      right: 50.width,
      bottom: 64.height,
      height: 48.height,
      child: GestureDetector(
        onTap: () {
          _createRoom();
        },
        child: Container(
          alignment: Alignment.center,
          decoration: BoxDecoration(
            borderRadius: BorderRadius.circular(24.height),
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

extension on _AnchorPreviewWidgetState {
  void _initEditInfo() {
    _editInfo = EditInfo(
        roomName: _getDefaultRoomName(),
        privacyMode: LiveStreamPrivacyStatus.public);
  }

  String _getDefaultRoomName() {
    final selfInfo = liveCoreController.userState.selfInfo;
    return selfInfo.userName.isEmpty ? selfInfo.userId : selfInfo.userName;
  }

  void _startCameraAndMicrophone() async {
    final startCameraResult = await liveCoreController.startCamera(true);
    if (startCameraResult.code != TUIError.success) {
      liveStreamManager.toastSubject.add(ErrorHandler.convertToErrorMessage(
              startCameraResult.code.rawValue, startCameraResult.message) ??
          '');
    }
    final startMicrophoneResult = await liveCoreController.startMicrophone();
    if (startMicrophoneResult.code != TUIError.success) {
      liveStreamManager.toastSubject.add(ErrorHandler.convertToErrorMessage(
              startMicrophoneResult.code.rawValue,
              startMicrophoneResult.message) ??
          '');
    }
  }

  void _stopCameraAndMicrophone() {
    liveCoreController.stopCamera();
    liveCoreController.stopMicrophone();
  }

  void _createRoom() {
    widget.didClickStart?.call(_editInfo);
  }

  void _closeWidget() {
    _stopCameraAndMicrophone();
    Navigator.pop(Global.appContext());
  }
}
