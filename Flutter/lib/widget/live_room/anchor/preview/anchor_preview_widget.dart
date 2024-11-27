import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

import 'anchor_preview_function_widget.dart';

class AnchorPreviewWidget extends BasicWidget {
  const AnchorPreviewWidget({super.key, required super.liveController});

  @override
  BasicState getState() {
    return AnchorPreviewWidgetState();
  }
}

class AnchorPreviewWidgetState extends BasicState<AnchorPreviewWidget> {
  @override
  Widget build(BuildContext context) {
    return Stack(
      children: [_initLiveInfoEditWidget(), _initFunctionWidget(), _initBackWidget(), _initStartLiveWidget()],
    );
  }

  _initLiveInfoEditWidget() {
    return Positioned(
      top: 96,
      left: (MediaQuery.of(Global.appContext()).size.width - 343) / 2,
      width: 343,
      height: 112,
      child: LiveInfoEditWidget(liveController: liveController),
    );
  }

  _initFunctionWidget() {
    return Positioned(
      left: 0,
      bottom: 134,
      width: MediaQuery.of(Global.appContext()).size.width,
      height: 56,
      child: AnchorPreviewFunctionWidget(liveController: liveController),
    );
  }

  _initBackWidget() {
    return Positioned(
      left: 16,
      top: 56,
      width: 24,
      height: 24,
      child: GestureDetector(
        onTap: () {
          _closeWidget();
        },
        child: Image.asset(
          LivekitImages.livekitReturnArrow,
          package: Constants.pluginName,
        ),
      ),
    );
  }

  _initStartLiveWidget() {
    return Positioned(
      left: (MediaQuery.of(Global.appContext()).size.width - 275) / 2,
      bottom: 64,
      width: 275,
      height: 40,
      child: GestureDetector(
        onTap: () {
          _createRoom();
        },
        child: Container(
          alignment: Alignment.center,
          decoration: BoxDecoration(
            borderRadius: BorderRadius.circular(20),
            color: LivekitColors.livekitDesignStandardB1,
          ),
          child: Text(
            LiveKitLocalizations.of(Global.appContext())!.livekit_start_live,
            style: const TextStyle(
                color: LivekitColors.livekitDesignStandardFlowkitWhite, fontSize: 16, fontWeight: FontWeight.w700),
          ),
        ),
      ),
    );
  }
}

extension AnchorPreviewWidgetStateLogicExtension on AnchorPreviewWidgetState {
  _createRoom() {
    Constants.dataReportComponent = Constants.dataReportComponentLiveRoom;
    liveController.start();
  }

  _closeWidget() {
    Navigator.pop(Global.appContext());
  }
}
