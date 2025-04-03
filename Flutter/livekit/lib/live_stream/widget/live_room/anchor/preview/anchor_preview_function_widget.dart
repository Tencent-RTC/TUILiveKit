import 'package:flutter/material.dart';

import '../../../../../common/index.dart';

class AnchorPreviewFunctionWidget extends BasicWidget {
  const AnchorPreviewFunctionWidget({super.key, required super.liveController});

  @override
  BasicState getState() {
    return AnchorPreviewFunctionWidgetState();
  }
}

class AnchorPreviewFunctionWidgetState extends BasicState<AnchorPreviewFunctionWidget> {
  @override
  Widget build(BuildContext context) {
    return Row(mainAxisAlignment: MainAxisAlignment.spaceEvenly, children: [
      _initBeautyWidget(),
      _initAudioEffectWidget(),
      _initFlipWidget(),
      _initMirrorWidget(),
    ]);
  }

  _initBeautyWidget() {
    return GestureDetector(
      onTap: () {
        _showBeautyPanel();
      },
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          SizedBox(
            width: 36,
            height: 36,
            child: Image.asset(
              LiveImages.previewFunctionItemBeauty,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.live_video_settings_item_beauty,
            style: const TextStyle(
              fontSize: 12,
              color: LiveColors.designStandardG7,
            ),
            overflow: TextOverflow.ellipsis,
          ),
        ],
      ),
    );
  }

  _initAudioEffectWidget() {
    return GestureDetector(
      onTap: () {
        _showAudioEffectPanel();
      },
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          SizedBox(
            width: 36,
            height: 36,
            child: Image.asset(
              LiveImages.previewFunctionItemMusic,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.live_audio_effect,
            style: const TextStyle(
              fontSize: 12,
              color: LiveColors.designStandardG7,
            ),
            overflow: TextOverflow.ellipsis,
          ),
        ],
      ),
    );
  }

  _initFlipWidget() {
    return GestureDetector(
      onTap: () {
        _flipCamera();
      },
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          SizedBox(
            width: 36,
            height: 36,
            child: Image.asset(
              LiveImages.previewFunctionItemFlip,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.live_video_settings_item_flip,
            style: const TextStyle(
              fontSize: 12,
              color: LiveColors.designStandardG7,
            ),
            overflow: TextOverflow.ellipsis,
          ),
        ],
      ),
    );
  }

  _initMirrorWidget() {
    return GestureDetector(
      onTap: () {
        _setVideoMirror();
      },
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          SizedBox(
            width: 36,
            height: 36,
            child: Image.asset(
              LiveImages.previewFunctionItemMirror,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.live_video_settings_item_mirror,
            style: const TextStyle(
              fontSize: 12,
              color: LiveColors.designStandardG7,
            ),
            overflow: TextOverflow.ellipsis,
          ),
        ],
      ),
    );
  }
}

extension AnchorPreviewFunctionWidgetStateLogicExtension on AnchorPreviewFunctionWidgetState {
  _showBeautyPanel() {
    showWidget(
      BeautyPanelWidget(liveController: liveController),
      barrierColor: LiveColors.designStandardTransparent,
    );
  }

  _showAudioEffectPanel() {
    showWidget(AudioEffectPanelWidget(liveController: liveController));
  }

  _flipCamera() {
    liveController.mediaController.switchCamera();
  }

  _setVideoMirror() {
    liveController.mediaController.setCameraMirror();
  }
}
