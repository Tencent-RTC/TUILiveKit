import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

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
              LivekitImages.livekitPreviewFunctionItemBeauty,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.livekit_function_item_beauty,
            style: const TextStyle(
              fontSize: 12,
              color: LivekitColors.livekitDesignStandardG7,
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
              LivekitImages.livekitPreviewFunctionItemMusic,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.livekit_audio_effect,
            style: const TextStyle(
              fontSize: 12,
              color: LivekitColors.livekitDesignStandardG7,
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
              LivekitImages.livekitPreviewFunctionItemFlip,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.livekit_function_item_flip,
            style: const TextStyle(
              fontSize: 12,
              color: LivekitColors.livekitDesignStandardG7,
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
              LivekitImages.livekitPreviewFunctionItemMirror,
              package: Constants.pluginName,
            ),
          ),
          Text(
            LiveKitLocalizations.of(Global.appContext())!.livekit_function_item_mirror,
            style: const TextStyle(
              fontSize: 12,
              color: LivekitColors.livekitDesignStandardG7,
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
      barrierColor: LivekitColors.livekitDesignStandardTransparent,
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
