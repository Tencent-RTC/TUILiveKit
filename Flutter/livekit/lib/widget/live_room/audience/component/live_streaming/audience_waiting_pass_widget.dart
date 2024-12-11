import 'dart:async';
import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/state/live_define.dart';

class AudienceWaitingPassWidget extends BasicWidget {
  const AudienceWaitingPassWidget({super.key, required super.liveController});

  @override
  AudienceWaitingPassWidgetState getState() {
    return AudienceWaitingPassWidgetState();
  }
}

class AudienceWaitingPassWidgetState extends BasicState<AudienceWaitingPassWidget> {
  final ValueNotifier<String> _text = ValueNotifier<String>("");
  int _index = 0;
  late final Timer _timer;

  @override
  void initState() {
    super.initState();
    _startTimer();
  }

  @override
  void dispose() {
    _timer.cancel();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return ValueListenableBuilder(
      valueListenable: liveController.getViewState().linkStatus,
      builder: (BuildContext context, LinkStatus value, Widget? child) {
        return Visibility(
          visible: liveController.getViewState().linkStatus.value == LinkStatus.applying,
          child: GestureDetector(
            onTap: () {
              _showCancelRequestPanelWidget();
            },
            child: Container(
              width: 86,
              height: 86,
              decoration: BoxDecoration(
                color: LivekitColors.livekitDesignStandardG2,
                border: Border.all(color: LivekitColors.livekitNotStandardWhite20Transparency, width: 1),
                borderRadius: const BorderRadius.all(Radius.circular(10)),
              ),
              child: Column(
                mainAxisAlignment: MainAxisAlignment.start,
                crossAxisAlignment: CrossAxisAlignment.center,
                children: [
                  14.verticalSpace,
                  _initUserAvatarWidget(),
                  6.verticalSpace,
                  _initWaitingPassTextWidget(),
                ],
              ),
            ),
          ),
        );
      },
    );
  }

  Widget _initUserAvatarWidget() {
    return Container(
      width: 36,
      height: 36,
      decoration: BoxDecoration(
        color: LivekitColors.livekitDesignStandardG2,
        borderRadius: BorderRadius.circular(18),
      ),
      child: ClipOval(
        child: Image.network(
          liveController.getUserState().selfInfo.avatarUrl.value ?? "",
          fit: BoxFit.cover,
          errorBuilder: (context, error, stackTrace) {
            return Image.asset(
              LivekitImages.livekitDefaultAvatar,
              package: Constants.pluginName,
            );
          },
        ),
      ),
    );
  }

  _initWaitingPassTextWidget() {
    return ValueListenableBuilder(
      valueListenable: _text,
      builder: (BuildContext context, value, Widget? child) {
        return Container(
          margin: const EdgeInsets.only(top: 3),
          child: Text(
            _text.value,
            style: const TextStyle(color: LivekitColors.livekitDesignStandardFlowkitWhite, fontSize: 14),
          ),
        );
      },
    );
  }
}

extension AudienceWaitingPassWidgetStateLogicExtension on AudienceWaitingPassWidgetState {
  void _showCancelRequestPanelWidget() {
    List<ActionSheetModel> list = [
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.livekit_text_cancel_link_mic_apply,
          textStyle: const TextStyle(
            color: LivekitColors.livekitDesignStandardFlowkitRed,
            fontSize: 16,
            fontWeight: FontWeight.w700,
          ),
          lineHeight: 3,
          bingData: 1),
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!.livekit_cancel,
          isShowBottomLine: false,
          bingData: 2),
    ];
    ActionSheet.show(list, (ActionSheetModel model) async {
      if (model.bingData == 1) {
        liveController.seatController.cancelTakeSeatApplication();
      }
    });
  }

  void _startTimer() {
    _timer = Timer.periodic(const Duration(milliseconds: 600), (timer) {
      _index++;
      if (_index > 3) {
        _index = 0;
      }
      String text = LiveKitLocalizations.of(Global.appContext())!.livekit_waiting_pass;
      for (int i = 0; i < _index; i++) {
        text = "$text.";
      }
      _text.value = text;
    });
  }
}
