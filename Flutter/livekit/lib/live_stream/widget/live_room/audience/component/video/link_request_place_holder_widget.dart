import 'dart:async';

import 'package:flutter/material.dart';

import '../../../../../../common/index.dart';

class LinkRequestPlaceHolderWidget extends BasicWidget {
  const LinkRequestPlaceHolderWidget({super.key, required super.liveController});

  @override
  LinkRequestPlaceHolderWidgetState getState() {
    return LinkRequestPlaceHolderWidgetState();
  }
}

class LinkRequestPlaceHolderWidgetState extends BasicState<LinkRequestPlaceHolderWidget> {
  final ValueNotifier<String> dotText = ValueNotifier("");
  static const int maxDotCount = 3;
  static const int minDotCount = 1;
  int dotCount = minDotCount;
  late Timer timer;

  @override
  void initState() {
    super.initState();
    initTimer();
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      width: double.infinity,
      color: LiveColors.designStandardG2,
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [_initLinkWaitIconWidget(), _initLinkWaitAnimationWidget()],
      ),
    );
  }

  @override
  void dispose() {
    timer.cancel();
    super.dispose();
  }

  _initLinkWaitIconWidget() {
    return SizedBox(
      width: 45,
      height: 45,
      child: Image.asset(
        LiveImages.linkWaiting,
        package: Constants.pluginName,
      ),
    );
  }

  _initLinkWaitAnimationWidget() {
    return ValueListenableBuilder(
      valueListenable: dotText,
      builder: (BuildContext context, String value, Widget? child) {
        return Container(
            margin: const EdgeInsets.only(top: 2),
            child: Text("${LiveKitLocalizations.of(Global.appContext())!.live_waiting_link}${dotText.value}",
                style: const TextStyle(color: LiveColors.notStandardGrey, fontSize: 12)));
      },
    );
  }

  void initTimer() {
    timer = Timer.periodic(const Duration(seconds: 1), (timer) {
      StringBuffer sb = StringBuffer();
      for (int i = 0; i < dotCount; i++) {
        sb.write(".");
      }
      dotCount++;
      if (dotCount > maxDotCount) {
        dotCount = minDotCount;
      }
      dotText.value = sb.toString();
    });
  }
}

extension LinkRequestPlaceHolderWidgetStateLogicExtension on LinkRequestPlaceHolderWidgetState {}
