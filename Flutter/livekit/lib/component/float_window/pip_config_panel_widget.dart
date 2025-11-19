import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

class PipConfigPanelWidget extends StatefulWidget {
  final ValueListenable<bool> enablePipMode;
  final ValueChanged<bool> onChanged;

  const PipConfigPanelWidget({super.key, required this.enablePipMode, required this.onChanged});

  @override
  State<StatefulWidget> createState() {
    return _PipConfigPanelWidget();
  }
}

class _PipConfigPanelWidget extends State<PipConfigPanelWidget> {
  @override
  Widget build(BuildContext context) {
    return Container(
      width: 1.screenWidth,
      height: 200.height,
      padding: EdgeInsets.all(10.radius),
      decoration: BoxDecoration(
        color: LiveColors.designStandardG2,
        borderRadius: BorderRadius.only(topLeft: Radius.circular(20.width), topRight: Radius.circular(20.width)),
      ),
      child: Column(children: [
        Divider(
          height: 20.height,
          color: LiveColors.designStandardTransparent,
        ),
        Text(LiveKitLocalizations.of(Global.appContext())!.common_video_settings_item_pip,
            style: const TextStyle(color: LiveColors.designStandardG7, fontSize: 16)),
        Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          crossAxisAlignment: CrossAxisAlignment.center,
          children: [
            Text(
              LiveKitLocalizations.of(Global.appContext())!.common_pip_toggle,
              style: const TextStyle(color: LiveColors.notStandardWhite, fontSize: 16),
            ),
            ValueListenableBuilder(
              valueListenable: widget.enablePipMode,
              builder: (context, enablePipMode, child) {
                return Switch(
                    activeColor: LiveColors.notStandardWhite,
                    activeTrackColor: LiveColors.designStandardB1,
                    value: enablePipMode,
                    onChanged: widget.onChanged);
              },
            ),
          ],
        ),
        Align(
          alignment: Alignment.topLeft,
          child: Text(
            textAlign: TextAlign.left,
            LiveKitLocalizations.of(Global.appContext())!.common_pip_toggle_description,
            style: const TextStyle(color: LiveColors.notStandardWhite, fontSize: 12),
          ),
        ),
        Divider(
          height: 20.height,
          color: LiveColors.designStandardTransparent,
        ),
      ]),
    );
  }
}
