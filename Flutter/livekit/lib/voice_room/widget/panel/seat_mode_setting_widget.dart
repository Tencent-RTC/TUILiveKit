import 'package:flutter/material.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/voice_room/index.dart';

import '../../../common/index.dart';

class SeatModeSettingPanelWidget extends StatefulWidget {
  final VoiceRoomManager manager;

  const SeatModeSettingPanelWidget({super.key, required this.manager});

  @override
  State<SeatModeSettingPanelWidget> createState() =>
      _SeatModeSettingPanelWidgetState();
}

class _SeatModeSettingPanelWidgetState
    extends State<SeatModeSettingPanelWidget> {
  late final VoiceRoomManager manager;

  @override
  void initState() {
    super.initState();
    manager = widget.manager;
  }

  @override
  Widget build(BuildContext context) {
    final double screenWidth = MediaQuery.of(context).size.width;
    return SizedBox(
      width: screenWidth,
      height: 300,
      child: Column(children: [
        SizedBox(height: context.adapter.getHeight(20)),
        SizedBox(
          height: context.adapter.getHeight(44),
          width: screenWidth,
          child: Stack(
            children: [
              Positioned(
                  left: context.adapter.getWidth(14),
                  child: GestureDetector(
                      onTap: () {
                        Navigator.pop(context);
                      },
                      child: Container(
                          width: context.adapter.getWidth(44),
                          height: context.adapter.getWidth(44),
                          padding: EdgeInsets.all(context.adapter.getWidth(10)),
                          child: Image.asset(
                            LiveImages.returnArrow,
                            package: Constants.pluginName,
                          )))),
              Center(
                child: Text(
                  LiveKitLocalizations.of(Global.appContext())!
                      .live_settings,
                  style: const TextStyle(
                      color: LiveColors.designStandardFlowkitWhite,
                      fontSize: 16),
                ),
              ),
            ],
          ),
        ),
        Row(
          children: [
            Padding(
              padding: EdgeInsets.all(context.adapter.getWidth(16)),
              child: Text(
                  LiveKitLocalizations.of(Global.appContext())!
                      .live_voiceroom_need_agree,
                  style: const TextStyle(
                      fontSize: 16,
                      color: LiveColors.designStandardFlowkitWhite)),
            ),
            Expanded(
                child: Align(
              alignment: Alignment.centerRight,
              child: Padding(
                padding: const EdgeInsets.only(right: 16),
                child: ValueListenableBuilder(
                    valueListenable: manager.roomState.seatMode,
                    builder: (context, seatMode, child) {
                      return Switch(
                          activeTrackColor: Colors.blue,
                          value: seatMode == TUISeatMode.applyToTake,
                          onChanged: (opened) {
                            final mode = opened
                                ? TUISeatMode.applyToTake
                                : TUISeatMode.freeToTake;
                            manager.onRoomSeatModeChanged(mode);
                          });
                    }),
              ),
            ))
          ],
        )
      ]),
    );
  }
}
