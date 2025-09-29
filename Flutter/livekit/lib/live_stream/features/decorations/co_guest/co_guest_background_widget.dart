import 'package:flutter/material.dart';
import 'package:live_stream_core/live_core_widget/live_core_controller.dart';
import 'package:rtc_room_engine/api/room/tui_room_define.dart';
import 'package:tencent_live_uikit/common/constants/index.dart';
import 'package:tencent_live_uikit/common/resources/colors.dart';
import 'package:tencent_live_uikit/common/resources/images.dart';
import 'package:tencent_live_uikit/common/screen/index.dart';

class CoGuestBackgroundWidget extends StatefulWidget {
  final SeatFullInfo userInfo;
  final LiveCoreController liveCoreController;

  const CoGuestBackgroundWidget({
    super.key,
    required this.userInfo,
    required this.liveCoreController,
  });

  @override
  State<CoGuestBackgroundWidget> createState() => _CoGuestWidgetState();
}

class _CoGuestWidgetState extends State<CoGuestBackgroundWidget> {
  @override
  Widget build(BuildContext context) {
    return Container(
        decoration:
            BoxDecoration(color: LiveColors.grayDark2, border: Border.all(color: LiveColors.black6, width: 0.5)),
        child: Stack(
          alignment: Alignment.center,
          children: [
            _buildAvatarWidget(),
          ],
        ));
  }

  _buildAvatarWidget() {
    return Center(
      child: SizedBox(
        width: 45.width,
        height: 45.width,
        child: ClipOval(
          child: Image.network(
            widget.userInfo.userAvatar,
            errorBuilder: (context, error, stackTrace) {
              return Image.asset(
                LiveImages.defaultAvatar,
                package: Constants.pluginName,
              );
            },
          ),
        ),
      ),
    );
  }
}
