import 'package:flutter/material.dart';

import '../../../../common/resources/colors.dart';
import '../../../../tencent_live_uikit.dart';

class AnchorEmptySeatWidget extends StatelessWidget {
  final SeatFullInfo seatFullInfo;

  const AnchorEmptySeatWidget({super.key, required this.seatFullInfo});

  @override
  Widget build(BuildContext context) {
    return Container(
      alignment: Alignment.center,
      decoration: BoxDecoration(color: LiveColors.grayDark2, border: Border.all(color: LiveColors.black6, width: 0.5)),
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          const Spacer(flex: 1),
          Text(
            seatFullInfo.seatIndex.toString(),
            style: const TextStyle(color: LiveColors.designStandardFlowkitWhite, fontSize: 18),
          ),
          const Spacer(flex: 1),
          Text(
            LiveKitLocalizations.of(context)!.common_wait_connection,
            style: const TextStyle(color: LiveColors.designStandardFlowkitWhite, fontSize: 14),
          ),
          const Spacer(flex: 1),
        ],
      ),
    );
  }
}
