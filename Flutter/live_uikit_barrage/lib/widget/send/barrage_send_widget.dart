import 'package:flutter/material.dart';

import '../../common/index.dart';
import 'barrage_input_panel_widget.dart';
import 'barrage_send_controller.dart';

class BarrageSendWidget extends StatelessWidget {
  final BarrageSendController controller;

  const BarrageSendWidget({super.key, required this.controller});

  @override
  Widget build(BuildContext context) {
    return SizedBox(
      height: 36,
      child: ElevatedButton(
        onPressed: () async {
          showInputWidget(context, controller);
        },
        style: ButtonStyle(
          padding: WidgetStateProperty.all(
            const EdgeInsets.all(0),
          ),
          backgroundColor:
              WidgetStateProperty.all<Color>(BarrageColors.barrageLightGrey),
          shape: WidgetStateProperty.all(
            RoundedRectangleBorder(
              borderRadius: BorderRadius.circular(18),
            ),
          ),
        ),
        child: Row(
          mainAxisAlignment: MainAxisAlignment.spaceAround,
          children: [
            Text(
              BarrageLocalizations.of(context)!.barrage_let_us_chat,
              style: const TextStyle(
                fontSize: 12,
                fontWeight: FontWeight.w400,
                color: BarrageColors.barrageTextGrey,
                overflow: TextOverflow.ellipsis,
              ),
            ),
            Image.asset(
              BarrageImages.emojiIcon,
              package: Constants.pluginName,
              width: 18.33,
              height: 18.33,
            ),
          ],
        ),
      ),
    );
  }

  void showInputWidget(BuildContext context, BarrageSendController controller) {
    showModalBottomSheet(
      context: context,
      barrierColor: Colors.transparent,
      builder: (context) => BarrageInputPanelWidget(controller: controller),
    );
  }
}
