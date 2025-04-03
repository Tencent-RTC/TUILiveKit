import 'package:flutter/material.dart';

import '../../../common/index.dart';
import '../../../state/index.dart';
import 'gift_panel_widget.dart';
import 'gift_send_controller.dart';

class GiftSendWidget extends StatelessWidget {
  final GiftSendController controller;

  GiftSendWidget({super.key, required this.controller}) {
    GiftStore().giftManager.getGiftData();
  }

  @override
  Widget build(BuildContext context) {
    return ElevatedButton(
      onPressed: () async {
        showGiftPanelWidget(context, controller);
      },
      style: ButtonStyle(
        padding: WidgetStateProperty.all(
          const EdgeInsets.all(0),
        ),
        backgroundColor:
            WidgetStateProperty.all<Color>(GiftColors.giftLightGrey),
        shape: WidgetStateProperty.all(
          RoundedRectangleBorder(
            borderRadius: BorderRadius.circular(18),
          ),
        ),
      ),
      child: Image.asset(
        GiftImages.giftSendIcon,
        package: Constants.pluginName,
        fit: BoxFit.fill,
      ),
    );
  }

  void showGiftPanelWidget(
      BuildContext context, GiftSendController controller) {
    showModalBottomSheet(
      context: context,
      barrierColor: Colors.transparent,
      builder: (context) => GiftPanelWidget(
        controller: controller,
      ),
    );
  }
}
