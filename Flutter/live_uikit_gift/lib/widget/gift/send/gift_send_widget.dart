import 'package:flutter/material.dart';

import '../../../common/index.dart';
import 'gift_list_widget.dart';
import 'gift_list_controller.dart';

class GiftSendWidget extends StatelessWidget {
  final GiftListController controller;

  const GiftSendWidget({super.key, required this.controller});

  @override
  Widget build(BuildContext context) {
    return IconButton(onPressed: () => showGiftPanelWidget(context, controller),
        padding: EdgeInsets.zero,
        icon: Image.asset(
          GiftImages.giftSendIcon,
          package: Constants.pluginName,
          fit: BoxFit.fill,
        ));
  }

  void showGiftPanelWidget(BuildContext context, GiftListController controller) {
    showModalBottomSheet(
      context: context,
      barrierColor: Colors.transparent,
      builder: (context) =>
          GiftListWidget(
            giftListController: controller,
          ),
    );
  }
}
