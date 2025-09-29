import 'package:flutter/material.dart';
import 'package:live_uikit_gift/common/screen/screen_adapter.dart';

import '../../../common/index.dart';
import 'like_send_controller.dart';

class LikeSendWidget extends StatelessWidget {
  final LikeSendController controller;

  const LikeSendWidget({super.key, required this.controller});

  @override
  Widget build(BuildContext context) {
    return IconButton(onPressed: () => controller.sendLike(),
        padding: EdgeInsets.zero,
        icon: Image.asset(
      GiftImages.giftLikeSendIcon,
      package: Constants.pluginName,
      fit: BoxFit.fill,
    ));
  }
}
