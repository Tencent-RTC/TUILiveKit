import 'dart:math';

import 'package:barrage/common/index.dart';
import 'package:barrage/widget/send/index.dart';
import 'package:flutter/material.dart';

class EmojiPanelWidget extends StatelessWidget {
  final BarrageSendController controller;

  const EmojiPanelWidget({super.key, required this.controller});

  @override
  Widget build(BuildContext context) {
    return LayoutBuilder(
      builder: (context, constraints) {
        final panelWidth = MediaQuery.of(context).size.width;
        final panelHeight = controller.getInputKeyboardHeight(context);
        const emojiSize = Size(35, 35);
        final emojiCountPerRow = (panelWidth / emojiSize.width).floor();
        final emojiHorizontalSpacing = (panelWidth - emojiCountPerRow * emojiSize.width) / (emojiCountPerRow - 1);
        Orientation orientation = MediaQuery.of(context).orientation;
        return GestureDetector(
          behavior: HitTestBehavior.translucent,
          onTap: () {},
          child: Container(
            width: panelWidth,
            height: panelHeight,
            color: BarrageColors.barrageColorDark,
            padding: EdgeInsets.symmetric(horizontal: orientation == Orientation.portrait ? 16 : 52),
            child: Stack(
              alignment: AlignmentDirectional.topCenter,
              children: [
                SingleChildScrollView(
                  scrollDirection: Axis.vertical,
                  child: Column(
                    children: [
                      const SizedBox(height: 10),
                      Wrap(
                        spacing: max(emojiHorizontalSpacing, 16),
                        runSpacing: 16,
                        children: BarrageEmoji.emojiMap.keys.map((emoji) {
                          return GestureDetector(
                            onTap: () {
                              controller.inputEmoji(BarrageEmoji.emojiMap[emoji] ?? '');
                            },
                            behavior: HitTestBehavior.opaque,
                            child: SizedBox(
                              width: emojiSize.width,
                              height: emojiSize.height,
                              child: Image.asset(
                                emoji,
                                package: Constants.pluginName,
                                fit: BoxFit.fill,
                              ),
                            ),
                          );
                        }).toList(),
                      ),
                      const SizedBox(height: 50.0),
                    ],
                  ),
                ),
                _buildDeleteButton(),
              ],
            ),
          ),
        );
      },
    );
  }

  Widget _buildDeleteButton() {
    return Positioned(
      width: 35,
      height: 30,
      bottom: 40,
      right: 5,
      child: Container(
        decoration: BoxDecoration(
          color: Colors.white,
          borderRadius: BorderRadius.circular(4.0),
        ),
        child: InkWell(
          onTap: () {
            controller.deleteCharacter();
          },
          child: Image.asset(
            BarrageImages.deleteIcon,
            package: Constants.pluginName,
            fit: BoxFit.fill,
          ),
        ),
      ),
    );
  }
}
