import 'package:extended_text_field/extended_text_field.dart';
import 'package:flutter/material.dart';

import '../../common/index.dart';
import '../../state/index.dart';
import '../send/index.dart';
import '../emoji/index.dart';

class BarrageInputPanelWidget extends StatelessWidget {
  final BarrageSendController controller;

  const BarrageInputPanelWidget({super.key, required this.controller});

  @override
  Widget build(BuildContext context) {
    controller.setInputKeyboardHeight(MediaQuery.viewInsetsOf(context).bottom);
    return SizedBox(
      width: MediaQuery.sizeOf(context).width,
      height: controller.getInputKeyboardHeight(context) + 60,
      child: Column(
        mainAxisAlignment: MainAxisAlignment.end,
        children: [
          _buildTopBarWidget(context),
          _buildPanelWidget(context),
        ],
      ),
    );
  }

  Widget _buildTopBarWidget(BuildContext context) {
    Orientation orientation = MediaQuery.orientationOf(context);
    return Container(
      width: MediaQuery.sizeOf(context).width,
      height: 60,
      padding: EdgeInsets.symmetric(
          horizontal: orientation == Orientation.portrait ? 16 : 52,
          vertical: 12),
      color: BarrageColors.barrageG2,
      child: Row(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          _buildEmojiButton(),
          const SizedBox(width: 10),
          _buildTextField(context),
          const SizedBox(width: 10),
          _buildSendButton(context),
        ],
      ),
    );
  }

  _buildPanelWidget(BuildContext context) {
    return ValueListenableBuilder(
        valueListenable: controller.showEmojiPanel,
        builder: (BuildContext context, bool value, Widget? child) {
          return controller.showEmojiPanel.value
              ? EmojiPanelWidget(controller: controller)
              : Container(
                  color: BarrageColors.barrageColorDark,
                  height: controller.getInputKeyboardHeight(context),
                );
        });
  }

  Widget _buildEmojiButton() {
    return GestureDetector(
      onTap: () {
        controller.toggleEmojiType();
      },
      child: ValueListenableBuilder(
        valueListenable: controller.showEmojiPanel,
        builder: (BuildContext context, bool value, Widget? child) {
          return Image.asset(
            controller.showEmojiPanel.value
                ? BarrageImages.keyboardIcon
                : BarrageImages.emojiIcon,
            width: 24,
            height: 24,
            fit: BoxFit.fill,
            package: Constants.pluginName,
          );
        },
      ),
    );
  }

  Widget _buildTextField(BuildContext context) {
    return Expanded(
      child: SizedBox(
        height: 36,
        child: ExtendedTextField(
          key: controller.textFieldKey,
          controller: controller.textEditingController,
          style: const TextStyle(
            color: Colors.white,
            fontSize: 12,
          ),
          autofocus: true,
          maxLines: null,
          specialTextSpanBuilder: EmojiTextSpanBuilder(context: context),
          cursorColor: BarrageColors.barrageFlowkitGreen,
          textInputAction: TextInputAction.send,
          focusNode: controller.focusNode,
          decoration: InputDecoration(
            hintText: BarrageLocalizations.of(context)!.barrage_let_us_chat,
            hintStyle: const TextStyle(
              color: BarrageColors.barrageTextGrey,
              fontSize: 12,
            ),
            fillColor: BarrageColors.barrage40G1,
            filled: true,
            enabledBorder: OutlineInputBorder(
              borderRadius: BorderRadius.circular(20),
            ),
            focusedBorder: OutlineInputBorder(
              borderRadius: BorderRadius.circular(20),
              borderSide: const BorderSide(color: Colors.transparent),
            ),
            contentPadding:
                const EdgeInsets.symmetric(horizontal: 18, vertical: 2),
          ),
          onTap: () {
            controller.showEmojiPanel.value = false;
          },
          onSubmitted: (value) async {
            final result = await controller.sendBarrage(_generateSendBarrage());
            if (true == result) {
              controller.textEditingController.clear();
              Navigator.pop(context);
            }
          },
        ),
      ),
    );
  }

  Widget _buildSendButton(BuildContext context) {
    return SizedBox(
      width: 60,
      height: 36,
      child: ElevatedButton(
        onPressed: () async {
          final result = await controller.sendBarrage(_generateSendBarrage());
          if (true == result) {
            controller.textEditingController.clear();
            Navigator.pop(context);
          }
        },
        style: ButtonStyle(
          backgroundColor:
              WidgetStateProperty.all(BarrageColors.barrageFlowkitGreen),
          padding: WidgetStateProperty.all(EdgeInsets.zero),
          shape: WidgetStateProperty.all(
            RoundedRectangleBorder(
              borderRadius: BorderRadius.circular(18),
            ),
          ),
        ),
        child: Text(
          BarrageLocalizations.of(context)!.barrage_send,
          style: const TextStyle(color: Colors.white, fontSize: 16),
        ),
      ),
    );
  }

  Barrage _generateSendBarrage() {
    BarrageUser user = BarrageUser();
    user.userId = BarrageStore().selfUserId;
    user.userName = BarrageStore().selfName;
    user.level = "32";
    Barrage barrageModel = Barrage();
    barrageModel.content = controller.textEditingController.text;
    barrageModel.user = user;
    return barrageModel;
  }
}
