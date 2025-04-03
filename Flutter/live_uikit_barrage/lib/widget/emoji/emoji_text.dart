import 'package:extended_text_field/extended_text_field.dart';
import 'package:flutter/material.dart';

import '../../common/index.dart';

class EmojiTextSpanBuilder extends SpecialTextSpanBuilder {
  EmojiTextSpanBuilder({required this.context});

  final BuildContext context;

  @override
  SpecialText? createSpecialText(String flag,
      {TextStyle? textStyle,
      SpecialTextGestureTapCallback? onTap,
      int? index}) {
    if (flag == '') {
      return null;
    }
    if (flag.endsWith('[')) {
      return EmojiText(textStyle, start: index! - (EmojiText.flag.length - 1));
    }
    return null;
  }
}

class EmojiText extends SpecialText {
  EmojiText(TextStyle? textStyle, {this.start})
      : super(EmojiText.flag, ']', textStyle);
  static const String flag = '[';
  final int? start;

  @override
  InlineSpan finishText() {
    final String _key = toString();
    final String imagePath = BarrageEmoji.emojiMap.keys.firstWhere(
        (key) => BarrageEmoji.emojiMap[key] == _key,
        orElse: () => '');
    if (imagePath.isNotEmpty) {
      return ImageSpan(
        AssetImage(imagePath, package: Constants.pluginName),
        imageWidth: 20.0,
        imageHeight: 20.0,
        start: start!,
        actualText: _key,
      );
    }
    return TextSpan(text: toString(), style: textStyle);
  }
}
