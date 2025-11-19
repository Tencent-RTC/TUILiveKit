import 'package:extended_text_field/extended_text_field.dart';
import 'package:flutter/material.dart';
import 'package:flutter/scheduler.dart';

import '../../common/index.dart';
import '../../state/index.dart';

class BarrageSendController {
  ValueNotifier<bool> isFloatWindowMode = ValueNotifier(false);
  ValueNotifier<bool> showEmojiPanel = ValueNotifier(false);
  FocusNode focusNode = FocusNode();
  final textEditingController = TextEditingController();
  final GlobalKey<ExtendedTextFieldState> textFieldKey = GlobalKey<ExtendedTextFieldState>();
  double _inputKeyboardHeight = 0;

  BarrageSendController(
      {required String roomId,
      required String ownerId,
      required String selfUserId,
      String? selfName}) {
    BarrageStore().manager.init(roomId, ownerId, selfUserId, selfName);
    focusNode.addListener(_handleFocusChange);
  }

  void setFloatWindowMode(bool isFloatWindow) {
    isFloatWindowMode.value = isFloatWindow;
  }

  Future<bool> sendBarrage(Barrage barrage) async {
    return BarrageStore().manager.sendBarrage(barrage);
  }

  void setInputKeyboardHeight(double height) {
    if (height > _inputKeyboardHeight) {
      _inputKeyboardHeight = height;
    }
  }

  double getInputKeyboardHeight(BuildContext context) {
    return _inputKeyboardHeight > 0
        ? _inputKeyboardHeight
        : MediaQuery.viewInsetsOf(context).bottom;
  }

  void inputEmoji(String displayText) {
    final TextEditingValue value = textEditingController.value;
    final int start = value.selection.start;
    int end = value.selection.end;
    if (value.selection.isValid) {
      String newText = '';
      if (value.selection.isCollapsed) {
        if (end > 0) {
          newText += value.text.substring(0, end);
        }
        newText += displayText;
        if (value.text.length > end) {
          newText += value.text.substring(end, value.text.length);
        }
      } else {
        newText = value.text.replaceRange(start, end, displayText);
        end = start;
      }

      textEditingController.value = value.copyWith(
          text: newText,
          selection: value.selection.copyWith(
              baseOffset: end + displayText.length,
              extentOffset: end + displayText.length));
    } else {
      textEditingController.value = TextEditingValue(
        text: displayText,
        selection: TextSelection.fromPosition(
          TextPosition(offset: displayText.length),
        ),
      );
    }

    SchedulerBinding.instance.addPostFrameCallback((Duration timeStamp) {
      textFieldKey.currentState
          ?.bringIntoView(textEditingController.selection.base);
    });
  }

  void toggleEmojiType() {
    if (focusNode.hasFocus) {
      showEmojiPanel.value = true;
      focusNode.unfocus();
    } else {
      showEmojiPanel.value = false;
      focusNode.requestFocus();
    }
  }

  void deleteCharacter() {
    String text = textEditingController.text;
    TextSelection selection = textEditingController.selection;

    if (selection.isCollapsed) {
      int cursorPosition = selection.baseOffset;
      if (cursorPosition <= 0 || cursorPosition > text.length) {
        return;
      }

      int start = cursorPosition;
      int end = cursorPosition;

      if (start > 0 && text[start - 1] == ']') {
        start = _findEmojiStart(text, start);
        if (start >= 0 && _isValidEmoji(text, start, end)) {
          _deleteText(text, start, end);
          return;
        }
      }

      start = cursorPosition;
      if (start > 0) {
        int deleteLength = _isUTF16(text, cursorPosition - 2) ? 2 : 1;
        start -= deleteLength;
        _deleteText(text, start, end);
      }
    } else {
      _deleteText(text, selection.start, selection.end);
    }
  }

  int _findEmojiStart(String text, int start) {
    while (start > 0 && text[start - 1] != '[') {
      start--;
    }
    if (start > 0 && text[start - 1] == '[') {
      start--;
    }
    return start;
  }

  bool _isValidEmoji(String text, int start, int end) {
    String selectText = text.substring(start, end);
    return text[start] == '[' &&
        text[end - 1] == ']' &&
        BarrageEmoji.emojiMap.containsValue(selectText);
  }

  void _deleteText(String text, int start, int end) {
    String newText = text.replaceRange(start, end, '');
    textEditingController.value = TextEditingValue(
      text: newText,
      selection: TextSelection.collapsed(offset: start),
    );
  }

  bool _isUTF16(String text, int index) {
    if (index < 0 || index + 1 >= text.length) {
      return false;
    }
    int first = text.codeUnitAt(index);
    int second = text.codeUnitAt(index + 1);
    return first >= 0xD800 &&
        first <= 0xDBFF &&
        second >= 0xDC00 &&
        second <= 0xDFFF;
  }

  void _handleFocusChange() {
    if (focusNode.hasFocus) {
      showEmojiPanel.value = false;
    }
  }
}
