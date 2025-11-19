import 'dart:io';

import 'package:flutter/material.dart';
import 'package:fluttertoast/fluttertoast.dart';
import 'package:tencent_live_uikit/common/resources/index.dart';

Future<bool?> makeToast({
  required String msg,
  Color backgroundColor = LiveColors.black80Transparency,
  Color textColor = Colors.white,
}) {
  return Fluttertoast.showToast(
    msg: msg,
    toastLength: Toast.LENGTH_SHORT,
    gravity: ToastGravity.CENTER,
    timeInSecForIosWeb: 1,
    backgroundColor: Platform.isAndroid ? null : backgroundColor,
    textColor: Platform.isAndroid ? null : textColor,
    fontSize: 16,
  );
}
