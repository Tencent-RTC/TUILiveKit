import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/live_stream/manager/live_controller.dart';

abstract class BasicWidget<T extends State> extends StatefulWidget {
  final LiveController liveController;

  /// Dependency injection currently uses the constructor injection, which can be optimized later
  const BasicWidget({super.key, required this.liveController});

  @override
  State createState() {
    return getState();
  }

  T getState();
}

abstract class BasicState<T extends StatefulWidget> extends State<T> {
  late final LiveController liveController;
  late final double screenWidth = MediaQuery.of(context).size.width;
  late final double screenHeight = MediaQuery.of(context).size.height;

  @override
  void initState() {
    super.initState();
    liveController = (widget as BasicWidget).liveController;
  }

  @override
  void dispose() {
    super.dispose();
  }
}

extension SizeExtension on num {
  SizedBox setVerticalSpacing(num height) => SizedBox(height: height.toDouble());

  setHorizontalSpacing(num width) => SizedBox(width: width.toDouble());

  SizedBox get verticalSpace => setVerticalSpacing(this);

  SizedBox get horizontalSpace => setHorizontalSpacing(this);
}

void showWidget(BasicWidget widget, {Color? barrierColor}) {
  showModalBottomSheet(
    barrierColor: barrierColor,
    isScrollControlled: true,
    context: Global.appContext(),
    builder: (context) => Container(
      decoration: const BoxDecoration(
        borderRadius: BorderRadius.only(
          topLeft: Radius.circular(20),
          topRight: Radius.circular(20),
        ),
        color: LiveColors.designStandardG2,
      ),
      child: widget,
    ),
  );
}
