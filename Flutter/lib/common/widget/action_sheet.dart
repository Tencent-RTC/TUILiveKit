import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

class ActionSheet {
  static void show(List<ActionSheetModel> list, Function(ActionSheetModel) clickBlock) {
    double bottomHeight = MediaQuery.of(Global.appContext()).padding.bottom;
    double actionSheetHeight = list.fold(0.0, (sum, item) => sum + item.cellHeight);
    debugPrint("showActionSheet:$actionSheetHeight");
    showModalBottomSheet(
      context: Global.appContext(),
      builder: (context) => Container(
        decoration: const BoxDecoration(
          borderRadius: BorderRadius.only(
            topLeft: Radius.circular(20),
            topRight: Radius.circular(20),
          ),
          color: LivekitColors.livekitDesignStandardG2,
        ),
        height: actionSheetHeight + bottomHeight,
        child: ListView.builder(
          itemCount: list.length,
          itemBuilder: (context, index) {
            return _buildCell(list[index], clickBlock);
          },
        ),
      ),
    );
  }

  static GestureDetector _buildCell(ActionSheetModel model, Function(ActionSheetModel) clickBlock) {
    return GestureDetector(
      onTap: () {
        clickBlock(model);
        Navigator.of(Global.appContext()).pop();
      },
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        crossAxisAlignment: CrossAxisAlignment.center,
        children: [
          Container(
            height: model.cellHeight - model.lineHeight,
            color: Colors.transparent,
            child: Center(
              child: Row(
                mainAxisSize: model.isCenter ? MainAxisSize.min : MainAxisSize.max,
                children: [
                  Visibility(
                    visible: model.icon.isNotEmpty,
                    child: const SizedBox(
                      width: 14,
                    ),
                  ),
                  Visibility(
                    visible: model.icon.isNotEmpty,
                    child: Image.asset(
                      model.icon,
                      width: 20,
                      height: 20,
                      fit: BoxFit.cover,
                      package: Constants.pluginName,
                    ),
                  ),
                  Visibility(
                    visible: model.icon.isNotEmpty,
                    child: const SizedBox(
                      width: 14,
                    ),
                  ),
                  Text(
                    model.text,
                    style: model.textStyle,
                  ),
                ],
              ),
            ),
          ),
          Container(
            height: model.lineHeight,
            color: model.isShowBottomLine ? model.lineColor : Colors.transparent,
          ),
        ],
      ),
    );
  }
}

class ActionSheetModel {
  final String text;
  final TextStyle textStyle;
  final double cellHeight;
  final bool isShowBottomLine;
  final Color lineColor;
  final double lineHeight;
  final String icon;
  final bool isCenter;
  dynamic bingData;

  ActionSheetModel({
    required this.text,
    this.cellHeight = 55,
    this.textStyle = const TextStyle(
      color: LivekitColors.livekitDesignStandardFlowkitWhite,
      fontSize: 16,
      fontWeight: FontWeight.w700,
    ),
    this.isShowBottomLine = true,
    this.lineColor = LivekitColors.livekitDesignStandardG3Divider,
    this.lineHeight = 1,
    this.icon = "",
    this.isCenter = true,
    this.bingData = "",
  });
}
