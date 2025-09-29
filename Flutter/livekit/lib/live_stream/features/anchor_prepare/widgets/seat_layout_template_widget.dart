import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/live_stream/features/anchor_prepare/anchor_preview_widget_define.dart';

import '../../../live_define.dart';

class SeatLayoutTemplateWidget extends StatelessWidget {
  final EditInfo editInfo;

  const SeatLayoutTemplateWidget({super.key, required this.editInfo});

  @override
  Widget build(BuildContext context) {
    return Container(
        padding: EdgeInsets.all(20.radius),
        width: double.infinity,
        child: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              crossAxisAlignment: CrossAxisAlignment.center,
              children: [
                IconButton(
                    onPressed: () {
                      Navigator.pop(context);
                    },
                    color: LiveColors.whiteAlphaE6,
                    icon: Icon(Icons.arrow_back_ios)),
                Padding(
                    padding: EdgeInsets.only(top: 10.height, bottom: 10.height),
                    child: Text(LiveKitLocalizations.of(context)!.common_template_layout_settings,
                        style:
                            const TextStyle(color: LiveColors.whiteAlphaE6, fontSize: 16, fontWeight: FontWeight.bold))),
                IconButton(onPressed: () {}, color: Colors.transparent, icon: const Icon(null)),
              ],
            ),
            Row(children: [
              Padding(
                  padding: EdgeInsets.only(top: 10.height, bottom: 10.height),
                  child: Text(LiveKitLocalizations.of(context)!.common_template_layout_co_host,
                      style: const TextStyle(color: LiveColors.whiteAlpha8C, fontSize: 14)))
            ]),
            Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                _buildTemplateGrid(context, true, LiveTemplateMode.verticalDynamicGrid),
                _buildTemplateGrid(context, true, LiveTemplateMode.verticalDynamicFloat),
              ],
            ),
            Row(children: [
              Padding(
                  padding: EdgeInsets.only(top: 10.height, bottom: 10.height),
                  child: Text(LiveKitLocalizations.of(context)!.common_template_layout_co_guest,
                      style: const TextStyle(color: LiveColors.whiteAlpha8C, fontSize: 14)))
            ]),
            Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                _buildTemplateGrid(context, false, LiveTemplateMode.verticalDynamicGrid),
                _buildTemplateGrid(context, false, LiveTemplateMode.verticalDynamicFloat),
              ],
            ),
            SizedBox.fromSize(size: Size(0, 20.height)),
            Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                _buildTemplateGrid(context, false, LiveTemplateMode.verticalStaticGrid),
                _buildTemplateGrid(context, false, LiveTemplateMode.verticalStaticFloat),
              ],
            ),
          ],
        ));
  }

  String _getSeatLayoutTemplateName(BuildContext context, LiveTemplateMode template) {
    final templateNameMaps = {
      LiveTemplateMode.verticalDynamicGrid: LiveKitLocalizations.of(context)!.common_template_dynamic_grid,
      LiveTemplateMode.verticalDynamicFloat: LiveKitLocalizations.of(context)!.common_template_dynamic_float,
      LiveTemplateMode.verticalStaticGrid: LiveKitLocalizations.of(context)!.common_template_static_grid,
      LiveTemplateMode.verticalStaticFloat: LiveKitLocalizations.of(context)!.common_template_static_float,
    };
    return templateNameMaps[template]!;
  }

  String _getSeatLayoutTemplateIcon(bool isPK, LiveTemplateMode template) {
    final templateNameMaps = {
      LiveTemplateMode.verticalDynamicGrid:
          isPK ? LiveImages.templatePKDynamicGrid : LiveImages.templateLinkDynamicGrid,
      LiveTemplateMode.verticalDynamicFloat:
          isPK ? LiveImages.templatePKDynamicFloat : LiveImages.templateLinkDynamicFloat,
      LiveTemplateMode.verticalStaticGrid: LiveImages.templateLinkStaticGrid,
      LiveTemplateMode.verticalStaticFloat: LiveImages.templateLinkStaticFloat,
    };
    return templateNameMaps[template]!;
  }

  Widget _buildTemplateGrid(BuildContext context, bool isPK, LiveTemplateMode id) {
    return GestureDetector(
        onTap: () {
          if (isPK) {
            editInfo.coHostTemplateMode.value = id;
          } else {
            editInfo.coGuestTemplateMode.value = id;
          }
        },
        child: ListenableBuilder(
            listenable: Listenable.merge([editInfo.coHostTemplateMode, editInfo.coGuestTemplateMode]),
            builder: (builder, _) {
              final normalDecoration =
                  BoxDecoration(color: LiveColors.seatTemplateGray, borderRadius: BorderRadius.all(Radius.circular(8.radius)));
              final selectedDecoration = BoxDecoration(
                  color: LiveColors.seatTemplateGray,
                  borderRadius: BorderRadius.all(Radius.circular(8.radius)),
                  border: Border.all(color: LiveColors.seatTemplateSelectedBorder, width: 2));
              final isSelected =
                  isPK ? editInfo.coHostTemplateMode.value == id : editInfo.coGuestTemplateMode.value == id;
              return Container(
                  padding: EdgeInsets.all(10.radius),
                  constraints: BoxConstraints(maxWidth: 150.width),
                  decoration: isSelected ? selectedDecoration : normalDecoration,
                  child: Row(
                    mainAxisAlignment: MainAxisAlignment.start,
                    crossAxisAlignment: CrossAxisAlignment.center,
                    children: [
                      SizedBox(
                        width: 24.height,
                        height: 24.height,
                        child: Image.asset(
                          _getSeatLayoutTemplateIcon(isPK, id),
                          package: Constants.pluginName,
                        ),
                      ),
                      SizedBox.fromSize(size: Size(10.width, 0)),
                      Expanded(
                          child: Text(
                        _getSeatLayoutTemplateName(context, id),
                        style: const TextStyle(color: LiveColors.whiteAlphaE6),
                      )),
                    ],
                  ));
            }));
  }
}
