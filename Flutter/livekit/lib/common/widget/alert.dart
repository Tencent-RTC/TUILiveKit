import 'package:flutter/material.dart';
import 'package:tencent_live_uikit/common/index.dart';

class AlertInfo {
  final String description;
  final String? imageUrl;

  final ({String title, Color titleColor})? cancelActionInfo;
  final ({String title, Color titleColor}) defaultActionInfo;
  final VoidCallback? cancelCallback;
  final VoidCallback defaultCallback;

  AlertInfo(
      {required this.description,
      required this.defaultActionInfo,
      required this.defaultCallback,
      this.imageUrl,
      this.cancelActionInfo,
      this.cancelCallback});
}

class Alert {
  static void showAlert(AlertInfo info) {
    showDialog(
      context: Global.appContext(),
      barrierDismissible: false,
      builder: (context) {
        final screenHeight = MediaQuery.of(context).size.height;
        return Dialog(
          backgroundColor: LiveColors.designStandardTransparent,
          child: ConstrainedBox(
            constraints: BoxConstraints(
              maxHeight: screenHeight * 0.8,
              minWidth: context.adapter.getWidth(323),
            ),
            child: SingleChildScrollView(
              child: Container(
                decoration: BoxDecoration(
                  color: LiveColors.designStandardFlowkitWhite,
                  borderRadius:
                      BorderRadius.circular(context.adapter.getHeight(10)),
                ),
                child: Column(
                  mainAxisSize: MainAxisSize.min,
                  children: [
                    Padding(
                      padding: EdgeInsets.only(
                          left: context.adapter.getWidth(45),
                          top: context.adapter.getHeight(24),
                          right: context.adapter.getWidth(45),
                          bottom: context.adapter.getHeight(24)),
                      child: Row(
                        children: [
                          if (info.imageUrl != null)
                            ClipOval(
                              child: Image.network(
                                info.imageUrl!,
                                width: context.adapter.getWidth(24),
                                height: context.adapter.getWidth(24),
                                fit: BoxFit.cover,
                                errorBuilder: (context, error, _) {
                                  return Image.asset(
                                    LiveImages.defaultAvatar,
                                    package: Constants.pluginName,
                                    width: context.adapter.getWidth(24),
                                    height: context.adapter.getWidth(24),
                                  );
                                },
                              ),
                            ),
                          SizedBox(width: context.adapter.getWidth(4)),
                          Expanded(
                            child: Text(
                              info.description,
                              style: const TextStyle(
                                  color: LiveColors.designStandardG1,
                                  fontSize: 16),
                            ),
                          ),
                        ],
                      ),
                    ),
                    Container(
                        height: context.adapter.getHeight(1),
                        color: LiveColors.designStandardG7),
                    IntrinsicHeight(
                      child: Row(
                        children: [
                          if (info.cancelActionInfo != null)
                            Expanded(
                              child: TextButton(
                                onPressed: info.cancelCallback,
                                child: Text(
                                  info.cancelActionInfo!.title,
                                  style: TextStyle(
                                    color: info.cancelActionInfo!.titleColor,
                                    fontSize: 16,
                                  ),
                                  maxLines: 1,
                                ),
                              ),
                            ),
                          if (info.cancelActionInfo != null)
                            Container(
                              width: context.adapter.getWidth(1),
                              color: LiveColors.designStandardG7,
                            ),
                          Expanded(
                            child: TextButton(
                              onPressed: info.defaultCallback,
                              child: Text(
                                info.defaultActionInfo.title,
                                style: TextStyle(
                                  color: info.defaultActionInfo.titleColor,
                                  fontSize: 16,
                                ),
                                maxLines: 1,
                              ),
                            ),
                          ),
                        ],
                      ),
                    ),
                  ],
                ),
              ),
            ),
          ),
        );
      },
    );
  }
}
