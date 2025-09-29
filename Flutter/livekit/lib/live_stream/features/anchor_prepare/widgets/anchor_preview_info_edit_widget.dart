import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:live_stream_core/live_core_widget/live_core_controller.dart';
import 'package:tencent_live_uikit/common/screen/index.dart';

import '../../../../common/constants/constants.dart';
import '../../../../common/language/index.dart';
import '../../../../common/resources/index.dart';
import '../../../../common/widget/index.dart';
import '../../../live_define.dart';
import '../../../manager/live_stream_manager.dart';
import '../anchor_preview_widget_define.dart';
import 'cover_select_panel_widget.dart';

class AnchorPreviewInfoEditWidget extends StatefulWidget {
  final EditInfo editInfo;
  final LiveStreamManager liveStreamManager;
  final LiveCoreController liveCoreController;

  const AnchorPreviewInfoEditWidget(
      {super.key,
        required this.editInfo,
        required this.liveStreamManager,
        required this.liveCoreController});

  @override
  State<AnchorPreviewInfoEditWidget> createState() =>
      _AnchorPreviewInfoEditWidgetState();
}

class _AnchorPreviewInfoEditWidgetState
    extends State<AnchorPreviewInfoEditWidget> {
  late final LiveStreamManager liveStreamManager;
  late final LiveCoreController liveCoreController;
  final TextEditingController _controller = TextEditingController();

  @override
  void initState() {
    super.initState();
    liveStreamManager = widget.liveStreamManager;
    liveCoreController = widget.liveCoreController;

    _controller.text = widget.editInfo.roomName.value;
  }

  @override
  void dispose() {
    _controller.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Stack(
      children: [
        _buildBackgroundWidget(),
        _buildLiveCoverWidget(liveStreamManager),
        _buildLiveNameWidget(liveStreamManager),
        _buildLiveModeWidget(liveStreamManager)
      ],
    );
  }

  Widget _buildBackgroundWidget() {
    return Positioned.fill(
      child: Container(
        decoration: BoxDecoration(
          color: LiveColors.notStandard40G1,
          borderRadius: BorderRadius.circular(16.radius),
        ),
      ),
    );
  }

  Widget _buildLiveCoverWidget(LiveStreamManager manager) {
    return Positioned(
      left: 8.width,
      top: 8.height,
      bottom: 8.height,
      child: GestureDetector(
        onTap: () {
          _showCoverSelectPanel();
        },
        child: Stack(
          children: [
            SizedBox(
              width: 72.width,
              height: 96.height,
              child: ClipRRect(
                borderRadius: BorderRadius.circular(8.radius),
                child: ValueListenableBuilder(
                  valueListenable: widget.editInfo.coverUrl,
                  builder: (context, coverUrl, child) {
                    return Image.network(
                      coverUrl,
                      fit: BoxFit.cover,
                      errorBuilder: (context, error, stackTrace) {
                        return Image.asset(
                          LiveImages.streamDefaultCover,
                          package: Constants.pluginName,
                        );
                      },
                    );
                  },
                ),
              ),
            ),
            Positioned(
              bottom: 0,
              left: 0,
              right: 0,
              child: Container(
                alignment: AlignmentDirectional.center,
                decoration: BoxDecoration(
                    color: LiveColors.notStandardBlack80Transparency,
                    borderRadius: BorderRadius.only(
                        bottomLeft: Radius.circular(8.width),
                        bottomRight: Radius.circular(8.width))),
                child: Text(
                  LiveKitLocalizations.of(Global.appContext())!
                      .common_set_cover,
                  style: const TextStyle(
                      color: LiveColors.designStandardG7, fontSize: 14),
                ),
              ),
            )
          ],
        ),
      ),
    );
  }

  Widget _buildLiveNameWidget(LiveStreamManager manager) {
    return Positioned(
      left: 92.width,
      right: 18.width,
      height: 48.height,
      child: Column(
        children: [
          Expanded(
            child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                crossAxisAlignment: CrossAxisAlignment.center,
                children: [
                  ValueListenableBuilder(
                    valueListenable: widget.editInfo.roomName,
                    builder: (context, value, child) {
                      return SizedBox(
                        width: 185.width,
                        child: TextField(
                            controller: _controller,
                            inputFormatters: [
                              _Utf8ByteLengthLimitingTextInputFormatter(100)
                            ],
                            decoration: const InputDecoration(
                              contentPadding: EdgeInsets.symmetric(
                                  horizontal: 0, vertical: 10),
                              border: InputBorder.none,
                            ),
                            textAlign: TextAlign.start,
                            maxLines: 1,
                            style: const TextStyle(
                                fontSize: 16,
                                color: LiveColors.designStandardG8,
                                fontWeight: FontWeight.w500),
                            onChanged: (value) {
                              widget.editInfo.roomName.value = value;
                            }),
                      );
                    },
                  ),
                  SizedBox(
                    width: 16.radius,
                    height: 16.radius,
                    child: Image.asset(
                      LiveImages.streamEditIcon,
                      package: Constants.pluginName,
                    ),
                  ),
                ]),
          ),
          Container(
            width: 235.width,
            height: 1.height,
            color: LiveColors.designStandardFlowkitWhite.withAlpha(0x4D),
          )
        ],
      ),
    );
  }

  Widget _buildLiveModeWidget(LiveStreamManager manager) {
    return Positioned(
      left: 92.width,
      top: 56.height,
      child: GestureDetector(
        onTap: () {
          _showLiveModeSelectPanel();
        },
        child: Row(
            mainAxisAlignment: MainAxisAlignment.start,
            crossAxisAlignment: CrossAxisAlignment.center,
            children: [
              SizedBox(
                width: 16.radius,
                height: 16.radius,
                child: Image.asset(
                  LiveImages.streamPrivacyMode,
                  package: Constants.pluginName,
                ),
              ),
              SizedBox(
                width: 8.width,
              ),
              Text(
                LiveKitLocalizations.of(Global.appContext())!
                    .common_stream_privacy_status,
                style: const TextStyle(
                  fontSize: 14,
                  color: LiveColors.designStandardG7,
                ),
                overflow: TextOverflow.ellipsis,
              ),
              ValueListenableBuilder(
                valueListenable: widget.editInfo.privacyMode,
                builder: (context, privacyMode, child) {
                  return Text(
                    _getPrivacyStatus(privacyMode),
                    style: const TextStyle(
                      fontSize: 14,
                      color: LiveColors.designStandardG7,
                    ),
                    overflow: TextOverflow.ellipsis,
                  );
                },
              ),
              SizedBox(
                width: 20.radius,
                height: 20.radius,
                child: Image.asset(
                  LiveImages.streamEditArrow,
                  package: Constants.pluginName,
                ),
              ),
            ]),
      ),
    );
  }
}

extension on _AnchorPreviewInfoEditWidgetState {
  void _showCoverSelectPanel() {
    popupWidget(CoverSelectPanelWidget(
      coverUrlNotifier: widget.editInfo.coverUrl,
      coverUrls: Constants.coverUrlList,
    ));
  }

  void _showLiveModeSelectPanel() {
    List<ActionSheetModel> list = [
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!
              .common_stream_privacy_status_default,
          bingData: 1),
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!
              .common_stream_privacy_status_privacy,
          bingData: 2),
    ];
    ActionSheet.show(list, (ActionSheetModel model) async {
      final mode = model.bingData == 1
          ? LiveStreamPrivacyStatus.public
          : LiveStreamPrivacyStatus.privacy;
      widget.editInfo.privacyMode.value = mode;
    });
  }

  String _getPrivacyStatus(LiveStreamPrivacyStatus status) {
    switch (status) {
      case LiveStreamPrivacyStatus.public:
        return LiveKitLocalizations.of(Global.appContext())!
            .common_stream_privacy_status_default;
      case LiveStreamPrivacyStatus.privacy:
        return LiveKitLocalizations.of(Global.appContext())!
            .common_stream_privacy_status_privacy;
      default:
        return LiveKitLocalizations.of(Global.appContext())!
            .common_stream_privacy_status_default;
    }
  }
}

class _Utf8ByteLengthLimitingTextInputFormatter extends TextInputFormatter {
  final int maxBytes;

  _Utf8ByteLengthLimitingTextInputFormatter(this.maxBytes);

  @override
  TextEditingValue formatEditUpdate(_, TextEditingValue newValue) {
    if (newValue.composing.isValid) return newValue;

    final bytes = utf8.encode(newValue.text);
    if (bytes.length <= maxBytes) return newValue;

    final safeText = _safeTruncate(newValue.text);
    if (safeText == newValue.text) return newValue;

    return TextEditingValue(
      text: safeText,
      selection: TextSelection.collapsed(offset: safeText.length),
    );
  }

  String _safeTruncate(String text) {
    final bytes = utf8.encode(text);
    if (bytes.length <= maxBytes) return text;

    final safeBytes = utf8.encode(text).sublist(0, maxBytes);

    int length = safeBytes.length;
    while (length > 0) {
      try {
        return utf8.decode(safeBytes.sublist(0, length), allowMalformed: false);
      } catch (_) {
        length--;
      }
    }

    return '';
  }
}
