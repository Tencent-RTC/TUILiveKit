import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:flutter/services.dart';

import '../../../common/index.dart';
import '../../index.dart';

class LivePrepareInfoEditWidget extends StatefulWidget {
  final VoiceRoomManager manager;

  const LivePrepareInfoEditWidget({super.key, required this.manager});

  @override
  State<LivePrepareInfoEditWidget> createState() =>
      _LivePrepareInfoEditWidgetState();
}

class _LivePrepareInfoEditWidgetState extends State<LivePrepareInfoEditWidget> {
  final TextEditingController _controller = TextEditingController();
  late final VoiceRoomManager manager;

  @override
  void initState() {
    super.initState();
    manager = widget.manager;
    final roomName = _getDefaultRoomName();
    _controller.text = roomName;
    manager.onRoomNameChanged(roomName);
  }

  @override
  Widget build(BuildContext context) {
    return Stack(
      children: [
        _initBackgroundWidget(),
        _initLiveCoverWidget(manager),
        _initLiveNameWidget(manager),
        _initLiveModeWidget(manager)
      ],
    );
  }

  @override
  dispose() {
    _controller.dispose();
    super.dispose();
  }

  _initBackgroundWidget() {
    return Positioned.fill(
      child: Container(
        decoration: BoxDecoration(
          color: LiveColors.notStandard40G1,
          borderRadius: BorderRadius.circular(context.adapter.getWidth(16)),
        ),
      ),
    );
  }

  _initLiveCoverWidget(VoiceRoomManager manager) {
    return Positioned(
      left: context.adapter.getWidth(8),
      top: context.adapter.getHeight(8),
      bottom: context.adapter.getHeight(8),
      child: GestureDetector(
        onTap: () {
          _showCoverSelectPanel();
        },
        child: Stack(
          children: [
            SizedBox(
              width: context.adapter.getWidth(72),
              height: context.adapter.getHeight(96),
              child: ClipRRect(
                borderRadius:
                    BorderRadius.circular(context.adapter.getWidth(8)),
                child: ValueListenableBuilder(
                  valueListenable: manager.roomState.coverUrl,
                  builder: (context, value, child) {
                    return Image.network(
                      value,
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
                        bottomLeft:
                            Radius.circular(context.adapter.getWidth(8)),
                        bottomRight:
                            Radius.circular(context.adapter.getWidth(8)))),
                child: Text(
                  LiveKitLocalizations.of(Global.appContext())!.live_edit_cover,
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

  _initLiveNameWidget(VoiceRoomManager manager) {
    return Positioned(
      left: context.adapter.getWidth(92),
      right: context.adapter.getWidth(18),
      height: context.adapter.getHeight(48),
      child: Column(
        children: [
          Expanded(
            child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                crossAxisAlignment: CrossAxisAlignment.center,
                children: [
                  ValueListenableBuilder(
                    valueListenable: manager.roomState.roomName,
                    builder: (context, value, child) {
                      return SizedBox(
                        width: context.adapter.getWidth(185),
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
                                color: LiveColors.designStandardG7,
                                fontWeight: FontWeight.w500),
                            onChanged: (value) {
                              manager.onRoomNameChanged(value);
                            }),
                      );
                    },
                  ),
                  SizedBox(
                    width: context.adapter.getWidth(16),
                    height: context.adapter.getWidth(16),
                    child: Image.asset(
                      LiveImages.streamEditIcon,
                      package: Constants.pluginName,
                    ),
                  ),
                ]),
          ),
          Container(
            width: context.adapter.getWidth(235),
            height: context.adapter.getHeight(1),
            color: LiveColors.designStandardFlowkitWhite.withAlpha(0x4D),
          )
        ],
      ),
    );
  }

  _initLiveModeWidget(VoiceRoomManager manager) {
    return Positioned(
      left: context.adapter.getWidth(92),
      top: context.adapter.getHeight(56),
      child: GestureDetector(
        onTap: () {
          _showLiveModeSelectPanel();
        },
        child: Row(
            mainAxisAlignment: MainAxisAlignment.start,
            crossAxisAlignment: CrossAxisAlignment.center,
            children: [
              SizedBox(
                width: context.adapter.getWidth(16),
                height: context.adapter.getWidth(16),
                child: Image.asset(
                  LiveImages.streamPrivacyMode,
                  package: Constants.pluginName,
                ),
              ),
              SizedBox(
                width: context.adapter.getWidth(8),
              ),
              Text(
                LiveKitLocalizations.of(Global.appContext())!
                    .live_stream_privacy_status,
                style: const TextStyle(
                  fontSize: 14,
                  color: LiveColors.designStandardG7,
                ),
                overflow: TextOverflow.ellipsis,
              ),
              ValueListenableBuilder(
                valueListenable: manager.roomState.liveExtraInfo.value.liveMode,
                builder: (context, value, child) {
                  return Text(
                    _getPrivacyStatus(value),
                    style: const TextStyle(
                      fontSize: 14,
                      color: LiveColors.designStandardG7,
                    ),
                    overflow: TextOverflow.ellipsis,
                  );
                },
              ),
              SizedBox(
                width: context.adapter.getWidth(20),
                height: context.adapter.getWidth(20),
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

extension on _LivePrepareInfoEditWidgetState {
  void _showCoverSelectPanel() {
    popupWidget(LiveCoverSelectPanelWidget(
        manager: manager,
        coverUrls: Constants.coverUrlList,
        initialCoverUrl: manager.roomState.coverUrl.value));
  }

  void _showLiveModeSelectPanel() {
    List<ActionSheetModel> list = [
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!
              .live_stream_privacy_status_default,
          bingData: 1),
      ActionSheetModel(
          isCenter: true,
          text: LiveKitLocalizations.of(Global.appContext())!
              .live_stream_privacy_status_privacy,
          bingData: 2),
    ];
    ActionSheet.show(list, (ActionSheetModel model) async {
      final mode =
          model.bingData == 1 ? PrivacyStatus.publicity : PrivacyStatus.privacy;
      manager.onLiveModeChanged(mode);
    });
  }

  String _getPrivacyStatus(PrivacyStatus status) {
    switch (status) {
      case PrivacyStatus.publicity:
        return LiveKitLocalizations.of(Global.appContext())!
            .live_stream_privacy_status_default;
      case PrivacyStatus.privacy:
        return LiveKitLocalizations.of(Global.appContext())!
            .live_stream_privacy_status_privacy;
      default:
        return LiveKitLocalizations.of(Global.appContext())!
            .live_stream_privacy_status_default;
    }
  }
}

extension on _LivePrepareInfoEditWidgetState {
  String _getDefaultRoomName() {
    if (manager.roomState.roomName.value.isNotEmpty) {
      return manager.roomState.roomName.value;
    } else if (manager.userState.selfInfo.name.isEmpty) {
      return manager.userState.selfInfo.userId;
    } else {
      return manager.userState.selfInfo.name;
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
