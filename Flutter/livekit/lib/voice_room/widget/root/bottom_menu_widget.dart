import 'package:flutter/material.dart';
import 'package:live_stream_core/live_stream_core.dart';
import 'package:live_uikit_gift/live_uikit_gift.dart';
import 'package:rtc_room_engine/api/common/tui_common_define.dart';

import '../../../common/index.dart';
import '../../index.dart';

class BottomMenuWidget extends StatefulWidget {
  final VoiceRoomManager manager;
  final SeatGridController seatGridController;
  final bool isOwner;

  const BottomMenuWidget({super.key, required this.manager, required this.seatGridController, required this.isOwner});

  @override
  State<BottomMenuWidget> createState() => _BottomMenuWidgetState();
}

class _BottomMenuWidgetState extends State<BottomMenuWidget> {
  late final VoiceRoomManager manager;
  late final SeatGridController seatGridController;
  GiftListController? _giftListController;
  LikeSendController? _likeSendController;
  late final bool isOwner;

  @override
  void initState() {
    super.initState();
    manager = widget.manager;
    seatGridController = widget.seatGridController;
    isOwner = widget.isOwner;
  }

  @override
  void dispose() {
    _likeSendController?.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    if (_giftListController == null) {
      final language = DeviceLanguage.getCurrentLanguageCode(context);

      _giftListController = GiftListController(roomId: manager.roomState.roomId, language: language);
    }
    _likeSendController ??= LikeSendController(roomId: manager.roomState.roomId);
    return Container(
        constraints: BoxConstraints(minWidth: 30.width, minHeight: 46.height),
        width: 50.width,
        height: 46.height,
        child: Row(
            mainAxisAlignment: MainAxisAlignment.end,
            crossAxisAlignment: CrossAxisAlignment.center,
            spacing: 16.width,
            children: _generateBottomButtonWidgets()));
  }
}

extension on _BottomMenuWidgetState {
  List<Widget> _generateBottomButtonWidgets() {
    return widget.isOwner ? _generateOwnerBottomButtonWidgets() : _generateMemberBottomButtonWidgets();
  }

  List<Widget> _generateOwnerBottomButtonWidgets() {
    final List<Widget> buttons = List.empty(growable: true);

    final setting = BottomButtonWidget(
        normalImage: Image.asset(LiveImages.functionSettings, package: Constants.pluginName),
        normalTitle: Text(LiveKitLocalizations.of(Global.appContext())!.common_settings,
            style: const TextStyle(fontSize: 12, color: LiveColors.designStandardFlowkitWhite),
            textAlign: TextAlign.center),
        imageSize: 28.radius,
        onPressed: () {
          _showSettingsPanel();
        });
    buttons.add(setting);

    final seatManagement = ValueListenableBuilder(
      valueListenable: manager.seatState.seatApplicationList,
      builder: (context, applications, child) {
        final filterApplications = List<SeatApplication>.from(applications);
        filterApplications.removeWhere((application) => application.userId == manager.userState.selfInfo.userId);
        return BottomButtonWidget(
            normalImage: Image.asset(LiveImages.functionSeatManagement, package: Constants.pluginName),
            normalTitle: Text(LiveKitLocalizations.of(Global.appContext())!.common_seat_management,
                style: const TextStyle(fontSize: 12, color: LiveColors.designStandardFlowkitWhite),
                textAlign: TextAlign.center),
            imageSize: 28.radius,
            markCount: filterApplications.length,
            onPressed: () {
              _showSeatManagementPanel();
            });
      },
    );
    buttons.add(seatManagement);

    return buttons;
  }

  List<Widget> _generateMemberBottomButtonWidgets() {
    final List<Widget> buttons = List.empty(growable: true);

    final gift = BottomButtonWidget(
        normalImage: Image.asset(LiveImages.functionGift, package: Constants.pluginName),
        normalTitle: Text(LiveKitLocalizations.of(Global.appContext())!.common_gift_title,
            style: const TextStyle(fontSize: 12, color: LiveColors.designStandardFlowkitWhite),
            textAlign: TextAlign.center),
        imageSize: 28.radius,
        onPressed: () {
          _showGiftListWidget(context, _giftListController!);
        });

    buttons.add(gift);

    final like = BottomButtonWidget(
        normalImage: Image.asset(LiveImages.functionLike, package: Constants.pluginName),
        normalTitle: Text(LiveKitLocalizations.of(Global.appContext())!.common_like,
            style: const TextStyle(fontSize: 12, color: LiveColors.designStandardFlowkitWhite),
            textAlign: TextAlign.center),
        imageSize: 28.radius,
        onPressed: () {
          _likeSendController?.sendLike();
        },
        delay: const Duration(milliseconds: 30));
    buttons.add(like);

    final ValueNotifier<bool> rotationNotifier = ValueNotifier(false);
    manager.seatState.isApplyingToTakeSeat.addListener(() {
      rotationNotifier.value = manager.seatState.isApplyingToTakeSeat.value;
    });

    final linkMic = ListenableBuilder(
        listenable: Listenable.merge([manager.seatState.isApplyingToTakeSeat, manager.seatState.seatList]),
        builder: (context, _) {
          final isApplying = manager.seatState.isApplyingToTakeSeat.value;
          final isOnSeat =
              manager.seatState.seatList.value.any((seatInfo) => seatInfo.userId == manager.userState.selfInfo.userId);
          final normalImageUrl = isOnSeat ? LiveImages.functionLinked : LiveImages.functionVoiceRoomLink;

          final hangupLocalization = LiveKitLocalizations.of(Global.appContext())!.common_hang_up;
          final linkMicLocalization = LiveKitLocalizations.of(Global.appContext())!.common_link;
          final normalTitle = isOnSeat ? hangupLocalization : linkMicLocalization;
          return BottomButtonWidget(
              normalImage: Image.asset(normalImageUrl, package: Constants.pluginName),
              selectedImage: Image.asset(LiveImages.functionVoiceRoomLinking, package: Constants.pluginName),
              normalTitle: Text(normalTitle,
                  style: const TextStyle(fontSize: 12, color: LiveColors.designStandardFlowkitWhite),
                  textAlign: TextAlign.center),
              selectedTitle: Text(LiveKitLocalizations.of(Global.appContext())!.common_cancel,
                  style: const TextStyle(fontSize: 12, color: LiveColors.designStandardFlowkitWhite),
                  textAlign: TextAlign.center),
              imageSize: 28.radius,
              onPressed: () {
                _handleAudienceLinkMic();
              },
              rotationNotifier: rotationNotifier,
              isSelected: isApplying);
        });
    buttons.add(linkMic);

    return buttons;
  }

  void _showSettingsPanel() {
    popupWidget(
      SettingsPanelWidget(manager: manager),
      barrierColor: LiveColors.designStandardTransparent,
    );
  }

  void _showSeatManagementPanel() {
    popupWidget(SeatManagementPanelWidget(manager: manager, seatGridController: seatGridController));
  }

  void _showGiftListWidget(BuildContext context, GiftListController controller) {
    showModalBottomSheet(
        context: context,
        barrierColor: Colors.transparent,
        builder: (context) => GiftListWidget(giftListController: controller));
  }

  void _handleAudienceLinkMic() async {
    final selfUserId = manager.userState.selfInfo.userId;
    final isApplying = manager.seatState.isApplyingToTakeSeat.value;

    if (isApplying) {
      final result = await seatGridController.cancelRequest(selfUserId);
      if (result.code == TUIError.success) {
        manager.onApplyingToSeatStateChanged(false);
      } else {
        manager.toastSubject.add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
      }
      return;
    }

    final isOnSeat = manager.seatState.seatList.value.any((seatInfo) => seatInfo.userId == selfUserId);
    if (isOnSeat) {
      final result = await seatGridController.leaveSeat();
      if (result.code != TUIError.success) {
        manager.toastSubject.add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
      }
      seatGridController.stopMicrophone();
      return;
    }

    manager.onApplyingToSeatStateChanged(true);
    const timeoutValue = 60;
    final result = await seatGridController.takeSeat(-1, timeoutValue);
    if (result.code == TUIError.success) {
      switch (result.type) {
        case RequestResultType.onAccepted:
          manager.onApplyingToSeatStateChanged(false);
          break;
        case RequestResultType.onRejected:
          manager.onApplyingToSeatStateChanged(false);
          manager.toastSubject.add(LiveKitLocalizations.of(Global.appContext())!.common_voiceroom_take_seat_rejected);
          break;
        case RequestResultType.onCancelled:
          manager.onApplyingToSeatStateChanged(false);
          break;
        case RequestResultType.onTimeout:
          manager.onApplyingToSeatStateChanged(false);
          manager.toastSubject.add(LiveKitLocalizations.of(Global.appContext())!.common_voiceroom_take_seat_timeout);
          break;
        default:
          break;
      }
    } else {
      manager.onApplyingToSeatStateChanged(false);
      manager.toastSubject.add(ErrorHandler.convertToErrorMessage(result.code.rawValue, result.message) ?? '');
    }
  }
}

class BottomButtonWidget extends StatefulWidget {
  final Widget normalImage;
  final Widget? selectedImage;
  final double imageSize;
  final int markCount;
  final Widget? normalTitle;
  final Widget? selectedTitle;
  final VoidCallback? onPressed;
  final bool isSelected;
  final ValueNotifier<bool>? rotationNotifier;
  final Duration delay;

  const BottomButtonWidget(
      {super.key,
      required this.normalImage,
      required this.imageSize,
      this.markCount = 0,
      this.normalTitle,
      this.selectedTitle,
      this.selectedImage,
      this.onPressed,
      this.isSelected = false,
      this.rotationNotifier,
      this.delay = const Duration(milliseconds: 100)});

  @override
  State<BottomButtonWidget> createState() => _BottomButtonWidgetState();
}

class _BottomButtonWidgetState extends State<BottomButtonWidget> with SingleTickerProviderStateMixin {
  late AnimationController _rotationController;
  late ValueNotifier<bool> _internalNotifier;

  @override
  void initState() {
    super.initState();
    _rotationController = AnimationController(duration: const Duration(seconds: 2), vsync: this);

    _internalNotifier = widget.rotationNotifier ?? ValueNotifier(false);
    _internalNotifier.addListener(_handleRotationState);
  }

  void _handleRotationState() {
    if (_internalNotifier.value) {
      _rotationController.repeat();
    } else {
      _rotationController.reset();
    }
  }

  @override
  void dispose() {
    _rotationController.dispose();
    if (widget.rotationNotifier == null) {
      _internalNotifier.dispose();
    }
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return SizedBox(
      height: widget.imageSize,
      child: Stack(
        alignment: Alignment.center,
        clipBehavior: Clip.none,
        children: [
          RotationTransition(
              turns: _rotationController,
              child: DebounceGestureRecognizer(
                onTap: () => widget.onPressed?.call(),
                child: SizedBox(
                    width: 28.radius,
                    height: 28.radius,
                    child: widget.isSelected ? _initSelectedImage() : widget.normalImage),
              )),
          SizedBox(height: 2.height),
          widget.isSelected ? _initSelectedTitle() : _initNormalTitle(),
          Visibility(
              visible: widget.markCount != 0,
              child: Positioned(
                  top: -5,
                  right: -5,
                  child: Container(
                    width: 20.radius,
                    height: 20.radius,
                    decoration: BoxDecoration(color: Colors.red, borderRadius: BorderRadius.circular(10.radius)),
                    child: Text(
                      widget.markCount > 99 ? '99+' : '${widget.markCount}',
                      style: const TextStyle(color: LiveColors.designStandardFlowkitWhite, fontSize: 12),
                      textAlign: TextAlign.center,
                    ),
                  )))
        ],
      ),
    );
  }

  void startRotate() {
    if (!_rotationController.isAnimating) {
      _rotationController.repeat();
    }
  }

  void stopRotate() {
    _rotationController.stop();
  }

  Widget _initSelectedImage() {
    return widget.selectedImage ?? widget.normalImage;
  }

  Widget _initNormalTitle() {
    return Visibility(
        visible: widget.normalTitle != null,
        child: Positioned(top: widget.imageSize, child: widget.normalTitle ?? Container()));
  }

  Widget _initSelectedTitle() {
    return Visibility(
        visible: widget.normalTitle != null,
        child: Positioned(top: widget.imageSize, child: widget.selectedTitle ?? Container()));
  }
}
