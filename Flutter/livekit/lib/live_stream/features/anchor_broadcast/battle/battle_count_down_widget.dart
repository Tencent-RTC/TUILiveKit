import 'package:flutter/material.dart';

import 'dart:async';

import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/live_stream/features/anchor_broadcast/battle/battle_count_down_backgroud_widget.dart';

class BattleCountDownWidget extends StatefulWidget {
  final int countdownTime;
  final VoidCallback? onTimeEnd;
  final VoidCallback? onCancel;

  const BattleCountDownWidget({
    super.key,
    required this.countdownTime,
    this.onTimeEnd,
    this.onCancel,
  });

  @override
  State<BattleCountDownWidget> createState() => _BattleCountDownWidgetState();
}

class _BattleCountDownWidgetState extends State<BattleCountDownWidget> {
  late int _remainingSeconds;
  late Timer _countdownTimer;
  late Timer _dotsTimer;
  String _dots = '';

  @override
  void initState() {
    super.initState();
    _remainingSeconds = widget.countdownTime;
    _startCountdown();
    _startDotsAnimation();
  }

  @override
  void dispose() {
    _countdownTimer.cancel();
    _dotsTimer.cancel();
    super.dispose();
  }

  void _startCountdown() {
    _countdownTimer = Timer.periodic(const Duration(seconds: 1), (timer) {
      if (_remainingSeconds > 0) {
        setState(() {
          _remainingSeconds--;
        });
      } else {
        timer.cancel();
        widget.onTimeEnd?.call();
      }
    });
  }

  void _startDotsAnimation() {
    _dotsTimer = Timer.periodic(const Duration(milliseconds: 500), (timer) {
      setState(() {
        if (_dots.length == 3) {
          _dots = '';
        } else {
          _dots += '.';
        }
      });
    });
  }

  void _cancelButtonClick() {
    _countdownTimer.cancel();
    _dotsTimer.cancel();
    widget.onCancel?.call();
  }

  String _convertSecondsToMinutes(int seconds) {
    int minutes = seconds ~/ 60;
    int remainingSeconds = seconds % 60;
    return '${minutes.toString().padLeft(2, '0')}:${remainingSeconds.toString().padLeft(2, '0')}';
  }

  @override
  Widget build(BuildContext context) {
    return Center(
      child: Stack(
        alignment: Alignment.center,
        children: [
          Positioned(
              child: BattleCountDownBackgroundWidget(
                  initialRadius: 80.radius, finalRadius: 121.radius)),
          Positioned(
            top: 68.height,
            child: Text(
              _convertSecondsToMinutes(_remainingSeconds),
              style: const TextStyle(
                color: LiveColors.designStandardG2,
                fontSize: 40,
                fontWeight: FontWeight.bold,
              ),
            ),
          ),
          Positioned(
            top: 133.height,
            child: Center(
              child: Text(
                '${LiveKitLocalizations.of(Global.appContext())!.common_battle_wait_start}$_dots',
                style: const TextStyle(
                  color: LiveColors.designStandardG3,
                  fontSize: 12,
                  fontWeight: FontWeight.w400,
                ),
              ),
            ),
          ),
          Positioned(
            bottom: 40.height,
            child: Center(
              child: TextButton(
                onPressed: _cancelButtonClick,
                child: Text(
                  LiveKitLocalizations.of(Global.appContext())!.common_cancel,
                  style: const TextStyle(
                    color: LiveColors.notStandardRed,
                    fontSize: 14,
                    fontWeight: FontWeight.w500,
                  ),
                ),
              ),
            ),
          ),
        ],
      ),
    );
  }
}
