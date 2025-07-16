import 'package:flutter/material.dart';
import 'dart:math';

import '../../../../common/resources/index.dart';

class BattleCountDownBackgroundWidget extends StatefulWidget {
  final double initialRadius;
  final double finalRadius;

  const BattleCountDownBackgroundWidget({
    super.key,
    required this.initialRadius,
    required this.finalRadius,
  });

  @override
  State<BattleCountDownBackgroundWidget> createState() =>
      _BattleCountDownBackgroundWidgetState();
}

class _BattleCountDownBackgroundWidgetState
    extends State<BattleCountDownBackgroundWidget>
    with TickerProviderStateMixin {
  late AnimationController _rotationController;
  late AnimationController _waveController;
  late Animation<double> _rotationAnimation;
  late Animation<double> _smallWaveRadiusAnimation;
  late Animation<double> _bigWaveRadiusAnimation;
  late Animation<double> _opacityAnimation;

  @override
  void initState() {
    super.initState();

    _rotationController = AnimationController(
      duration: const Duration(milliseconds: 1500),
      vsync: this,
    )..repeat();

    _waveController = AnimationController(
      duration: const Duration(milliseconds: 2500),
      vsync: this,
    )..repeat();

    _rotationAnimation = Tween<double>(begin: 0, end: 2 * pi).animate(
      CurvedAnimation(parent: _rotationController, curve: Curves.linear),
    );

    final waveCurve = CurvedAnimation(
      parent: _waveController,
      curve: Curves.easeInOut,
    );

    _smallWaveRadiusAnimation = Tween<double>(
      begin: widget.initialRadius,
      end: (widget.initialRadius + widget.finalRadius) / 2,
    ).animate(waveCurve);

    _bigWaveRadiusAnimation = Tween<double>(
      begin: (widget.initialRadius + widget.finalRadius) / 2,
      end: widget.finalRadius,
    ).animate(waveCurve);

    _opacityAnimation = Tween<double>(begin: 1.0, end: 0.2).animate(
      CurvedAnimation(parent: _waveController, curve: Curves.easeIn),
    );
  }

  @override
  void dispose() {
    _rotationController.dispose();
    _waveController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return AnimatedBuilder(
      animation: Listenable.merge([_rotationController, _waveController]),
      builder: (context, child) {
        return CustomPaint(
          painter: _ArcWavePainter(
            rotation: _rotationAnimation.value,
            smallWaveRadius: _smallWaveRadiusAnimation.value,
            bigWaveRadius: _bigWaveRadiusAnimation.value,
            waveOpacity: _opacityAnimation.value,
            initialRadius: widget.initialRadius,
          ),
          size: Size(widget.finalRadius * 2, widget.finalRadius * 2),
        );
      },
    );
  }
}

class _ArcWavePainter extends CustomPainter {
  final double rotation;
  final double smallWaveRadius;
  final double bigWaveRadius;
  final double waveOpacity;
  final double initialRadius;

  _ArcWavePainter({
    required this.rotation,
    required this.smallWaveRadius,
    required this.bigWaveRadius,
    required this.waveOpacity,
    required this.initialRadius,
  });

  @override
  void paint(Canvas canvas, Size size) {
    final center = Offset(size.width / 2, size.height / 2);

    final circlePainter = Paint()
      ..color = LiveColors.designStandardFlowkitWhite.withAlpha(0xCC)
      ..style = PaintingStyle.fill;
    canvas.drawCircle(size.center(Offset.zero), initialRadius, circlePainter);

    final smallWavePaint = Paint()
      ..style = PaintingStyle.stroke
      ..strokeWidth = 1.0
      ..color = LiveColors.designStandardFlowkitWhite.withAlpha((waveOpacity * 255).toInt());
    canvas.drawCircle(center, smallWaveRadius, smallWavePaint);

    final bigWavePaint = Paint()
      ..style = PaintingStyle.stroke
      ..strokeWidth = 1.0
      ..color = LiveColors.designStandardFlowkitWhite.withAlpha((waveOpacity * 255).toInt());
    canvas.drawCircle(center, bigWaveRadius, bigWavePaint);

    final arcPaint = Paint()
      ..style = PaintingStyle.stroke
      ..strokeWidth = 3.0
      ..strokeCap = StrokeCap.round
      ..color = LiveColors.designStandardFlowkitWhite;

    // long arc
    canvas.drawArc(
      Rect.fromCircle(center: center, radius: initialRadius),
      rotation,
      4 * pi / 5,
      false,
      arcPaint,
    );

    // short arc
    canvas.drawArc(
      Rect.fromCircle(center: center, radius: initialRadius),
      rotation + 6 * pi / 5,
      2 * pi / 5,
      false,
      arcPaint,
    );
  }

  @override
  bool shouldRepaint(covariant _ArcWavePainter oldDelegate) {
    return rotation != oldDelegate.rotation ||
        smallWaveRadius != oldDelegate.smallWaveRadius ||
        bigWaveRadius != oldDelegate.bigWaveRadius ||
        waveOpacity != oldDelegate.waveOpacity;
  }
}
