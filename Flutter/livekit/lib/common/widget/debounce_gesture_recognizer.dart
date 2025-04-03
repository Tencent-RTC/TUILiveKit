
import 'package:flutter/material.dart';

class DebounceGestureRecognizer extends StatefulWidget {
  final Widget child;
  final VoidCallback onTap;
  final Duration delay;

  const DebounceGestureRecognizer({
    super.key,
    required this.child,
    required this.onTap,
    this.delay = const Duration(milliseconds: 500),
  });

  @override
  State<DebounceGestureRecognizer> createState() =>
      _DebounceGestureRecognizerState();
}

class _DebounceGestureRecognizerState extends State<DebounceGestureRecognizer> {
  DateTime? _lastDateTime;

  @override
  void dispose() {
    super.dispose();
  }

  void _handleTap() {
    final current = DateTime.now();
    if (_lastDateTime != null &&
        current.difference(_lastDateTime!).inMilliseconds <
            widget.delay.inMilliseconds) {
      return;
    }
    _lastDateTime = current;
    widget.onTap();
  }

  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      onTap: _handleTap,
      child: widget.child,
    );
  }
}
