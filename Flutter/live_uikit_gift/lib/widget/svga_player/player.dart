import 'dart:math';

import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/widgets.dart';

import 'painter.dart';
import 'proto/svga_pb.dart';

import 'proto/svga_pb_server.dart';

class SVGAImage extends StatefulWidget {
  final SVGAAnimationController _controller;
  final BoxFit fit;
  final bool clearsAfterStop;

  /// Used to set the filterQuality of drawing the images inside SVGA.
  ///
  /// Defaults to [FilterQuality.low]
  final FilterQuality filterQuality;

  /// If `true`, the SVGA painter may draw beyond the expected canvas bounds
  /// and cause additional memory overhead.
  ///
  /// For backwards compatibility, defaults to `null`,
  /// which means allow drawing to overflow canvas bounds.
  final bool? allowDrawingOverflow;

  /// If `null`, the viewbox size of [MovieEntity] will be use.
  ///
  /// Defaults to null.
  final Size? preferredSize;

  const SVGAImage(
    this._controller, {
    Key? key,
    this.fit = BoxFit.contain,
    this.filterQuality = FilterQuality.low,
    this.allowDrawingOverflow,
    this.clearsAfterStop = true,
    this.preferredSize,
  }) : super(key: key);

  @override
  State<StatefulWidget> createState() => _SVGAImageState();

  @override
  void debugFillProperties(DiagnosticPropertiesBuilder properties) {
    super.debugFillProperties(properties);
    properties.add(DiagnosticsProperty<Listenable>('controller', _controller));
  }
}

class SVGAAnimationController extends AnimationController {
  MovieEntity? _movieEntity;
  bool canvasNeedsClear = false;

  SVGAAnimationController({
    required super.vsync,
  }) : super(duration: Duration.zero);

  set movieEntity(MovieEntity? value) {
    assert(!_isDisposed, '$this has been disposed!');
    if (_isDisposed) return;
    if (isAnimating) {
      stop();
    }
    if (value == null) {
      clear();
    }
    if (_movieEntity != null && _movieEntity!.autorelease) {
      _movieEntity!.dispose();
    }
    _movieEntity = value;
    if (value != null) {
      final movieParams = value.params;
      assert(
          movieParams.viewBoxWidth >= 0 &&
              movieParams.viewBoxHeight >= 0 &&
              movieParams.frames >= 1,
          "Invalid SVGA file!");
      int fps = movieParams.fps;
      if (fps == 0) fps = 20;
      duration =
          Duration(milliseconds: (movieParams.frames / fps * 1000).toInt());
    } else {
      duration = Duration.zero;
    }
    reset();
  }

  MovieEntity? get movieEntity => _movieEntity;

  /// Current drawing frame index of [movieEntity], returns 0 if [movieEntity] is null.
  int get currentFrame {
    final videoItem = _movieEntity;
    if (videoItem == null) return 0;
    return min(
      videoItem.params.frames - 1,
      max(0, (videoItem.params.frames.toDouble() * value).toInt()),
    );
  }

  /// Total frames of [movieEntity], returns 0 if [movieEntity] is null.
  int get frames {
    final videoItem = _movieEntity;
    if (videoItem == null) return 0;
    return videoItem.params.frames;
  }

  /// mark [_SVGAPainter] needs clear
  void clear() {
    canvasNeedsClear = true;
    if (!_isDisposed) notifyListeners();
  }

  @override
  TickerFuture forward({double? from}) {
    assert(_movieEntity != null,
        'SVGAAnimationController.forward() called after dispose()?');
    return super.forward(from: from);
  }

  bool _isDisposed = false;

  @override
  void dispose() {
    // auto dispose _videoItem when set null
    movieEntity = null;
    _isDisposed = true;
    super.dispose();
  }
}

class _SVGAImageState extends State<SVGAImage> {
  MovieEntity? video;

  @override
  void initState() {
    super.initState();
    video = widget._controller.movieEntity;
    widget._controller.addListener(_handleChange);
    widget._controller.addStatusListener(_handleStatusChange);
  }

  @override
  void didUpdateWidget(SVGAImage oldWidget) {
    super.didUpdateWidget(oldWidget);
    if (oldWidget._controller != widget._controller) {
      oldWidget._controller.removeListener(_handleChange);
      oldWidget._controller.removeStatusListener(_handleStatusChange);
      video = widget._controller.movieEntity;
      widget._controller.addListener(_handleChange);
      widget._controller.addStatusListener(_handleStatusChange);
    }
  }

  void _handleChange() {
    if (mounted &&
        !widget._controller._isDisposed &&
        video != widget._controller.movieEntity) {
      setState(() {
        // rebuild
        video = widget._controller.movieEntity;
      });
    }
  }

  void _handleStatusChange(AnimationStatus status) {
    if (status == AnimationStatus.completed && widget.clearsAfterStop) {
      widget._controller.clear();
    }
  }

  @override
  void dispose() {
    video = null;
    widget._controller.removeListener(_handleChange);
    widget._controller.removeStatusListener(_handleStatusChange);
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final video = this.video;
    final Size viewBoxSize;
    if (video == null || !video.isInitialized()) {
      viewBoxSize = Size.zero;
    } else {
      viewBoxSize = Size(video.params.viewBoxWidth, video.params.viewBoxHeight);
    }
    if (viewBoxSize.isEmpty) {
      return const SizedBox.shrink();
    }
    // sugguest the size of CustomPaint
    Size preferredSize = viewBoxSize;
    if (widget.preferredSize != null) {
      preferredSize =
          BoxConstraints.tight(widget.preferredSize!).constrain(viewBoxSize);
    }
    return IgnorePointer(
      child: CustomPaint(
        painter: SVGAPainter(
          // _SVGAPainter will auto repaint on _controller animating
          widget._controller,
          fit: widget.fit,
          filterQuality: widget.filterQuality,
          // default is allowing overflow for backward compatibility
          clipRect: widget.allowDrawingOverflow == false,
        ),
        size: preferredSize,
      ),
    );
  }
}
