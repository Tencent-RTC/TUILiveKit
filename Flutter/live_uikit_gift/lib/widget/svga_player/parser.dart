import 'dart:developer';
import 'dart:ui';

import 'package:archive/archive.dart';
import 'package:flutter/foundation.dart';
import 'package:flutter/painting.dart';
import 'package:flutter/services.dart';

import 'proto/svga_pb_server.dart';

const _filterKey = 'SVGAParser';

/// You use SVGAParser to load and decode animation files.
class SVGAParser {
  const SVGAParser();

  static const shared = SVGAParser();

  /// Download animation file from bundle assets, and decode it.
  Future<MovieEntity> decodeFromAssets(String path) async {
    return decodeFromBuffer((await rootBundle.load(path)).buffer.asUint8List());
  }

  /// Download animation file from bundle assets, and decode it.
  Future<MovieEntity> decodeFromStream(Uint8List stream) async {
    return decodeFromBuffer(stream);
  }

  /// Download animation file from buffer, and decode it.
  Future<MovieEntity> decodeFromBuffer(List<int> bytes) {
    TimelineTask? timeline;
    if (!kReleaseMode) {
      timeline = TimelineTask(filterKey: _filterKey)..start('DecodeFromBuffer', arguments: {'length': bytes.length});
    }
    final inflatedBytes = const ZLibDecoder().decodeBytes(bytes);
    if (timeline != null) {
      timeline.instant('MovieEntity.fromBuffer()', arguments: {'inflatedLength': inflatedBytes.length});
    }
    final movie = MovieEntity.fromBuffer(inflatedBytes);
    if (timeline != null) {
      timeline.instant('prepareResources()', arguments: {'images': movie.images.keys.join(',')});
    }
    return _prepareResources(
      _processShapeItems(movie),
      timeline: timeline,
    ).whenComplete(() {
      if (timeline != null) timeline.finish();
    });
  }

  MovieEntity _processShapeItems(MovieEntity movieItem) {
    movieItem.sprites.forEach((sprite) {
      List<ShapeEntity>? lastShape;
      sprite.frames.forEach((frame) {
        if (frame.shapes.isNotEmpty && frame.shapes.length > 0) {
          if (frame.shapes[0].type == ShapeEntityType.KEEP && lastShape != null) {
            frame.shapes = lastShape;
          } else if (frame.shapes.isNotEmpty == true) {
            lastShape = frame.shapes;
          }
        }
      });
    });
    return movieItem;
  }

  Future<MovieEntity> _prepareResources(MovieEntity movieItem, {TimelineTask? timeline}) {
    final images = movieItem.images;
    if (images.isEmpty) return Future.value(movieItem);
    return Future.wait(images.entries.map((item) async {
      // result null means a decoding error occurred
      final decodeImage = await _decodeImageItem(item.key, Uint8List.fromList(item.value), timeline: timeline);
      if (decodeImage != null) {
        movieItem.bitmapCache[item.key] = decodeImage;
      }
    })).then((_) => movieItem);
  }

  Future<Image?> _decodeImageItem(String key, Uint8List bytes, {TimelineTask? timeline}) async {
    TimelineTask? task;
    if (!kReleaseMode) {
      task = TimelineTask(filterKey: _filterKey, parent: timeline)
        ..start('DecodeImage', arguments: {'key': key, 'length': bytes.length});
    }
    try {
      final image = await decodeImageFromList(bytes);
      if (task != null) {
        task.finish(
          arguments: {'imageSize': '${image.width}x${image.height}'},
        );
      }
      return image;
    } catch (e, stack) {
      if (task != null) {
        task.finish(arguments: {'error': '$e', 'stack': '$stack'});
      }
      assert(() {
        FlutterError.reportError(FlutterErrorDetails(
          exception: e,
          stack: stack,
          library: 'svgaplayer',
          context: ErrorDescription('during prepare resource'),
          informationCollector: () sync* {
            yield ErrorSummary('Decoding image failed.');
          },
        ));
        return true;
      }());
      return null;
    }
  }
}
