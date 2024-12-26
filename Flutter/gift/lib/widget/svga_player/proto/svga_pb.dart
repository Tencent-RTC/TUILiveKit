import 'dart:core';
import 'dart:ui';
import 'package:protobuf/protobuf.dart';

import '../dynamic_entity.dart';
import 'svga_pb_enum.dart';

export 'svga_pb_enum.dart';

class MovieParams extends GeneratedMessage {
  static final BuilderInfo _i = BuilderInfo(
      const bool.fromEnvironment('protobuf.omit_message_names')
          ? ''
          : 'MovieParams',
      package: const PackageName(
          bool.fromEnvironment('protobuf.omit_message_names')
              ? ''
              : 'com.opensource.svga'),
      createEmptyInstance: create)
    ..a<double>(
        1, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'viewBoxWidth', PbFieldType.OF,
        protoName: 'viewBoxWidth')
    ..a<double>(
        2,
        const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'viewBoxHeight',
        PbFieldType.OF,
        protoName: 'viewBoxHeight')
    ..a<int>(3, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'fps', PbFieldType.O3)
    ..a<int>(4, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'frames', PbFieldType.O3)
    ..hasRequiredFields = false;

  MovieParams._() : super();
  factory MovieParams({
    double? viewBoxWidth,
    double? viewBoxHeight,
    int? fps,
    int? frames,
  }) {
    final _result = create();
    if (viewBoxWidth != null) {
      _result.viewBoxWidth = viewBoxWidth;
    }
    if (viewBoxHeight != null) {
      _result.viewBoxHeight = viewBoxHeight;
    }
    if (fps != null) {
      _result.fps = fps;
    }
    if (frames != null) {
      _result.frames = frames;
    }
    return _result;
  }
  factory MovieParams.fromBuffer(List<int> i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromBuffer(i, r);
  factory MovieParams.fromJson(String i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromJson(i, r);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
      'Will be removed in next major version')
  MovieParams clone() => MovieParams()..mergeFromMessage(this);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
      'Will be removed in next major version')
  MovieParams copyWith(void Function(MovieParams) updates) =>
      super.copyWith((message) => updates(message as MovieParams))
          as MovieParams; // ignore: deprecated_member_use
  BuilderInfo get info_ => _i;
  @pragma('dart2js:noInline')
  static MovieParams create() => MovieParams._();
  MovieParams createEmptyInstance() => create();
  static PbList<MovieParams> createRepeated() => PbList<MovieParams>();
  @pragma('dart2js:noInline')
  static MovieParams getDefault() => _defaultInstance ??=
      GeneratedMessage.$_defaultFor<MovieParams>(create);
  static MovieParams? _defaultInstance;

  @TagNumber(1)
  double get viewBoxWidth => $_getN(0);
  @TagNumber(1)
  set viewBoxWidth(double v) {
    $_setFloat(0, v);
  }

  @TagNumber(1)
  bool hasViewBoxWidth() => $_has(0);
  @TagNumber(1)
  void clearViewBoxWidth() => clearField(1);

  @TagNumber(2)
  double get viewBoxHeight => $_getN(1);
  @TagNumber(2)
  set viewBoxHeight(double v) {
    $_setFloat(1, v);
  }

  @TagNumber(2)
  bool hasViewBoxHeight() => $_has(1);
  @TagNumber(2)
  void clearViewBoxHeight() => clearField(2);

  @TagNumber(3)
  int get fps => $_getIZ(2);
  @TagNumber(3)
  set fps(int v) {
    $_setSignedInt32(2, v);
  }

  @TagNumber(3)
  bool hasFps() => $_has(2);
  @TagNumber(3)
  void clearFps() => clearField(3);

  @TagNumber(4)
  int get frames => $_getIZ(3);
  @TagNumber(4)
  set frames(int v) {
    $_setSignedInt32(3, v);
  }

  @TagNumber(4)
  bool hasFrames() => $_has(3);
  @TagNumber(4)
  void clearFrames() => clearField(4);
}

class SpriteEntity extends GeneratedMessage {
  static final BuilderInfo _i = BuilderInfo(
      const bool.fromEnvironment('protobuf.omit_message_names')
          ? ''
          : 'SpriteEntity',
      package: const PackageName(
          const bool.fromEnvironment('protobuf.omit_message_names')
              ? ''
              : 'com.opensource.svga'),
      createEmptyInstance: create)
    ..aOS(1, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'imageKey',
        protoName: 'imageKey')
    ..pc<FrameEntity>(
        2,
        const bool.fromEnvironment('protobuf.omit_field_names')
            ? ''
            : 'frames',
        PbFieldType.PM,
        subBuilder: FrameEntity.create)
    ..aOS(3, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'matteKey', protoName: 'matteKey')
    ..hasRequiredFields = false;

  SpriteEntity._() : super();
  factory SpriteEntity({
    String? imageKey,
    Iterable<FrameEntity>? frames,
    String? matteKey,
  }) {
    final _result = create();
    if (imageKey != null) {
      _result.imageKey = imageKey;
    }
    if (frames != null) {
      _result.frames.addAll(frames);
    }
    if (matteKey != null) {
      _result.matteKey = matteKey;
    }
    return _result;
  }
  factory SpriteEntity.fromBuffer(List<int> i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromBuffer(i, r);
  factory SpriteEntity.fromJson(String i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromJson(i, r);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
      'Will be removed in next major version')
  SpriteEntity clone() => SpriteEntity()..mergeFromMessage(this);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
      'Will be removed in next major version')
  SpriteEntity copyWith(void Function(SpriteEntity) updates) =>
      super.copyWith((message) => updates(message as SpriteEntity))
          as SpriteEntity; // ignore: deprecated_member_use
  BuilderInfo get info_ => _i;
  @pragma('dart2js:noInline')
  static SpriteEntity create() => SpriteEntity._();
  SpriteEntity createEmptyInstance() => create();
  static PbList<SpriteEntity> createRepeated() =>
      PbList<SpriteEntity>();
  @pragma('dart2js:noInline')
  static SpriteEntity getDefault() => _defaultInstance ??=
      GeneratedMessage.$_defaultFor<SpriteEntity>(create);
  static SpriteEntity? _defaultInstance;

  @TagNumber(1)
  String get imageKey => $_getSZ(0);
  @TagNumber(1)
  set imageKey(String v) {
    $_setString(0, v);
  }

  @TagNumber(1)
  bool hasImageKey() => $_has(0);
  @TagNumber(1)
  void clearImageKey() => clearField(1);

  @TagNumber(2)
  List<FrameEntity> get frames => $_getList(1);

  @TagNumber(3)
  String get matteKey => $_getSZ(2);
  @TagNumber(3)
  set matteKey(String v) {
    $_setString(2, v);
  }

  @TagNumber(3)
  bool hasMatteKey() => $_has(2);
  @TagNumber(3)
  void clearMatteKey() => clearField(3);
}

class AudioEntity extends GeneratedMessage {
  static final BuilderInfo _i = BuilderInfo(
      const bool.fromEnvironment('protobuf.omit_message_names')
          ? ''
          : 'AudioEntity',
      package: const PackageName(
          const bool.fromEnvironment('protobuf.omit_message_names')
              ? ''
              : 'com.opensource.svga'),
      createEmptyInstance: create)
    ..aOS(1, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'audioKey',
        protoName: 'audioKey')
    ..a<int>(
        2, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'startFrame', PbFieldType.O3,
        protoName: 'startFrame')
    ..a<int>(
        3, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'endFrame', PbFieldType.O3,
        protoName: 'endFrame')
    ..a<int>(4, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'startTime', PbFieldType.O3, protoName: 'startTime')
    ..a<int>(5, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'totalTime', PbFieldType.O3, protoName: 'totalTime')
    ..hasRequiredFields = false;

  AudioEntity._() : super();
  factory AudioEntity({
    String? audioKey,
    int? startFrame,
    int? endFrame,
    int? startTime,
    int? totalTime,
  }) {
    final _result = create();
    if (audioKey != null) {
      _result.audioKey = audioKey;
    }
    if (startFrame != null) {
      _result.startFrame = startFrame;
    }
    if (endFrame != null) {
      _result.endFrame = endFrame;
    }
    if (startTime != null) {
      _result.startTime = startTime;
    }
    if (totalTime != null) {
      _result.totalTime = totalTime;
    }
    return _result;
  }
  factory AudioEntity.fromBuffer(List<int> i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromBuffer(i, r);
  factory AudioEntity.fromJson(String i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromJson(i, r);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
      'Will be removed in next major version')
  AudioEntity clone() => AudioEntity()..mergeFromMessage(this);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
      'Will be removed in next major version')
  AudioEntity copyWith(void Function(AudioEntity) updates) =>
      super.copyWith((message) => updates(message as AudioEntity))
          as AudioEntity; // ignore: deprecated_member_use
  BuilderInfo get info_ => _i;
  @pragma('dart2js:noInline')
  static AudioEntity create() => AudioEntity._();
  AudioEntity createEmptyInstance() => create();
  static PbList<AudioEntity> createRepeated() => PbList<AudioEntity>();
  @pragma('dart2js:noInline')
  static AudioEntity getDefault() => _defaultInstance ??=
      GeneratedMessage.$_defaultFor<AudioEntity>(create);
  static AudioEntity? _defaultInstance;

  @TagNumber(1)
  String get audioKey => $_getSZ(0);
  @TagNumber(1)
  set audioKey(String v) {
    $_setString(0, v);
  }

  @TagNumber(1)
  bool hasAudioKey() => $_has(0);
  @TagNumber(1)
  void clearAudioKey() => clearField(1);

  @TagNumber(2)
  int get startFrame => $_getIZ(1);
  @TagNumber(2)
  set startFrame(int v) {
    $_setSignedInt32(1, v);
  }

  @TagNumber(2)
  bool hasStartFrame() => $_has(1);
  @TagNumber(2)
  void clearStartFrame() => clearField(2);

  @TagNumber(3)
  int get endFrame => $_getIZ(2);
  @TagNumber(3)
  set endFrame(int v) {
    $_setSignedInt32(2, v);
  }

  @TagNumber(3)
  bool hasEndFrame() => $_has(2);
  @TagNumber(3)
  void clearEndFrame() => clearField(3);

  @TagNumber(4)
  int get startTime => $_getIZ(3);
  @TagNumber(4)
  set startTime(int v) {
    $_setSignedInt32(3, v);
  }

  @TagNumber(4)
  bool hasStartTime() => $_has(3);
  @TagNumber(4)
  void clearStartTime() => clearField(4);

  @TagNumber(5)
  int get totalTime => $_getIZ(4);
  @TagNumber(5)
  set totalTime(int v) {
    $_setSignedInt32(4, v);
  }

  @TagNumber(5)
  bool hasTotalTime() => $_has(4);
  @TagNumber(5)
  void clearTotalTime() => clearField(5);
}

class Layout extends GeneratedMessage {
  static final BuilderInfo _i = BuilderInfo(
      const bool.fromEnvironment('protobuf.omit_message_names')
          ? ''
          : 'Layout',
      package: const PackageName(
          const bool.fromEnvironment('protobuf.omit_message_names')
              ? ''
              : 'com.opensource.svga'),
      createEmptyInstance: create)
    ..a<double>(
        1,
        const bool.fromEnvironment('protobuf.omit_field_names')
            ? ''
            : 'x',
        PbFieldType.OF)
    ..a<double>(
        2,
        const bool.fromEnvironment('protobuf.omit_field_names')
            ? ''
            : 'y',
        PbFieldType.OF)
    ..a<double>(
        3,
        const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'width',
        PbFieldType.OF)
    ..a<double>(4, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'height', PbFieldType.OF)
    ..hasRequiredFields = false;

  Layout._() : super();
  factory Layout({
    double? x,
    double? y,
    double? width,
    double? height,
  }) {
    final _result = create();
    if (x != null) {
      _result.x = x;
    }
    if (y != null) {
      _result.y = y;
    }
    if (width != null) {
      _result.width = width;
    }
    if (height != null) {
      _result.height = height;
    }
    return _result;
  }
  factory Layout.fromBuffer(List<int> i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromBuffer(i, r);
  factory Layout.fromJson(String i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromJson(i, r);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
      'Will be removed in next major version')
  Layout clone() => Layout()..mergeFromMessage(this);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
      'Will be removed in next major version')
  Layout copyWith(void Function(Layout) updates) =>
      super.copyWith((message) => updates(message as Layout))
          as Layout; // ignore: deprecated_member_use
  BuilderInfo get info_ => _i;
  @pragma('dart2js:noInline')
  static Layout create() => Layout._();
  Layout createEmptyInstance() => create();
  static PbList<Layout> createRepeated() => PbList<Layout>();
  @pragma('dart2js:noInline')
  static Layout getDefault() =>
      _defaultInstance ??= GeneratedMessage.$_defaultFor<Layout>(create);
  static Layout? _defaultInstance;

  @TagNumber(1)
  double get x => $_getN(0);
  @TagNumber(1)
  set x(double v) {
    $_setFloat(0, v);
  }

  @TagNumber(1)
  bool hasX() => $_has(0);
  @TagNumber(1)
  void clearX() => clearField(1);

  @TagNumber(2)
  double get y => $_getN(1);
  @TagNumber(2)
  set y(double v) {
    $_setFloat(1, v);
  }

  @TagNumber(2)
  bool hasY() => $_has(1);
  @TagNumber(2)
  void clearY() => clearField(2);

  @TagNumber(3)
  double get width => $_getN(2);
  @TagNumber(3)
  set width(double v) {
    $_setFloat(2, v);
  }

  @TagNumber(3)
  bool hasWidth() => $_has(2);
  @TagNumber(3)
  void clearWidth() => clearField(3);

  @TagNumber(4)
  double get height => $_getN(3);
  @TagNumber(4)
  set height(double v) {
    $_setFloat(3, v);
  }

  @TagNumber(4)
  bool hasHeight() => $_has(3);
  @TagNumber(4)
  void clearHeight() => clearField(4);
}

class Transform extends GeneratedMessage {
  static final BuilderInfo _i = BuilderInfo(
      const bool.fromEnvironment('protobuf.omit_message_names')
          ? ''
          : 'Transform',
      package: const PackageName(
          const bool.fromEnvironment('protobuf.omit_message_names')
              ? ''
              : 'com.opensource.svga'),
      createEmptyInstance: create)
    ..a<double>(
        1,
        const bool.fromEnvironment('protobuf.omit_field_names')
            ? ''
            : 'a',
        PbFieldType.OF)
    ..a<double>(
        2,
        const bool.fromEnvironment('protobuf.omit_field_names')
            ? ''
            : 'b',
        PbFieldType.OF)
    ..a<double>(
        3,
        const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'c',
        PbFieldType.OF)
    ..a<double>(4, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'd', PbFieldType.OF)
    ..a<double>(5, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'tx', PbFieldType.OF)
    ..a<double>(6, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'ty', PbFieldType.OF)
    ..hasRequiredFields = false;

  Transform._() : super();
  factory Transform({
    double? a,
    double? b,
    double? c,
    double? d,
    double? tx,
    double? ty,
  }) {
    final _result = create();
    if (a != null) {
      _result.a = a;
    }
    if (b != null) {
      _result.b = b;
    }
    if (c != null) {
      _result.c = c;
    }
    if (d != null) {
      _result.d = d;
    }
    if (tx != null) {
      _result.tx = tx;
    }
    if (ty != null) {
      _result.ty = ty;
    }
    return _result;
  }
  factory Transform.fromBuffer(List<int> i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromBuffer(i, r);
  factory Transform.fromJson(String i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromJson(i, r);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
      'Will be removed in next major version')
  Transform clone() => Transform()..mergeFromMessage(this);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
      'Will be removed in next major version')
  Transform copyWith(void Function(Transform) updates) =>
      super.copyWith((message) => updates(message as Transform))
          as Transform; // ignore: deprecated_member_use
  BuilderInfo get info_ => _i;
  @pragma('dart2js:noInline')
  static Transform create() => Transform._();
  Transform createEmptyInstance() => create();
  static PbList<Transform> createRepeated() => PbList<Transform>();
  @pragma('dart2js:noInline')
  static Transform getDefault() =>
      _defaultInstance ??= GeneratedMessage.$_defaultFor<Transform>(create);
  static Transform? _defaultInstance;

  @TagNumber(1)
  double get a => $_getN(0);
  @TagNumber(1)
  set a(double v) {
    $_setFloat(0, v);
  }

  @TagNumber(1)
  bool hasA() => $_has(0);
  @TagNumber(1)
  void clearA() => clearField(1);

  @TagNumber(2)
  double get b => $_getN(1);
  @TagNumber(2)
  set b(double v) {
    $_setFloat(1, v);
  }

  @TagNumber(2)
  bool hasB() => $_has(1);
  @TagNumber(2)
  void clearB() => clearField(2);

  @TagNumber(3)
  double get c => $_getN(2);
  @TagNumber(3)
  set c(double v) {
    $_setFloat(2, v);
  }

  @TagNumber(3)
  bool hasC() => $_has(2);
  @TagNumber(3)
  void clearC() => clearField(3);

  @TagNumber(4)
  double get d => $_getN(3);
  @TagNumber(4)
  set d(double v) {
    $_setFloat(3, v);
  }

  @TagNumber(4)
  bool hasD() => $_has(3);
  @TagNumber(4)
  void clearD() => clearField(4);

  @TagNumber(5)
  double get tx => $_getN(4);
  @TagNumber(5)
  set tx(double v) {
    $_setFloat(4, v);
  }

  @TagNumber(5)
  bool hasTx() => $_has(4);
  @TagNumber(5)
  void clearTx() => clearField(5);

  @TagNumber(6)
  double get ty => $_getN(5);
  @TagNumber(6)
  set ty(double v) {
    $_setFloat(5, v);
  }

  @TagNumber(6)
  bool hasTy() => $_has(5);
  @TagNumber(6)
  void clearTy() => clearField(6);
}

class ShapeEntity_ShapeArgs extends GeneratedMessage {
  static final BuilderInfo _i = BuilderInfo(
      const bool.fromEnvironment('protobuf.omit_message_names')
          ? ''
          : 'ShapeEntity.ShapeArgs',
      package: const PackageName(
          const bool.fromEnvironment('protobuf.omit_message_names')
              ? ''
              : 'com.opensource.svga'),
      createEmptyInstance: create)
    ..aOS(
        1,
        const bool.fromEnvironment('protobuf.omit_field_names')
            ? ''
            : 'd')
    ..hasRequiredFields = false;

  ShapeEntity_ShapeArgs._() : super();
  factory ShapeEntity_ShapeArgs({
    String? d,
  }) {
    final _result = create();
    if (d != null) {
      _result.d = d;
    }
    return _result;
  }
  factory ShapeEntity_ShapeArgs.fromBuffer(List<int> i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromBuffer(i, r);
  factory ShapeEntity_ShapeArgs.fromJson(String i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromJson(i, r);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
      'Will be removed in next major version')
  ShapeEntity_ShapeArgs clone() =>
      ShapeEntity_ShapeArgs()..mergeFromMessage(this);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
      'Will be removed in next major version')
  ShapeEntity_ShapeArgs copyWith(
          void Function(ShapeEntity_ShapeArgs) updates) =>
      super.copyWith((message) => updates(message as ShapeEntity_ShapeArgs))
          as ShapeEntity_ShapeArgs; // ignore: deprecated_member_use
  BuilderInfo get info_ => _i;
  @pragma('dart2js:noInline')
  static ShapeEntity_ShapeArgs create() => ShapeEntity_ShapeArgs._();
  ShapeEntity_ShapeArgs createEmptyInstance() => create();
  static PbList<ShapeEntity_ShapeArgs> createRepeated() =>
      PbList<ShapeEntity_ShapeArgs>();
  @pragma('dart2js:noInline')
  static ShapeEntity_ShapeArgs getDefault() => _defaultInstance ??=
      GeneratedMessage.$_defaultFor<ShapeEntity_ShapeArgs>(create);
  static ShapeEntity_ShapeArgs? _defaultInstance;

  @TagNumber(1)
  String get d => $_getSZ(0);
  @TagNumber(1)
  set d(String v) {
    $_setString(0, v);
  }

  @TagNumber(1)
  bool hasD() => $_has(0);
  @TagNumber(1)
  void clearD() => clearField(1);
}

class ShapeEntity_RectArgs extends GeneratedMessage {
  static final BuilderInfo _i = BuilderInfo(
      const bool.fromEnvironment('protobuf.omit_message_names')
          ? ''
          : 'ShapeEntity.RectArgs',
      package: const PackageName(
          const bool.fromEnvironment('protobuf.omit_message_names')
              ? ''
              : 'com.opensource.svga'),
      createEmptyInstance: create)
    ..a<double>(
        1,
        const bool.fromEnvironment('protobuf.omit_field_names')
            ? ''
            : 'x',
        PbFieldType.OF)
    ..a<double>(
        2,
        const bool.fromEnvironment('protobuf.omit_field_names')
            ? ''
            : 'y',
        PbFieldType.OF)
    ..a<double>(
        3,
        const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'width',
        PbFieldType.OF)
    ..a<double>(4, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'height', PbFieldType.OF)
    ..a<double>(5, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'cornerRadius', PbFieldType.OF, protoName: 'cornerRadius')
    ..hasRequiredFields = false;

  ShapeEntity_RectArgs._() : super();
  factory ShapeEntity_RectArgs({
    double? x,
    double? y,
    double? width,
    double? height,
    double? cornerRadius,
  }) {
    final _result = create();
    if (x != null) {
      _result.x = x;
    }
    if (y != null) {
      _result.y = y;
    }
    if (width != null) {
      _result.width = width;
    }
    if (height != null) {
      _result.height = height;
    }
    if (cornerRadius != null) {
      _result.cornerRadius = cornerRadius;
    }
    return _result;
  }
  factory ShapeEntity_RectArgs.fromBuffer(List<int> i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromBuffer(i, r);
  factory ShapeEntity_RectArgs.fromJson(String i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromJson(i, r);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
      'Will be removed in next major version')
  ShapeEntity_RectArgs clone() =>
      ShapeEntity_RectArgs()..mergeFromMessage(this);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
      'Will be removed in next major version')
  ShapeEntity_RectArgs copyWith(void Function(ShapeEntity_RectArgs) updates) =>
      super.copyWith((message) => updates(message as ShapeEntity_RectArgs))
          as ShapeEntity_RectArgs; // ignore: deprecated_member_use
  BuilderInfo get info_ => _i;
  @pragma('dart2js:noInline')
  static ShapeEntity_RectArgs create() => ShapeEntity_RectArgs._();
  ShapeEntity_RectArgs createEmptyInstance() => create();
  static PbList<ShapeEntity_RectArgs> createRepeated() =>
      PbList<ShapeEntity_RectArgs>();
  @pragma('dart2js:noInline')
  static ShapeEntity_RectArgs getDefault() => _defaultInstance ??=
      GeneratedMessage.$_defaultFor<ShapeEntity_RectArgs>(create);
  static ShapeEntity_RectArgs? _defaultInstance;

  @TagNumber(1)
  double get x => $_getN(0);
  @TagNumber(1)
  set x(double v) {
    $_setFloat(0, v);
  }

  @TagNumber(1)
  bool hasX() => $_has(0);
  @TagNumber(1)
  void clearX() => clearField(1);

  @TagNumber(2)
  double get y => $_getN(1);
  @TagNumber(2)
  set y(double v) {
    $_setFloat(1, v);
  }

  @TagNumber(2)
  bool hasY() => $_has(1);
  @TagNumber(2)
  void clearY() => clearField(2);

  @TagNumber(3)
  double get width => $_getN(2);
  @TagNumber(3)
  set width(double v) {
    $_setFloat(2, v);
  }

  @TagNumber(3)
  bool hasWidth() => $_has(2);
  @TagNumber(3)
  void clearWidth() => clearField(3);

  @TagNumber(4)
  double get height => $_getN(3);
  @TagNumber(4)
  set height(double v) {
    $_setFloat(3, v);
  }

  @TagNumber(4)
  bool hasHeight() => $_has(3);
  @TagNumber(4)
  void clearHeight() => clearField(4);

  @TagNumber(5)
  double get cornerRadius => $_getN(4);
  @TagNumber(5)
  set cornerRadius(double v) {
    $_setFloat(4, v);
  }

  @TagNumber(5)
  bool hasCornerRadius() => $_has(4);
  @TagNumber(5)
  void clearCornerRadius() => clearField(5);
}

class ShapeEntity_EllipseArgs extends GeneratedMessage {
  static final BuilderInfo _i = BuilderInfo(
      const bool.fromEnvironment('protobuf.omit_message_names')
          ? ''
          : 'ShapeEntity.EllipseArgs',
      package: const PackageName(
          const bool.fromEnvironment('protobuf.omit_message_names')
              ? ''
              : 'com.opensource.svga'),
      createEmptyInstance: create)
    ..a<double>(
        1,
        const bool.fromEnvironment('protobuf.omit_field_names')
            ? ''
            : 'x',
        PbFieldType.OF)
    ..a<double>(
        2,
        const bool.fromEnvironment('protobuf.omit_field_names')
            ? ''
            : 'y',
        PbFieldType.OF)
    ..a<double>(
        3, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'radiusX', PbFieldType.OF,
        protoName: 'radiusX')
    ..a<double>(4, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'radiusY', PbFieldType.OF, protoName: 'radiusY')
    ..hasRequiredFields = false;

  ShapeEntity_EllipseArgs._() : super();
  factory ShapeEntity_EllipseArgs({
    double? x,
    double? y,
    double? radiusX,
    double? radiusY,
  }) {
    final _result = create();
    if (x != null) {
      _result.x = x;
    }
    if (y != null) {
      _result.y = y;
    }
    if (radiusX != null) {
      _result.radiusX = radiusX;
    }
    if (radiusY != null) {
      _result.radiusY = radiusY;
    }
    return _result;
  }
  factory ShapeEntity_EllipseArgs.fromBuffer(List<int> i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromBuffer(i, r);
  factory ShapeEntity_EllipseArgs.fromJson(String i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromJson(i, r);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
      'Will be removed in next major version')
  ShapeEntity_EllipseArgs clone() =>
      ShapeEntity_EllipseArgs()..mergeFromMessage(this);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
      'Will be removed in next major version')
  ShapeEntity_EllipseArgs copyWith(
          void Function(ShapeEntity_EllipseArgs) updates) =>
      super.copyWith((message) => updates(message as ShapeEntity_EllipseArgs))
          as ShapeEntity_EllipseArgs; // ignore: deprecated_member_use
  BuilderInfo get info_ => _i;
  @pragma('dart2js:noInline')
  static ShapeEntity_EllipseArgs create() => ShapeEntity_EllipseArgs._();
  ShapeEntity_EllipseArgs createEmptyInstance() => create();
  static PbList<ShapeEntity_EllipseArgs> createRepeated() =>
      PbList<ShapeEntity_EllipseArgs>();
  @pragma('dart2js:noInline')
  static ShapeEntity_EllipseArgs getDefault() => _defaultInstance ??=
      GeneratedMessage.$_defaultFor<ShapeEntity_EllipseArgs>(create);
  static ShapeEntity_EllipseArgs? _defaultInstance;

  @TagNumber(1)
  double get x => $_getN(0);
  @TagNumber(1)
  set x(double v) {
    $_setFloat(0, v);
  }

  @TagNumber(1)
  bool hasX() => $_has(0);
  @TagNumber(1)
  void clearX() => clearField(1);

  @TagNumber(2)
  double get y => $_getN(1);
  @TagNumber(2)
  set y(double v) {
    $_setFloat(1, v);
  }

  @TagNumber(2)
  bool hasY() => $_has(1);
  @TagNumber(2)
  void clearY() => clearField(2);

  @TagNumber(3)
  double get radiusX => $_getN(2);
  @TagNumber(3)
  set radiusX(double v) {
    $_setFloat(2, v);
  }

  @TagNumber(3)
  bool hasRadiusX() => $_has(2);
  @TagNumber(3)
  void clearRadiusX() => clearField(3);

  @TagNumber(4)
  double get radiusY => $_getN(3);
  @TagNumber(4)
  set radiusY(double v) {
    $_setFloat(3, v);
  }

  @TagNumber(4)
  bool hasRadiusY() => $_has(3);
  @TagNumber(4)
  void clearRadiusY() => clearField(4);
}

class ShapeEntity_ShapeStyle_RGBAColor extends GeneratedMessage {
  static final BuilderInfo _i = BuilderInfo(
      const bool.fromEnvironment('protobuf.omit_message_names')
          ? ''
          : 'ShapeEntity.ShapeStyle.RGBAColor',
      package: const PackageName(
          const bool.fromEnvironment('protobuf.omit_message_names')
              ? ''
              : 'com.opensource.svga'),
      createEmptyInstance: create)
    ..a<double>(
        1,
        const bool.fromEnvironment('protobuf.omit_field_names')
            ? ''
            : 'r',
        PbFieldType.OF)
    ..a<double>(
        2,
        const bool.fromEnvironment('protobuf.omit_field_names')
            ? ''
            : 'g',
        PbFieldType.OF)
    ..a<double>(
        3,
        const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'b',
        PbFieldType.OF)
    ..a<double>(4, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'a', PbFieldType.OF)
    ..hasRequiredFields = false;

  ShapeEntity_ShapeStyle_RGBAColor._() : super();
  factory ShapeEntity_ShapeStyle_RGBAColor({
    double? r,
    double? g,
    double? b,
    double? a,
  }) {
    final _result = create();
    if (r != null) {
      _result.r = r;
    }
    if (g != null) {
      _result.g = g;
    }
    if (b != null) {
      _result.b = b;
    }
    if (a != null) {
      _result.a = a;
    }
    return _result;
  }
  factory ShapeEntity_ShapeStyle_RGBAColor.fromBuffer(List<int> i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromBuffer(i, r);
  factory ShapeEntity_ShapeStyle_RGBAColor.fromJson(String i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromJson(i, r);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
      'Will be removed in next major version')
  ShapeEntity_ShapeStyle_RGBAColor clone() =>
      ShapeEntity_ShapeStyle_RGBAColor()..mergeFromMessage(this);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
      'Will be removed in next major version')
  ShapeEntity_ShapeStyle_RGBAColor copyWith(
          void Function(ShapeEntity_ShapeStyle_RGBAColor) updates) =>
      super.copyWith(
              (message) => updates(message as ShapeEntity_ShapeStyle_RGBAColor))
          as ShapeEntity_ShapeStyle_RGBAColor; // ignore: deprecated_member_use
  BuilderInfo get info_ => _i;
  @pragma('dart2js:noInline')
  static ShapeEntity_ShapeStyle_RGBAColor create() =>
      ShapeEntity_ShapeStyle_RGBAColor._();
  ShapeEntity_ShapeStyle_RGBAColor createEmptyInstance() => create();
  static PbList<ShapeEntity_ShapeStyle_RGBAColor> createRepeated() =>
      PbList<ShapeEntity_ShapeStyle_RGBAColor>();
  @pragma('dart2js:noInline')
  static ShapeEntity_ShapeStyle_RGBAColor getDefault() => _defaultInstance ??=
      GeneratedMessage.$_defaultFor<ShapeEntity_ShapeStyle_RGBAColor>(
          create);
  static ShapeEntity_ShapeStyle_RGBAColor? _defaultInstance;

  @TagNumber(1)
  double get r => $_getN(0);
  @TagNumber(1)
  set r(double v) {
    $_setFloat(0, v);
  }

  @TagNumber(1)
  bool hasR() => $_has(0);
  @TagNumber(1)
  void clearR() => clearField(1);

  @TagNumber(2)
  double get g => $_getN(1);
  @TagNumber(2)
  set g(double v) {
    $_setFloat(1, v);
  }

  @TagNumber(2)
  bool hasG() => $_has(1);
  @TagNumber(2)
  void clearG() => clearField(2);

  @TagNumber(3)
  double get b => $_getN(2);
  @TagNumber(3)
  set b(double v) {
    $_setFloat(2, v);
  }

  @TagNumber(3)
  bool hasB() => $_has(2);
  @TagNumber(3)
  void clearB() => clearField(3);

  @TagNumber(4)
  double get a => $_getN(3);
  @TagNumber(4)
  set a(double v) {
    $_setFloat(3, v);
  }

  @TagNumber(4)
  bool hasA() => $_has(3);
  @TagNumber(4)
  void clearA() => clearField(4);
}

class ShapeEntity_ShapeStyle extends GeneratedMessage {
  static final BuilderInfo _i = BuilderInfo(const bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'ShapeEntity.ShapeStyle',
      package: const PackageName(const bool.fromEnvironment('protobuf.omit_message_names')
          ? ''
          : 'com.opensource.svga'),
      createEmptyInstance: create)
    ..aOM<ShapeEntity_ShapeStyle_RGBAColor>(
        1, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'fill',
        subBuilder: ShapeEntity_ShapeStyle_RGBAColor.create)
    ..aOM<ShapeEntity_ShapeStyle_RGBAColor>(
        2, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'stroke',
        subBuilder: ShapeEntity_ShapeStyle_RGBAColor.create)
    ..a<double>(3, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'strokeWidth', PbFieldType.OF,
        protoName: 'strokeWidth')
    ..e<ShapeEntityStyleLineCap>(4, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'lineCap', PbFieldType.OE,
        protoName: 'lineCap',
        defaultOrMaker: ShapeEntityStyleLineCap.LineCap_BUTT,
        valueOf: ShapeEntityStyleLineCap.valueOf,
        enumValues: ShapeEntityStyleLineCap.values)
    ..e<ShapeEntityStyleLineJoin>(5, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'lineJoin', PbFieldType.OE,
        protoName: 'lineJoin',
        defaultOrMaker: ShapeEntityStyleLineJoin.LineJoin_MITER,
        valueOf: ShapeEntityStyleLineJoin.valueOf,
        enumValues: ShapeEntityStyleLineJoin.values)
    ..a<double>(6, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'miterLimit', PbFieldType.OF, protoName: 'miterLimit')
    ..a<double>(7, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'lineDashI', PbFieldType.OF, protoName: 'lineDashI')
    ..a<double>(8, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'lineDashII', PbFieldType.OF, protoName: 'lineDashII')
    ..a<double>(9, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'lineDashIII', PbFieldType.OF, protoName: 'lineDashIII')
    ..hasRequiredFields = false;

  ShapeEntity_ShapeStyle._() : super();
  factory ShapeEntity_ShapeStyle({
    ShapeEntity_ShapeStyle_RGBAColor? fill,
    ShapeEntity_ShapeStyle_RGBAColor? stroke,
    double? strokeWidth,
    ShapeEntityStyleLineCap? lineCap,
    ShapeEntityStyleLineJoin? lineJoin,
    double? miterLimit,
    double? lineDashI,
    double? lineDashII,
    double? lineDashIII,
  }) {
    final _result = create();
    if (fill != null) {
      _result.fill = fill;
    }
    if (stroke != null) {
      _result.stroke = stroke;
    }
    if (strokeWidth != null) {
      _result.strokeWidth = strokeWidth;
    }
    if (lineCap != null) {
      _result.lineCap = lineCap;
    }
    if (lineJoin != null) {
      _result.lineJoin = lineJoin;
    }
    if (miterLimit != null) {
      _result.miterLimit = miterLimit;
    }
    if (lineDashI != null) {
      _result.lineDashI = lineDashI;
    }
    if (lineDashII != null) {
      _result.lineDashII = lineDashII;
    }
    if (lineDashIII != null) {
      _result.lineDashIII = lineDashIII;
    }
    return _result;
  }
  factory ShapeEntity_ShapeStyle.fromBuffer(List<int> i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromBuffer(i, r);
  factory ShapeEntity_ShapeStyle.fromJson(String i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromJson(i, r);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
      'Will be removed in next major version')
  ShapeEntity_ShapeStyle clone() =>
      ShapeEntity_ShapeStyle()..mergeFromMessage(this);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
      'Will be removed in next major version')
  ShapeEntity_ShapeStyle copyWith(
          void Function(ShapeEntity_ShapeStyle) updates) =>
      super.copyWith((message) => updates(message as ShapeEntity_ShapeStyle))
          as ShapeEntity_ShapeStyle; // ignore: deprecated_member_use
  BuilderInfo get info_ => _i;
  @pragma('dart2js:noInline')
  static ShapeEntity_ShapeStyle create() => ShapeEntity_ShapeStyle._();
  ShapeEntity_ShapeStyle createEmptyInstance() => create();
  static PbList<ShapeEntity_ShapeStyle> createRepeated() =>
      PbList<ShapeEntity_ShapeStyle>();
  @pragma('dart2js:noInline')
  static ShapeEntity_ShapeStyle getDefault() => _defaultInstance ??=
      GeneratedMessage.$_defaultFor<ShapeEntity_ShapeStyle>(create);
  static ShapeEntity_ShapeStyle? _defaultInstance;

  @TagNumber(1)
  ShapeEntity_ShapeStyle_RGBAColor get fill => $_getN(0);
  @TagNumber(1)
  set fill(ShapeEntity_ShapeStyle_RGBAColor v) {
    setField(1, v);
  }

  @TagNumber(1)
  bool hasFill() => $_has(0);
  @TagNumber(1)
  void clearFill() => clearField(1);
  @TagNumber(1)
  ShapeEntity_ShapeStyle_RGBAColor ensureFill() => $_ensure(0);

  @TagNumber(2)
  ShapeEntity_ShapeStyle_RGBAColor get stroke => $_getN(1);
  @TagNumber(2)
  set stroke(ShapeEntity_ShapeStyle_RGBAColor v) {
    setField(2, v);
  }

  @TagNumber(2)
  bool hasStroke() => $_has(1);
  @TagNumber(2)
  void clearStroke() => clearField(2);
  @TagNumber(2)
  ShapeEntity_ShapeStyle_RGBAColor ensureStroke() => $_ensure(1);

  @TagNumber(3)
  double get strokeWidth => $_getN(2);
  @TagNumber(3)
  set strokeWidth(double v) {
    $_setFloat(2, v);
  }

  @TagNumber(3)
  bool hasStrokeWidth() => $_has(2);
  @TagNumber(3)
  void clearStrokeWidth() => clearField(3);

  @TagNumber(4)
  ShapeEntityStyleLineCap get lineCap => $_getN(3);
  @TagNumber(4)
  set lineCap(ShapeEntityStyleLineCap v) {
    setField(4, v);
  }

  @TagNumber(4)
  bool hasLineCap() => $_has(3);
  @TagNumber(4)
  void clearLineCap() => clearField(4);

  @TagNumber(5)
  ShapeEntityStyleLineJoin get lineJoin => $_getN(4);
  @TagNumber(5)
  set lineJoin(ShapeEntityStyleLineJoin v) {
    setField(5, v);
  }

  @TagNumber(5)
  bool hasLineJoin() => $_has(4);
  @TagNumber(5)
  void clearLineJoin() => clearField(5);

  @TagNumber(6)
  double get miterLimit => $_getN(5);
  @TagNumber(6)
  set miterLimit(double v) {
    $_setFloat(5, v);
  }

  @TagNumber(6)
  bool hasMiterLimit() => $_has(5);
  @TagNumber(6)
  void clearMiterLimit() => clearField(6);

  @TagNumber(7)
  double get lineDashI => $_getN(6);
  @TagNumber(7)
  set lineDashI(double v) {
    $_setFloat(6, v);
  }

  @TagNumber(7)
  bool hasLineDashI() => $_has(6);
  @TagNumber(7)
  void clearLineDashI() => clearField(7);

  @TagNumber(8)
  double get lineDashII => $_getN(7);
  @TagNumber(8)
  set lineDashII(double v) {
    $_setFloat(7, v);
  }

  @TagNumber(8)
  bool hasLineDashII() => $_has(7);
  @TagNumber(8)
  void clearLineDashII() => clearField(8);

  @TagNumber(9)
  double get lineDashIII => $_getN(8);
  @TagNumber(9)
  set lineDashIII(double v) {
    $_setFloat(8, v);
  }

  @TagNumber(9)
  bool hasLineDashIII() => $_has(8);
  @TagNumber(9)
  void clearLineDashIII() => clearField(9);
}

enum ShapeEntity_Args { shape, rect, ellipse, notSet }

class ShapeEntity extends GeneratedMessage {
  static const Map<int, ShapeEntity_Args> _ShapeEntity_ArgsByTag = {
    2: ShapeEntity_Args.shape,
    3: ShapeEntity_Args.rect,
    4: ShapeEntity_Args.ellipse,
    0: ShapeEntity_Args.notSet
  };
  static final BuilderInfo _i = BuilderInfo(const bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'ShapeEntity',
      package: const PackageName(const bool.fromEnvironment('protobuf.omit_message_names')
          ? ''
          : 'com.opensource.svga'),
      createEmptyInstance: create)
    ..oo(0, [2, 3, 4])
    ..e<ShapeEntityType>(
        1, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'type', PbFieldType.OE,
        defaultOrMaker: ShapeEntityType.SHAPE,
        valueOf: ShapeEntityType.valueOf,
        enumValues: ShapeEntityType.values)
    ..aOM<ShapeEntity_ShapeArgs>(
        2, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'shape',
        subBuilder: ShapeEntity_ShapeArgs.create)
    ..aOM<ShapeEntity_RectArgs>(
        3, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'rect',
        subBuilder: ShapeEntity_RectArgs.create)
    ..aOM<ShapeEntity_EllipseArgs>(
        4, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'ellipse',
        subBuilder: ShapeEntity_EllipseArgs.create)
    ..aOM<ShapeEntity_ShapeStyle>(10, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'styles', subBuilder: ShapeEntity_ShapeStyle.create)
    ..aOM<Transform>(11, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'transform', subBuilder: Transform.create)
    ..hasRequiredFields = false;

  ShapeEntity._() : super();
  factory ShapeEntity({
    ShapeEntityType? type,
    ShapeEntity_ShapeArgs? shape,
    ShapeEntity_RectArgs? rect,
    ShapeEntity_EllipseArgs? ellipse,
    ShapeEntity_ShapeStyle? styles,
    Transform? transform,
  }) {
    final _result = create();
    if (type != null) {
      _result.type = type;
    }
    if (shape != null) {
      _result.shape = shape;
    }
    if (rect != null) {
      _result.rect = rect;
    }
    if (ellipse != null) {
      _result.ellipse = ellipse;
    }
    if (styles != null) {
      _result.styles = styles;
    }
    if (transform != null) {
      _result.transform = transform;
    }
    return _result;
  }
  factory ShapeEntity.fromBuffer(List<int> i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromBuffer(i, r);
  factory ShapeEntity.fromJson(String i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromJson(i, r);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
      'Will be removed in next major version')
  ShapeEntity clone() => ShapeEntity()..mergeFromMessage(this);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
      'Will be removed in next major version')
  ShapeEntity copyWith(void Function(ShapeEntity) updates) =>
      super.copyWith((message) => updates(message as ShapeEntity))
          as ShapeEntity; // ignore: deprecated_member_use
  BuilderInfo get info_ => _i;
  @pragma('dart2js:noInline')
  static ShapeEntity create() => ShapeEntity._();
  ShapeEntity createEmptyInstance() => create();
  static PbList<ShapeEntity> createRepeated() => PbList<ShapeEntity>();
  @pragma('dart2js:noInline')
  static ShapeEntity getDefault() => _defaultInstance ??=
      GeneratedMessage.$_defaultFor<ShapeEntity>(create);
  static ShapeEntity? _defaultInstance;

  ShapeEntity_Args whichArgs() => _ShapeEntity_ArgsByTag[$_whichOneof(0)]!;
  void clearArgs() => clearField($_whichOneof(0));

  @TagNumber(1)
  ShapeEntityType get type => $_getN(0);
  @TagNumber(1)
  set type(ShapeEntityType v) {
    setField(1, v);
  }

  @TagNumber(1)
  bool hasType() => $_has(0);
  @TagNumber(1)
  void clearType() => clearField(1);

  @TagNumber(2)
  ShapeEntity_ShapeArgs get shape => $_getN(1);
  @TagNumber(2)
  set shape(ShapeEntity_ShapeArgs v) {
    setField(2, v);
  }

  @TagNumber(2)
  bool hasShape() => $_has(1);
  @TagNumber(2)
  void clearShape() => clearField(2);
  @TagNumber(2)
  ShapeEntity_ShapeArgs ensureShape() => $_ensure(1);

  @TagNumber(3)
  ShapeEntity_RectArgs get rect => $_getN(2);
  @TagNumber(3)
  set rect(ShapeEntity_RectArgs v) {
    setField(3, v);
  }

  @TagNumber(3)
  bool hasRect() => $_has(2);
  @TagNumber(3)
  void clearRect() => clearField(3);
  @TagNumber(3)
  ShapeEntity_RectArgs ensureRect() => $_ensure(2);

  @TagNumber(4)
  ShapeEntity_EllipseArgs get ellipse => $_getN(3);
  @TagNumber(4)
  set ellipse(ShapeEntity_EllipseArgs v) {
    setField(4, v);
  }

  @TagNumber(4)
  bool hasEllipse() => $_has(3);
  @TagNumber(4)
  void clearEllipse() => clearField(4);
  @TagNumber(4)
  ShapeEntity_EllipseArgs ensureEllipse() => $_ensure(3);

  @TagNumber(10)
  ShapeEntity_ShapeStyle get styles => $_getN(4);
  @TagNumber(10)
  set styles(ShapeEntity_ShapeStyle v) {
    setField(10, v);
  }

  @TagNumber(10)
  bool hasStyles() => $_has(4);
  @TagNumber(10)
  void clearStyles() => clearField(10);
  @TagNumber(10)
  ShapeEntity_ShapeStyle ensureStyles() => $_ensure(4);

  @TagNumber(11)
  Transform get transform => $_getN(5);
  @TagNumber(11)
  set transform(Transform v) {
    setField(11, v);
  }

  @TagNumber(11)
  bool hasTransform() => $_has(5);
  @TagNumber(11)
  void clearTransform() => clearField(11);
  @TagNumber(11)
  Transform ensureTransform() => $_ensure(5);
}

class FrameEntity extends GeneratedMessage {
  static final BuilderInfo _i = BuilderInfo(const bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'FrameEntity',
      package: const PackageName(
          const bool.fromEnvironment('protobuf.omit_message_names')
              ? ''
              : 'com.opensource.svga'),
      createEmptyInstance: create)
    ..a<double>(
        1,
        const bool.fromEnvironment('protobuf.omit_field_names')
            ? ''
            : 'alpha',
        PbFieldType.OF)
    ..aOM<Layout>(2, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'layout',
        subBuilder: Layout.create)
    ..aOM<Transform>(
        3, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'transform',
        subBuilder: Transform.create)
    ..aOS(4, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'clipPath',
        protoName: 'clipPath')
    ..pc<ShapeEntity>(
        5, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'shapes', PbFieldType.PM,
        subBuilder: ShapeEntity.create)
    ..hasRequiredFields = false;

  FrameEntity._() : super();
  factory FrameEntity({
    double? alpha,
    Layout? layout,
    Transform? transform,
    String? clipPath,
    Iterable<ShapeEntity>? shapes,
  }) {
    final _result = create();
    if (alpha != null) {
      _result.alpha = alpha;
    }
    if (layout != null) {
      _result.layout = layout;
    }
    if (transform != null) {
      _result.transform = transform;
    }
    if (clipPath != null) {
      _result.clipPath = clipPath;
    }
    if (shapes != null) {
      _result.shapes.addAll(shapes);
    }
    return _result;
  }
  factory FrameEntity.fromBuffer(List<int> i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromBuffer(i, r);
  factory FrameEntity.fromJson(String i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromJson(i, r);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
      'Will be removed in next major version')
  FrameEntity clone() => FrameEntity()..mergeFromMessage(this);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
      'Will be removed in next major version')
  FrameEntity copyWith(void Function(FrameEntity) updates) =>
      super.copyWith((message) => updates(message as FrameEntity))
          as FrameEntity; // ignore: deprecated_member_use
  BuilderInfo get info_ => _i;
  @pragma('dart2js:noInline')
  static FrameEntity create() => FrameEntity._();
  FrameEntity createEmptyInstance() => create();
  static PbList<FrameEntity> createRepeated() => PbList<FrameEntity>();
  @pragma('dart2js:noInline')
  static FrameEntity getDefault() => _defaultInstance ??=
      GeneratedMessage.$_defaultFor<FrameEntity>(create);
  static FrameEntity? _defaultInstance;

  @TagNumber(1)
  double get alpha => $_getN(0);
  @TagNumber(1)
  set alpha(double v) {
    $_setFloat(0, v);
  }

  @TagNumber(1)
  bool hasAlpha() => $_has(0);
  @TagNumber(1)
  void clearAlpha() => clearField(1);

  @TagNumber(2)
  Layout get layout => $_getN(1);
  @TagNumber(2)
  set layout(Layout v) {
    setField(2, v);
  }

  @TagNumber(2)
  bool hasLayout() => $_has(1);
  @TagNumber(2)
  void clearLayout() => clearField(2);
  @TagNumber(2)
  Layout ensureLayout() => $_ensure(1);

  @TagNumber(3)
  Transform get transform => $_getN(2);
  @TagNumber(3)
  set transform(Transform v) {
    setField(3, v);
  }

  @TagNumber(3)
  bool hasTransform() => $_has(2);
  @TagNumber(3)
  void clearTransform() => clearField(3);
  @TagNumber(3)
  Transform ensureTransform() => $_ensure(2);

  @TagNumber(4)
  String get clipPath => $_getSZ(3);
  @TagNumber(4)
  set clipPath(String v) {
    $_setString(3, v);
  }

  @TagNumber(4)
  bool hasClipPath() => $_has(3);
  @TagNumber(4)
  void clearClipPath() => clearField(4);

  @TagNumber(5)
  List<ShapeEntity> get shapes => this._shapes ?? $_getList(4);

  List<ShapeEntity>? _shapes;
  set shapes(List<ShapeEntity>? value) => this._shapes = value;
}

class MovieEntity extends GeneratedMessage {
  static final BuilderInfo _i = BuilderInfo(const bool.fromEnvironment('protobuf.omit_message_names') ? '' : 'MovieEntity',
      package: const PackageName(
          const bool.fromEnvironment('protobuf.omit_message_names')
              ? ''
              : 'com.opensource.svga'),
      createEmptyInstance: create)
    ..aOS(
        1,
        const bool.fromEnvironment('protobuf.omit_field_names')
            ? ''
            : 'version')
    ..aOM<MovieParams>(
        2, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'params',
        subBuilder: MovieParams.create)
    ..m<String, List<int>>(
        3, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'images',
        entryClassName: 'MovieEntity.ImagesEntry',
        keyFieldType: PbFieldType.OS,
        valueFieldType: PbFieldType.OY,
        packageName: const PackageName('com.opensource.svga'))
    ..pc<SpriteEntity>(
        4,
        const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'sprites',
        PbFieldType.PM,
        subBuilder: SpriteEntity.create)
    ..pc<AudioEntity>(5, const bool.fromEnvironment('protobuf.omit_field_names') ? '' : 'audios', PbFieldType.PM, subBuilder: AudioEntity.create)
    ..hasRequiredFields = false;

  MovieEntity._() : super();
  factory MovieEntity({
    String? version,
    MovieParams? params,
    Map<String, List<int>>? images,
    Iterable<SpriteEntity>? sprites,
    Iterable<AudioEntity>? audios,
  }) {
    final _result = create();
    if (version != null) {
      _result.version = version;
    }
    if (params != null) {
      _result.params = params;
    }
    if (images != null) {
      _result.images.addAll(images);
    }
    if (sprites != null) {
      _result.sprites.addAll(sprites);
    }
    if (audios != null) {
      _result.audios.addAll(audios);
    }
    return _result;
  }
  factory MovieEntity.fromBuffer(List<int> i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromBuffer(i, r);
  factory MovieEntity.fromJson(String i,
          [ExtensionRegistry r = ExtensionRegistry.EMPTY]) =>
      create()..mergeFromJson(i, r);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.deepCopy] instead. '
      'Will be removed in next major version')
  MovieEntity clone() => MovieEntity()..mergeFromMessage(this);
  @Deprecated('Using this can add significant overhead to your binary. '
      'Use [GeneratedMessageGenericExtensions.rebuild] instead. '
      'Will be removed in next major version')
  MovieEntity copyWith(void Function(MovieEntity) updates) =>
      super.copyWith((message) => updates(message as MovieEntity))
          as MovieEntity; // ignore: deprecated_member_use
  BuilderInfo get info_ => _i;
  @pragma('dart2js:noInline')
  static MovieEntity create() => MovieEntity._();
  MovieEntity createEmptyInstance() => create();
  static PbList<MovieEntity> createRepeated() => PbList<MovieEntity>();
  @pragma('dart2js:noInline')
  static MovieEntity getDefault() => _defaultInstance ??=
      GeneratedMessage.$_defaultFor<MovieEntity>(create);
  static MovieEntity? _defaultInstance;

  @TagNumber(1)
  String get version => $_getSZ(0);
  @TagNumber(1)
  set version(String v) {
    $_setString(0, v);
  }

  @TagNumber(1)
  bool hasVersion() => $_has(0);
  @TagNumber(1)
  void clearVersion() => clearField(1);

  @TagNumber(2)
  MovieParams get params => $_getN(1);
  @TagNumber(2)
  set params(MovieParams v) {
    setField(2, v);
  }

  @TagNumber(2)
  bool hasParams() => $_has(1);
  @TagNumber(2)
  void clearParams() => clearField(2);
  @TagNumber(2)
  MovieParams ensureParams() => $_ensure(1);

  @TagNumber(3)
  Map<String, List<int>> get images => $_getMap(2);

  @TagNumber(4)
  List<SpriteEntity> get sprites => $_getList(3);

  @TagNumber(5)
  List<AudioEntity> get audios => $_getList(4);

  bool autorelease = false;
  SVGADynamicEntity dynamicItem = SVGADynamicEntity();
  Map<String, Image> bitmapCache = {};
  Map<String, Path> pathCache = {};

  void dispose() {
    bitmapCache.values.forEach((element) {
      element.dispose();
    });
    bitmapCache.clear();
    pathCache.clear();
  }
}
