import 'dart:core';
import 'package:protobuf/protobuf.dart';

class ShapeEntityType extends ProtobufEnum {
  static const ShapeEntityType SHAPE = ShapeEntityType._(
      0,
      bool.fromEnvironment('protobuf.omit_enum_names')
          ? ''
          : 'SHAPE');
  static const ShapeEntityType RECT = ShapeEntityType._(
      1,
      bool.fromEnvironment('protobuf.omit_enum_names')
          ? ''
          : 'RECT');
  static const ShapeEntityType ELLIPSE = ShapeEntityType._(
      2,
      bool.fromEnvironment('protobuf.omit_enum_names')
          ? ''
          : 'ELLIPSE');
  static const ShapeEntityType KEEP = ShapeEntityType._(
      3,
      bool.fromEnvironment('protobuf.omit_enum_names')
          ? ''
          : 'KEEP');

  static const List<ShapeEntityType> values =
      <ShapeEntityType>[
    SHAPE,
    RECT,
    ELLIPSE,
    KEEP,
  ];

  static final Map<int, ShapeEntityType> _byValue =
      ProtobufEnum.initByValue(values);
  static ShapeEntityType? valueOf(int value) => _byValue[value];

  const ShapeEntityType._(int v, String n) : super(v, n);
}

class ShapeEntityStyleLineCap extends ProtobufEnum {
  static const ShapeEntityStyleLineCap LineCap_BUTT =
      ShapeEntityStyleLineCap._(
          0,
          bool.fromEnvironment('protobuf.omit_enum_names')
              ? ''
              : 'LineCap_BUTT');
  static const ShapeEntityStyleLineCap LineCap_ROUND =
      ShapeEntityStyleLineCap._(
          1,
          bool.fromEnvironment('protobuf.omit_enum_names')
              ? ''
              : 'LineCap_ROUND');
  static const ShapeEntityStyleLineCap LineCap_SQUARE =
      ShapeEntityStyleLineCap._(
          2,
          bool.fromEnvironment('protobuf.omit_enum_names')
              ? ''
              : 'LineCap_SQUARE');

  static const List<ShapeEntityStyleLineCap> values =
      <ShapeEntityStyleLineCap>[
    LineCap_BUTT,
    LineCap_ROUND,
    LineCap_SQUARE,
  ];

  static final Map<int, ShapeEntityStyleLineCap> _byValue =
      ProtobufEnum.initByValue(values);
  static ShapeEntityStyleLineCap? valueOf(int value) =>
      _byValue[value];

  const ShapeEntityStyleLineCap._(int v, String n)
      : super(v, n);
}

class ShapeEntityStyleLineJoin extends ProtobufEnum {
  static const ShapeEntityStyleLineJoin LineJoin_MITER =
      ShapeEntityStyleLineJoin._(
          0,
          bool.fromEnvironment('protobuf.omit_enum_names')
              ? ''
              : 'LineJoin_MITER');
  static const ShapeEntityStyleLineJoin LineJoin_ROUND =
      ShapeEntityStyleLineJoin._(
          1,
          bool.fromEnvironment('protobuf.omit_enum_names')
              ? ''
              : 'LineJoin_ROUND');
  static const ShapeEntityStyleLineJoin LineJoin_BEVEL =
      ShapeEntityStyleLineJoin._(
          2,
          bool.fromEnvironment('protobuf.omit_enum_names')
              ? ''
              : 'LineJoin_BEVEL');

  static const List<ShapeEntityStyleLineJoin> values =
      <ShapeEntityStyleLineJoin>[
    LineJoin_MITER,
    LineJoin_ROUND,
    LineJoin_BEVEL,
  ];

  static final Map<int, ShapeEntityStyleLineJoin> _byValue =
      ProtobufEnum.initByValue(values);
  static ShapeEntityStyleLineJoin? valueOf(int value) =>
      _byValue[value];

  const ShapeEntityStyleLineJoin._(int v, String n)
      : super(v, n);
}
