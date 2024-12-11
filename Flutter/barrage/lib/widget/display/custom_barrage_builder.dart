import 'package:barrage/state/index.dart';
import 'package:flutter/material.dart';

abstract class CustomBarrageBuilder {
  bool shouldCustomizeBarrageItem(Barrage barrage);

  Widget buildWidget(BuildContext context, Barrage barrage);
}
