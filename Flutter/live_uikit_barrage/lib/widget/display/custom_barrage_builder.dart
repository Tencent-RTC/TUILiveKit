import 'package:flutter/material.dart';

import '../../state/index.dart';

abstract class CustomBarrageBuilder {
  bool shouldCustomizeBarrageItem(Barrage barrage);

  Widget buildWidget(BuildContext context, Barrage barrage);
}
