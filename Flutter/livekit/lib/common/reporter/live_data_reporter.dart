import 'dart:convert';

import 'package:flutter/foundation.dart';
import 'package:live_stream_core/live_core_widget/live_core_controller.dart';

import '../constants/index.dart';

class LiveDataReporter {
  static void reportComponent(LiveComponentType componentType) {
    var component = Constants.dataReportComponentLiveRoom;
    switch (componentType) {
      case LiveComponentType.liveRoom:
        component = Constants.dataReportComponentLiveRoom;
        break;
      case LiveComponentType.voiceRoom:
        component = Constants.dataReportComponentVoiceRoom;
        break;
    }

    try {
      Map<String, dynamic> params = {
        'framework': Constants.dataReportFramework,
        'component': component,
        'language': Constants.dataReportLanguageFlutter,
      };

      Map<String, dynamic> jsonObject = {
        'api': 'setFramework',
        'params': params,
      };

      String jsonString = jsonEncode(jsonObject);
      LiveCoreController.callExperimentalAPI(jsonString);
    } catch (e) {
      debugPrint('Error reporting component');
    }
  }
}

enum LiveComponentType { liveRoom, voiceRoom }
