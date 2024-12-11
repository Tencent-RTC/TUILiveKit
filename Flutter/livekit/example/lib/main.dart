import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:tencent_live_uikit/common/index.dart';
import 'package:tencent_live_uikit/tencent_live_uikit.dart';
import 'package:tencent_live_uikit_example/generated/l10n.dart';
import 'package:tencent_live_uikit_example/src/view/index.dart';
import 'package:barrage/barrage.dart';

void main() {
  runApp(const MyApp());
  SystemChrome.setSystemUIOverlayStyle(const SystemUiOverlayStyle(
    statusBarColor: Colors.transparent,
  ));
}

class MyApp extends StatefulWidget {
  const MyApp({super.key});

  @override
  State<MyApp> createState() => _MyAppState();
}

class _MyAppState extends State<MyApp> {
  @override
  void initState() {
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    S.load(View.of(context).platformDispatcher.locale);
    return MaterialApp(
        navigatorObservers: [
          TUILiveKitNavigatorObserver.instance
        ],
        localizationsDelegates: const [
          ...LiveKitLocalizations.localizationsDelegates,
          ...BarrageLocalizations.localizationsDelegates
        ],
        supportedLocales: [
          ...S.delegate.supportedLocales,
          ...LiveKitLocalizations.supportedLocales,
          ...BarrageLocalizations.supportedLocales
        ],
        builder: (context, child) => Scaffold(
              resizeToAvoidBottomInset: false,
              body: GestureDetector(
                onTap: () {
                  hideKeyboard(context);
                },
                child: child,
              ),
            ),
        home: const LoginWidget());
  }

  void hideKeyboard(BuildContext context) {
    FocusScopeNode currentFocus = FocusScope.of(context);
    if (!currentFocus.hasPrimaryFocus && currentFocus.focusedChild != null) {
      FocusManager.instance.primaryFocus?.unfocus();
    }
  }
}
