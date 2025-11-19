import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:tencent_live_uikit/common/widget/global.dart';
import 'package:tencent_live_uikit/tencent_live_uikit.dart';
import 'package:tencent_live_uikit_example/generated/l10n.dart';
import 'package:tencent_live_uikit_example/src/view/index.dart';

void main() {
  runApp(const MyApp());
  SystemChrome.setSystemUIOverlayStyle(const SystemUiOverlayStyle(
    statusBarColor: Colors.transparent,
  ));
  SystemChrome.setPreferredOrientations([
    DeviceOrientation.portraitUp,
    DeviceOrientation.portraitDown,
  ]);
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
        debugShowCheckedModeBanner: false,
        navigatorObservers: [TUILiveKitNavigatorObserver.instance],
        localizationsDelegates: const [
          ...LiveKitLocalizations.localizationsDelegates,
          ...BarrageLocalizations.localizationsDelegates,
          ...GiftLocalizations.localizationsDelegates,
        ],
        supportedLocales: const [
          Locale('en'),
          Locale.fromSubtags(languageCode: 'zh', scriptCode: 'Hans'),
          Locale.fromSubtags(languageCode: 'zh', scriptCode: 'Hant'),
          Locale('zh'),
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
        home: Navigator(
          key: Global.secondaryNavigatorKey,
          onGenerateRoute: (settings) => MaterialPageRoute(builder: (BuildContext context) {
            return const LoginWidget();
          }),
        )
    );
  }

  void hideKeyboard(BuildContext context) {
    FocusScopeNode currentFocus = FocusScope.of(context);
    if (!currentFocus.hasPrimaryFocus && currentFocus.focusedChild != null) {
      FocusManager.instance.primaryFocus?.unfocus();
    }
  }
}
