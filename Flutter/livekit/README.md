# TUILiveKit

_English | [简体中文](README-zh_CN.md)_

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left" width=65 height=65>

TUILiveKit is a real-time interactive live streaming component that includes features such as host streaming, audience viewing, bullet chat, likes and gifts, audience management, and co-hosting management. It is suitable for live streaming scenarios such as entertainment, e-commerce, and education. By integrating TUILiveKit, you can add all the above live streaming features to your application in just three steps and launch your business quickly within 30 minutes.

## Environment Preparation

### Flutter

- Flutter 3.27.4 or higher.
- Dart 3.6.2 or higher.

### Android

- Android Studio 3.5 or higher.
- Android devices with Android 5.0 or higher.

### iOS

- Xcode 15.0 or higher.
- Ensure your project has a valid developer signature.

## Getting Started

### Add Dependencies

Follow the documentation to add the `tencent_live_uikit` package as a dependency in your `pubspec.yaml` file ([Installation Guide](https://pub.flutter-io.cn/packages/tencent_live_uikit/install)).

### Activate Services

To use the interactive live streaming features, ensure that you have activated the service.

1. **Activate the Service**  
   You can activate the service and obtain your `SDKAppID` and `SDKSecretKey` in the [Console](https://trtc.io/document/60033?platform=flutter&product=live&menulabel=uikit).

2. **Configure SDKAppID and SDKSecretKey**  
   Open the `example/lib/debug/generate_test_user_sig.dart` file and fill in the obtained `SDKAppID` and `SDKSecretKey`:

   ```dart
   static int sdkAppId = 0; // Replace with your SDKAppID
   static String secretKey = ''; // Replace with your SDKSecretKey
   ```

## Example Experience

After configuring the `SDKAppID` and `SDKSecretKey`, you can directly run the example project to experience the features.  
> For a better experience, it is recommended to prepare two mobile devices: one for the host to start the live stream and the other for the audience to watch or listen.

## Quick Integration

### Configure Routing and Internationalization

```dart
return MaterialApp(
   navigatorObservers: TUILiveKitNavigatorObserver.instance,
   localizationsDelegates: [
      ...LiveKitLocalizations.localizationsDelegates,
      ...BarrageLocalizations.localizationsDelegates,
      ...GiftLocalizations.localizationsDelegates,
   ],
   supportedLocales: [
      ...LiveKitLocalizations.supportedLocales,
      ...BarrageLocalizations.supportedLocales,
      ...GiftLocalizations.supportedLocales
   ],
   //...
);
```

### Login

Before using the interactive live streaming features, ensure that you have completed the initialization by executing the following login code.

```dart
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

final int sdkAppId = 'replace with your sdkAppId';
final String userId = 'replace with your userId';
final String userSig = 'replace with your userSig';

await TUILogin.instance.login(
   sdkAppId,
   userId,
   userSig,
   TUICallback(onSuccess: () async {
      debugPrint("TUILogin login success");
   }, onError: (code, message) {
      debugPrint("TUILogin login fail, {code:$code, message:$message}");
   }));
```

> - **sdkAppId**: You can obtain it from the [Console](https://trtc.io/document/60033?platform=flutter&product=live&menulabel=uikit) after activating the service.  
> - **userSig**: Generate it using the [UserSig Guide](https://trtc.io/document/35166?platform=flutter&product=live&menulabel=uikit).

### Host Streaming Page Integration

You can use the `Navigator.push` method to navigate to the host streaming page.

For video streaming, refer to the following code:

```dart
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

Navigator.push(context, MaterialPageRoute(
   builder: (context) {
      final String userId = 'replace with your userId';
      final String roomId = LiveIdentityGenerator.instance.generateId(userId, RoomType.live);
      return TUILiveRoomAnchorWidget(roomId: roomId);
   }));
```

For audio chat room streaming, refer to the following code:

```dart
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

Navigator.push(context, MaterialPageRoute(
   builder: (context) {
      final String userId = 'replace with your userId';
      final String roomId = LiveIdentityGenerator.instance.generateId(userId, RoomType.live);
      final params = RoomParams();
      params.maxSeatCount = 10; // Set the number of seats (maximum value depends on your subscription plan).
      return TUIVoiceRoomWidget(
                              roomId: roomId,
                              behavior: RoomBehavior.prepareCreate,
                              params: params);
   }));
```

> The maximum value of `maxSeatCount` can be found in the [Multi-guests in Package Details](https://trtc.io/document/59407?platform=flutter&product=live&menulabel=uikit#658e2423-30d2-45e2-91b8-128b2730b072).

### Live Room List Page Integration

The live room list page displays ongoing video live streams and audio chat rooms. You can click on any room to join as an audience member and watch or listen.  
You can use the `Navigator.push` method to navigate to the live room list page.  
Reference code:

```dart
Navigator.push(context, MaterialPageRoute(
   builder: (context) {
      return Scaffold(
         body: SafeArea(child: LiveListWidget()));
   }));
```

You can also directly add the live room list page as a child widget in your existing page.  
Reference code:

```dart
// Single child widget, using Container as an example
Container(
   child: LiveListWidget()
)

// Multiple child widget, using Column as an example
Column(
   children: [LiveListWidget()]
)
```

## Feedback and Support

If you have any questions or feedback, please contact us at: <info_rtc@tencent.com>.
