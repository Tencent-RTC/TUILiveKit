# TUILiveKit

_[English](README.md) | 简体中文_

<img src="https://qcloudimg.tencent-cloud.cn/raw/ec034fc6e4cf42cae579d32f5ab434a1.png" align="left" width=65 height=65>
TUILiveKit 是一款实时互动直播组件，包含主播开播、观众观看、弹幕聊天、点赞送礼、观众管理、连麦管理等功能，适用于娱乐、电商、教育等直播场景；通过集成
TUILiveKit ，仅需三步，即可在30分钟内为您的应用添加上诉所有直播功能，快速上线业务。

## 环境准备

### Flutter

- Flutter 3.27.4 或更高版本。
- Dart 3.6.2 或更高的版本。

### Android

- Android Studio 3.5及以上版本。
- Android 5.0 及以上版本的 Android 设备。

### iOS

- Xcode 15.0及以上版本。
- 请确保您的项目已设置有效的开发者签名。

## 开始使用

### 添加依赖

依照文档将 ```tencent_live_uikit```
包添加为 [pubspec 的依赖](https://pub.flutter-io.cn/packages/tencent_live_uikit/install)。

### 激活服务

为使用互动直播的功能，您需要确保您已激活服务。

1. **开通服务**
   您可在 [控制台](https://trtc.io/zh/document/60033?platform=flutter&product=live&menulabel=uikit)
   开通服务并获取 ```SDKAppID``` 和 ```SDKSecretKey``` 。
2. **配置 SDKAppID 和 SDKSecretKey**
   打开 ```example/lib/debug/generate_test_user_sig.dart``` 文件，将获取到的 ```SDKAppID```
   和 ```SDKSecretKey``` 填入其中：

    ```dart
    static int sdkAppId = 0; // 替换成您已开通应用的SDKAppID
    static String secretKey = ''; // 替换成您已开通应用的SDKSecretKey
    ```

## 示例体验

在配置完 ```SDKAppID``` 和 ```SDKSecretKey```后，您可直接运行 example 工程体验效果
> 为了您能更好的体验互动直播，建议您准备两台移动设备，一台用于主播开播，一台用于观看观看或收听。

## 快速接入

### 配置路由和国际化

```dart
return MaterialApp(
   navigatorObservers: [TUILiveKitNavigatorObserver.instance],
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

### 登录

在使用互动直播功能前，请确保您已执行如下登录代码完成初始化工作。

```dart
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

final int sdkAppId = 'replace with your sdkAppId';
final String userId = 'replace with your userId';
final String userSig = 'replace with your userSig'

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

> - sdkAppId获取方式：您可在 [控制台](https://trtc.io/zh/document/60033?platform=flutter&product=live&menulabel=uikit) 开通服务并获取 SDKAppID
> - [userSig生成](https://trtc.io/zh/document/35166?platform=flutter&product=live&menulabel=uikit)

### 主播开播页面接入

您可使用 ```Navigator.push``` 方法路由到主播页面

若您是视频开播可参考：

```dart
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

Navigator.push(context, MaterialPageRoute(
   builder: (context) {
      final String userId = 'replace with your userId';
      final String roomId = LiveIdentityGenerator.instance.generateId(userId, RoomType.live)
      return TUILiveRoomAnchorWidget(roomId: roomId);
   }));
```

若您是语音聊天室开播可参考：

```dart
import 'package:tencent_live_uikit/tencent_live_uikit.dart';

Navigator.push(context, MaterialPageRoute(
   builder: (context) {
      final String userId = 'replace with your userId';
      final String roomId = LiveIdentityGenerator.instance.generateId(userId, RoomType.live)
      final params = RoomParams();
      params.maxSeatCount = 10; // 这里是您想要展示的麦位数，最大值不超过您套餐所支持的最大麦位数
      return TUIVoiceRoomWidget(
                              roomId: roomId,
                              behavior: RoomBehavior.prepareCreate,
                              params: params);
   }));
```

> maxSeatCount最大值可参考[套餐明细的连麦数](https://trtc.io/zh/document/59407?platform=flutter&product=live&menulabel=uikit#3f34c3ef-7f0c-486c-be7a-dabaa34b3591)

### 直播间列表页面接入

直播间列表页面中将会展示已开播的视频直播和语音聊天室，您可点击任意直播间并已观众身份加入直播间进行收听和观看。
您可使用 ```Navigator.push``` 方法路由到直播间列表页面
参考代码：

```dart
Navigator.push(context, MaterialPageRoute(
      builder: (context) {
         return Scaffold(
            body: SafeArea(child: LiveListWidget()));
      }));
```

您也可直接将直播间列表页面添加为您某个页面的子空间
参考代码：

```dart

// 单子组件，以Container为例
Container(
   child: LiveListWidget()
)

// 多子组件，以Column为例
Column(
   children:[LiveListWidget()]
)
```

## 交流反馈

如果有任何需要或者反馈，您可以联系：<info_rtc@tencent.com>。
