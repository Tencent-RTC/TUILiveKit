# 接入腾讯美颜特效 示例中接入的是 s1-07 sdk版本
参考接入文档：https://cloud.tencent.com/document/product/616/81197

1、在Example 中 修改 license 对应 包名 和 bundleID
Android 目录在：example/android/app/build.gradle 文件中，applicationId 属性的值。
iOS: 在xcode 中打开 example/ios/Runner.xcworkspace 工程，在 xcode 中配置。
2、在插件模块的 Constants 类中替换 licenseUrl 和 licenseKey
3、在 Example 模块 给 GenerateTestUserSig 类中 为 sdkAppId 和 secretKey 赋值。


