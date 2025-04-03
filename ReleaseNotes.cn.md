# TUILiveKit 发布日志

## 版本3.0

### RoomEngine
#### 新增功能
- 直播类型房间新增支持模糊搜索成员
- LayoutManager 新增自定义布局接口
- 新增屏幕共享暂停接口
- Web 端补齐直播列表接口
- Web 端增加房间 metadata 接口

#### 接口变更
- 错误码变更
- OnRemoteUserEnterRoom/OnRemoteUserLeaveRoom 参数 userInfo 补齐禁言字段
- iOS 端禁言 API 的 swift API 声明变更

#### 修复问题
- 优化屏幕分享异常问题
- 修复 Android偶现崩溃问题
- 修复进房后可能无法打开麦克风问题

### LiveStreamCore
#### Flutter
- 新增直播&语聊组件

#### iOS & Android
- 静态库发布
- State状态可访问，可订阅变更
- 增加接口 switchCamera & enableMirror
- 统一国际化文案
- 修复已知问题

### TUILiveKit
#### Flutter
- 新增语聊房场景
- 统一国际化文案
- 修复已知问题

#### iOS & Android
- 高级特效播放器鉴权问题修复
- iOS 端高级美颜支持使用历史效果
- 新增直播间用户管理功能（禁言、踢出房间、踢下麦、禁止音视频等）
- 删除未完整实现的功能（用户等级、音乐播放、直播类型）
- 修复频繁刷礼物导致直播间异常的问题
- 统一国际化文案
- iOS 端直播列表信息支持定制化修改
- Android 端优化一些特殊机型导致的问题
- 连麦观众进房后自动下麦
- 修复已知问题
