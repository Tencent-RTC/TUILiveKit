import 'livekit_localizations.dart';

/// The translations for Chinese (`zh`).
class LiveKitLocalizationsZh extends LiveKitLocalizations {
  LiveKitLocalizationsZh([String locale = 'zh']) : super(locale);

  @override
  String get livekit_error_success => '操作成功';

  @override
  String get livekit_error_failed => '操作错误';

  @override
  String get livekit_error_freqLimit => '请求被限频，请稍后重试';

  @override
  String get livekit_error_repeat_operation => '重复操作';

  @override
  String get livekit_error_sdkAppId_notFound =>
      '未找到 SDKAppID，请在腾讯云视立方 SDK[控制台](https://console_cloud_tencent_com/vcube/project/manage)确认应用信息';

  @override
  String get livekit_error_invalidParameter => '调用 API 时，传入的参数不合法，检查入参是否合法';

  @override
  String get livekit_error_sdkNotInitialized => '未登录,请调用 Login 接口';

  @override
  String get livekit_error_permissionDenied => '获取权限失败，当前未授权音/视频权限，请查看是否开启设备权限';

  @override
  String get livekit_error_requirePayment => '检查云服务账户是否正常';

  @override
  String get livekit_error_cameraStartFail => '系统问题，打开摄像头失败。检查摄像头设备是否正常';

  @override
  String get livekit_error_cameraNotAuthorized => '摄像头没有系统授权, 检查系统授权';

  @override
  String get livekit_error_cameraOccupied => '摄像头被占用，检查是否有其他进程使用摄像头';

  @override
  String get livekit_error_cameraDeviceEmpty => '当前无摄像头设备，请插入摄像头设备解决该问题';

  @override
  String get livekit_error_microphoneStartFail => '系统问题，打开麦克风失败。检查麦克风设备是否正常';

  @override
  String get livekit_error_microphoneNotAuthorized => '麦克风没有系统授权，检查系统授权';

  @override
  String get livekit_error_microphoneOccupied => '麦克风被占用';

  @override
  String get livekit_error_microphoneDeviceEmpty => '当前无麦克风设备';

  @override
  String get livekit_error_getScreenSharingTargetFailed =>
      '获取屏幕分享源（屏幕和窗口）失败，检查屏幕录制权限';

  @override
  String get livekit_error_startScreenSharingFailed =>
      '开启屏幕分享失败，检查房间内是否有人正在屏幕分享';

  @override
  String get livekit_error_roomId_notExist => '进房时房间不存在，或许已被解散';

  @override
  String get livekit_error_operation_invalid_beforeEnterRoom => '需要进房后才可使用此功能';

  @override
  String get livekit_error_exitNotSupported_forRoomOwner => '请转让房主后再退房';

  @override
  String get livekit_error_operation_notSupported_inCurrentRoomType =>
      '当前房间类型下不支持该操作';

  @override
  String get livekit_error_operation_notSupported_inCurrentSpeechMode =>
      '当前发言模式下不支持该操作';

  @override
  String get livekit_error_roomId_invalid =>
      '创建房间 ID 非法，自定义 ID 必须为可打印 ASCII 字符（0x20-0x7e），最长 48 个字节';

  @override
  String get livekit_error_roomId_occupied => '房间 ID 已被使用，请选择别的房间 ID';

  @override
  String get livekit_error_roomName_invalid =>
      '房间名称非法，名称最长 30 字节，字符编码必须是 UTF-8，如果包含中文';

  @override
  String get livekit_error_already_in_OtherRoom => '当前用户已在别的房间内，需要先退房才能加入新的房间';

  @override
  String get livekit_error_userNotExist => '用户不存在';

  @override
  String get livekit_error_userNotEntered => '用户不在当前房间内';

  @override
  String get livekit_error_user_need_OwnerPermission => '需要房主权限才能操作';

  @override
  String get livekit_error_user_need_AdminPermission => '需要房主或者管理员权限才能操作';

  @override
  String get livekit_error_request_noPermission => '信令请求无权限，例如取消非自己发起的邀请';

  @override
  String get livekit_error_requestId_invalid => '信令请求 ID 无效或已经被处理过';

  @override
  String get livekit_error_repeat_requestId => '信令请求重复';

  @override
  String get livekit_error_conflict_requestId => '信令请求冲突';

  @override
  String get livekit_error_max_seat_count_limit => '最大麦位超出套餐包数量限制';

  @override
  String get livekit_error_already_in_seat => '当前用户已经在麦位上';

  @override
  String get livekit_error_seat_occupied => '当前麦位已经有人了';

  @override
  String get livekit_error_seat_locked => '当前麦位被锁';

  @override
  String get livekit_error_seat_index_not_exist => '麦位编号不存在';

  @override
  String get livekit_error_user_not_in_seat => '当前用户没有在麦上';

  @override
  String get livekit_error_all_seat_occupied => '上麦人数已满';

  @override
  String get livekit_error_seat_not_support_link_mic => '不支持连麦';

  @override
  String get livekit_error_open_microphone_need_seat_unlock =>
      '当前麦位音频被锁，需要解锁后才能打开麦克风';

  @override
  String get livekit_error_open_microphone_need_permission_from_admin =>
      '需要向房主或管理员申请后打开麦克风';

  @override
  String get livekit_error_open_camera_need_seat_unlock =>
      '当前麦位视频被锁，需要解锁后才能打开摄像头';

  @override
  String get livekit_error_open_camera_need_permission_from_admin =>
      '需要向房主或管理员申请后打开摄像头';

  @override
  String get livekit_error_open_screen_share_need_seat_unlock =>
      '当前麦位屏幕分享被锁，需要解锁后才能打开屏幕分享';

  @override
  String get livekit_error_open_screen_share_need_permission_from_admin =>
      '需要向房主或管理员申请后打开屏幕分享';

  @override
  String get livekit_error_send_message_disabled_for_all => '当前房间已开启全员禁言';

  @override
  String get livekit_error_send_message_disabled_for_current => '当前房间内，您已被禁言';

  @override
  String get livekit_take_seat_rejected => '上麦申请被拒绝';

  @override
  String get livekit_room_destroy => '直播已结束';

  @override
  String get livekit_kicked_out_of_seat => '被主持人踢下麦位';

  @override
  String get livekit_take_seat_timeout => '上麦申请超时';

  @override
  String get livekit_anchor => '主播';

  @override
  String get livekit_audience => '观众';

  @override
  String get livekit_edit_cover => '修改封面';

  @override
  String get livekit_stream_categories => '直播分类：';

  @override
  String get livekit_stream_privacy_status => '直播模式：';

  @override
  String get livekit_stream_privacy_status_public => '公开';

  @override
  String get livekit_stream_privacy_status_privacy => '隐私';

  @override
  String get livekit_stream_categories_daily_chat => '日常聊天';

  @override
  String get livekit_stream_categories_appearance => '颜值';

  @override
  String get livekit_stream_categories_knowledge_teaching => '知识教学';

  @override
  String get livekit_stream_categories_shopping => '购物';

  @override
  String get livekit_stream_categories_music => '音乐';

  @override
  String get livekit_function_item_beauty => '美颜';

  @override
  String get livekit_function_item_music => '音乐';

  @override
  String get livekit_function_item_flip => '翻转';

  @override
  String get livekit_function_item_mirror => '镜像';

  @override
  String get livekit_start_live => '开始直播';

  @override
  String get livekit_preset_cover => '系统图库';

  @override
  String get livekit_set_as_cover => '设为封面';

  @override
  String get livekit_stream_type => '直播分类';

  @override
  String get livekit_stream_id => '直播间号';

  @override
  String get livekit_barrage_input_hint => '请输入弹幕';

  @override
  String get livekit_barrage_btn_send => '发送';

  @override
  String get livekit_barrage_me => '我';

  @override
  String get livekit_barrage_agree => '同意';

  @override
  String get livekit_barrage_warning_not_empty => '输入不能为空';

  @override
  String get livekit_settings => '设置';

  @override
  String get livekit_audio_effect => '音效';

  @override
  String get livekit_more_settings => '更多设置';

  @override
  String get livekit_video_params => '视频参数';

  @override
  String get livekit_video_config => '视频参数';

  @override
  String get livekit_link => '连麦';

  @override
  String get livekit_pk => '主播PK';

  @override
  String get livekit_send_gift => '送礼物';

  @override
  String get livekit_like => '点赞';

  @override
  String get livekit_comment => '评论';

  @override
  String get livekit_hidden_audience_nickname => '隐藏观众昵称';

  @override
  String get livekit_allow_share => '允许分享';

  @override
  String get livekit_waiting_pass => '待通过';

  @override
  String get livekit_gift_give_gift => '送出';

  @override
  String get livekit_gift_title => '礼物';

  @override
  String get livekit_gift_me => '我';

  @override
  String get livekit_music => '音乐';

  @override
  String get livekit_music_cheerful => '欢快';

  @override
  String get livekit_music_melancholy => '忧郁';

  @override
  String get livekit_music_wonder_world => '神奇世界';

  @override
  String get livekit_clarity => '清晰度';

  @override
  String get livekit_fps => '帧率';

  @override
  String get livekit_ear_return => '耳返';

  @override
  String get livekit_ear_return_volume => '耳返音量';

  @override
  String get livekit_background_music => '背景音乐';

  @override
  String get livekit_select_music => '选择音乐';

  @override
  String get livekit_audio_settings => '音频设置';

  @override
  String get livekit_music_volume => '音乐音量';

  @override
  String get livekit_people_volume => '人声音量';

  @override
  String get livekit_music_pitch => '音乐升降调';

  @override
  String get livekit_change_voice => '变声';

  @override
  String get livekit_change_voice_none => '原声';

  @override
  String get livekit_change_voice_child => '熊孩子';

  @override
  String get livekit_change_voice_girl => '萝莉';

  @override
  String get livekit_change_voice_uncle => '大叔';

  @override
  String get livekit_change_voice_metal => '重金属';

  @override
  String get livekit_change_voice_cold => '感冒';

  @override
  String get livekit_change_voice_foreign_language => '外语腔';

  @override
  String get livekit_change_voice_trapped_beast => '困兽';

  @override
  String get livekit_change_voice_fat_house => '肥宅';

  @override
  String get livekit_change_voice_strong_current => '强电流';

  @override
  String get livekit_change_voice_machinery => '重机械';

  @override
  String get livekit_change_voice_ethereal => '空灵';

  @override
  String get livekit_reverb => '混响';

  @override
  String get livekit_reverb_none => '无效果';

  @override
  String get livekit_reverb_karaoke => 'KTV';

  @override
  String get livekit_reverb_small_room => '小房间';

  @override
  String get livekit_reverb_town_hall => '大会堂';

  @override
  String get livekit_reverb_low => '低沉';

  @override
  String get livekit_reverb_loud_and_loud => '洪亮';

  @override
  String get livekit_reverb_metallic_sound => '金属声';

  @override
  String get livekit_reverb_metallic_magnetic => '磁性';

  @override
  String get livekit_reverb_metallic_ethereal => '空灵';

  @override
  String get livekit_reverb_metallic_recording_studio => '录音棚';

  @override
  String get livekit_reverb_metallic_melodious => '悠扬';

  @override
  String get livekit_reverb_metallic_recording_studio2 => '录音棚2';

  @override
  String get livekit_tips_title => '温馨提示';

  @override
  String get livekit_confirm_delete_tips => '确认要删除\"%s\"吗?';

  @override
  String get livekit_cancel => '取消';

  @override
  String get livekit_confirm => '确认';

  @override
  String get livekit_waiting_link => '等待连麦';

  @override
  String get livekit_live_room_list => '直播';

  @override
  String get livekit_live_has_stop => '直播已结束';

  @override
  String get livekit_sent => '送给';

  @override
  String get livekit_entered_room => '进入房间';

  @override
  String get livekit_who_live_room => '的直播间';

  @override
  String get livekit_gift_balance => '余额';

  @override
  String get livekit_gift_recharge => '充值';

  @override
  String get livekit_gift_balance_insufficient => '余额不足';

  @override
  String get livekit_preview_video_live => '视频直播';

  @override
  String get livekit_preview_voice_live => '语音直播';

  @override
  String get livekit_audience_count_in_room => '%d人正在观看';

  @override
  String get livekit_loading => '正在加载...';

  @override
  String get livekit_no_more_data => '没有更多数据了';

  @override
  String get livekit_no_room_tip => '请先开启一个直播间';

  @override
  String get livekit_common_this_live_data => '本场直播数据';

  @override
  String get livekit_common_live_duration => '直播时长';

  @override
  String get livekit_common_gift_income => '礼物收入';

  @override
  String get livekit_common_live_people_number => '观看人数';

  @override
  String get livekit_common_message_count => '消息数量';

  @override
  String get livekit_common_send_gift_people_count => '送礼人数';

  @override
  String get livekit_common_like_count => '点赞数量';

  @override
  String get livekit_anchor_audience_list_panel_title => '在线观众';

  @override
  String get livekit_follow_anchor => '关注';

  @override
  String get livekit_unfollow_anchor => '取消关注';

  @override
  String get livekit_live_room_id => '直播房间ID: ';

  @override
  String get livekit_fan_count => '粉丝';

  @override
  String get livekit_beauty_panel_title => '一键美颜';

  @override
  String get livekit_beauty_item_none => '关闭';

  @override
  String get livekit_beauty_item_smooth => '磨皮';

  @override
  String get livekit_beauty_item_whiteness => '美白';

  @override
  String get livekit_beauty_item_ruddy => '红润';

  @override
  String get livekit_resolution_360p => '低清';

  @override
  String get livekit_resolution_540p => '标清';

  @override
  String get livekit_resolution_720p => '高清';

  @override
  String get livekit_resolution_1080p => '超清';

  @override
  String get livekit_enable_audience_request_link => '允许观众申请连麦';

  @override
  String get livekit_link_mic_up_title => '当前麦位';

  @override
  String get livekit_link_mic_down_title => '连麦申请';

  @override
  String get livekit_link_mic_down_title_popup => '连麦申请';

  @override
  String get livekit_title_link_mic_selector => '选择连麦方式';

  @override
  String get livekit_text_link_mic_selector => '选择连麦方式，主播同意后接通';

  @override
  String get livekit_text_link_mic_video => '申请视频连麦';

  @override
  String get livekit_text_link_mic_audio => '申请语音连麦';

  @override
  String get livekit_text_cancel_link_mic_apply => '取消连麦申请';

  @override
  String get livekit_text_close_link_mic => '结束连麦';

  @override
  String get livekit_link_mic_manager => '连麦管理';

  @override
  String get livekit_hang_up => '挂断';

  @override
  String get livekit_accept => '同意';

  @override
  String get livekit_reject => '拒绝';

  @override
  String get livekit_toast_apply_link_mic => '你提交了连麦申请，请等待主播同意';

  @override
  String get livekit_title_link_video_settings => '调整视频连麦画面';

  @override
  String get livekit_btn_apply_link_mic => '申请连麦';

  @override
  String get livekit_tips_apply_link_mic => '接通后画面效果将自动生效';
}
