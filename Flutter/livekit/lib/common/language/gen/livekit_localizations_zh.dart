import 'livekit_localizations.dart';

// ignore_for_file: type=lint

/// The translations for Chinese (`zh`).
class LiveKitLocalizationsZh extends LiveKitLocalizations {
  LiveKitLocalizationsZh([String locale = 'zh']) : super(locale);

  @override
  String get common_disconnect_guest_tips => '确定要断开xxx的连麦吗？';

  @override
  String get common_anchor_battle => '主播PK';

  @override
  String get common_anchor_end_link_tips => '当前处于连麦状态，是否需要「关闭直播间」';

  @override
  String get common_battle_connecting => '连线中';

  @override
  String get common_battle_end_pk => '结束PK';

  @override
  String get common_battle_end_pk_tips => '确定要结束PK吗？结束后将以当前结果为最终结果';

  @override
  String get common_battle_invitation_timeout => 'PK邀请已超时';

  @override
  String get common_battle_invitee_reject => 'xxx拒绝了 PK';

  @override
  String get common_battle_inviter_cancel => 'xxx取消了PK，请尝试再次发起';

  @override
  String get common_battle_inviting => 'xxx向你发来PK邀请';

  @override
  String get common_battle_pk_end => 'PK结束';

  @override
  String get common_battle_wait_start => '等待同意PK';

  @override
  String get common_battle_wait_stop => '取消';

  @override
  String get common_connect_conflict => '被邀请连线的房间与其他房间连线中。';

  @override
  String get common_connect_error => '其他错误，无法发起连线';

  @override
  String get common_connect_inviting => '邀请中';

  @override
  String get common_connect_inviting_append => 'xxx向你发来连线邀请';

  @override
  String get common_connect_request_rejected => '连线申请被拒绝';

  @override
  String get common_connect_invitation_timeout => '邀请已超时';

  @override
  String get common_connection => '发起连线';

  @override
  String get common_connection_list_title => '连线中(xxx)';

  @override
  String get common_connection_room_full => '当前连线人数已达最大限制。';

  @override
  String get common_cover => '封面';

  @override
  String get common_disable_audio => '禁用音频';

  @override
  String get common_disable_message => '禁止发言';

  @override
  String get common_disable_video => '禁用视频';

  @override
  String get common_disconnect_tips => '确定要断开与其他主播的连线吗?';

  @override
  String get common_enable_audio => '启用音频';

  @override
  String get common_enable_message => '解除禁言';

  @override
  String get common_enable_video => '启用视频';

  @override
  String get common_end_connect => '断开连线';

  @override
  String get common_end_connection_tips => '当前处于连线状态，是否需要「断开连线」或「关闭直播间」';

  @override
  String get common_end_pk_tips => '当前处于PK状态，是否需要「结束PK」或「关闭直播间」';

  @override
  String get common_kick_out_of_room => '踢出房间';

  @override
  String get common_kicked_out_of_room_by_owner => '您已被踢出房间';

  @override
  String get common_more => '更多';

  @override
  String get common_more_features => '更多功能';

  @override
  String get common_mute_audio_by_master => '您已被主播静音';

  @override
  String get common_mute_video_by_owner => '您已被主播关闭摄像头';

  @override
  String get common_recommended_list => '推荐列表';

  @override
  String get common_link_mic_up_title => '当前麦位';

  @override
  String get common_send_message_disabled => '当前房间内，您已被禁言';

  @override
  String get common_send_message_enable => '当前房间内，您已被解除禁言';

  @override
  String get common_start_live => '开始直播';

  @override
  String get common_start_video => '开启视频';

  @override
  String get common_stop_video => '关闭视频';

  @override
  String get common_terminate_room_connection_message => '确定要断开连麦吗？';

  @override
  String get common_un_mute_audio_by_master => '主播解除了您的静音';

  @override
  String get common_un_mute_video_by_master => '主播解禁了你的摄像头';

  @override
  String get common_link_guest => '连观众';

  @override
  String get common_link_host => '连主播';

  @override
  String get live_barrage_error_content_is_long => '内容太长，请缩减内容后再试';

  @override
  String get live_barrage_error_network => '网络异常，请稍后再试';

  @override
  String get live_barrage_error_sensitive_word => '检测到敏感词，请修改后重试';

  @override
  String get live_error_connection_notexit => '邀请连线的房间不存在';

  @override
  String get live_error_connection_retry => '内部错误，推荐重试一次。';

  @override
  String get livelist_loading => '正在加载...';

  @override
  String get livelist_no_more_data => '没有更多数据了';

  @override
  String get livelist_viewed_audience_count => 'xxx人看过';

  @override
  String get livelist_exit_float_window_tip => '正在直播中，请稍后再试。';

  @override
  String get livestreamcore_battle_error_conflict => '主播正在PK中，无法发起PK';

  @override
  String get livestreamcore_battle_error_other => '其他错误，无法发起PK';

  @override
  String get common_audience_end_link_tips => '当前处于连麦状态，是否需要「断开连麦」或「退出直播间」';

  @override
  String get common_exit_live => '退出直播间';

  @override
  String get common_gift_me => '我';

  @override
  String get common_resolution_1080p => '1080p';

  @override
  String get common_stream_categories => '直播分类:';

  @override
  String get common_stream_privacy_status => '直播模式:';

  @override
  String get common_voiceroom_kicked_out_of_seat => '被主持人踢下麦位';

  @override
  String get common_like => '点赞';

  @override
  String get common_apply_link_mic => '申请连麦';

  @override
  String get common_client_error_already_in_other_room =>
      '当前用户已在别的房间内，单个 roomEngine 实例只支持用户进入一个房间，如果要进入不同的房间请先退房或者使用新的 roomEngine 实例';

  @override
  String get common_client_error_camera_device_empty =>
      '当前无摄像头设备，请插入摄像头设备解决该问题';

  @override
  String get common_client_error_camera_not_authorized => '摄像头没有系统授权, 检查系统授权';

  @override
  String get common_client_error_camera_occupied => '摄像头被占用，检查是否有其他进程使用摄像头';

  @override
  String get common_client_error_camera_start_fail =>
      '系统问题，打开摄像头失败。检查摄像头设备是否正常';

  @override
  String get common_client_error_connection_connecting =>
      '被邀请连线的房间已在邀请列表或者已连线。';

  @override
  String get common_client_error_exit_not_supported_for_room_owner =>
      '房主不支持退房操作，房主只能解散房间';

  @override
  String get common_client_error_failed => '暂未归类的通用错误';

  @override
  String get common_client_error_freq_limit => '请求被限频，请稍后重试';

  @override
  String get common_client_error_get_screen_sharing_target_failed =>
      '获取屏幕分享源（屏幕和窗口）失败，检查屏幕录制权限';

  @override
  String get common_client_error_invalid_parameter =>
      '调用 API 时，传入的参数不合法，检查入参是否合法';

  @override
  String get common_client_error_max_seat_count_limit => '最大麦位超出套餐包数量限制';

  @override
  String get common_client_error_microphone_device_empty => '当前无麦克风设备';

  @override
  String get common_client_error_microphone_not_authorized =>
      '麦克风没有系统授权，检查系统授权';

  @override
  String get common_client_error_microphone_occupied => '麦克风被占用';

  @override
  String get common_client_error_microphone_start_fail =>
      '系统问题，打开麦克风失败。检查麦克风设备是否正常';

  @override
  String get common_client_error_open_camera_need_permission_from_admin =>
      '需要向房主或管理员申请后打开摄像头';

  @override
  String get common_client_error_open_camera_need_seat_unlock =>
      '当前麦位视频被锁, 需要由房主解锁麦位后，才能打开摄像头';

  @override
  String get common_client_error_open_microphone_need_permission_from_admin =>
      '需要向房主或管理员申请后打开麦克风';

  @override
  String get common_client_error_open_microphone_need_seat_unlock => '当前麦位音频被锁';

  @override
  String get common_client_error_open_screen_share_need_permission_from_admin =>
      '需要向房主或管理员申请后打开屏幕分享';

  @override
  String get common_client_error_open_screen_share_need_seat_unlock =>
      '当前麦位视频被锁, 需要由房主解锁麦位后，才能打开屏幕分享';

  @override
  String get common_client_error_operation_invalid_before_enter_room =>
      '需要进房后才可使用此功能';

  @override
  String get common_client_error_operation_not_supported_in_current_room_type =>
      '当前房间类型下不支持该操作';

  @override
  String get common_client_error_permission_denied =>
      '获取权限失败，当前未授权音/视频权限，请查看是否开启设备权限';

  @override
  String get common_client_error_repeat_operation => '重复操作';

  @override
  String get common_client_error_request_id_invalid => '信令请求 ID 无效或已经被处理过';

  @override
  String get common_client_error_request_id_repeat => '信令请求重复';

  @override
  String get common_client_error_request_no_permission =>
      '信令请求无权限，例如取消非自己发起的邀请';

  @override
  String get common_client_error_require_payment =>
      '该功能需要开通额外的套餐，请在 腾讯云视立方 SDK 控制台 按需开通对应套餐';

  @override
  String get common_client_error_room_id_invalid =>
      '创建房间 ID 非法，自定义 ID 必须为可打印 ASCII 字符（0x20-0x7e），最长48个字节';

  @override
  String get common_client_error_room_name_invalid =>
      '房间名称非法，名称最长30字节，字符编码必须是 UTF-8 ，如果包含中文';

  @override
  String get common_client_error_room_not_support_preloading => '当前房间不支持预加载';

  @override
  String get common_client_error_sdk_app_id_not_found =>
      '未找到 SDKAppID，请在 腾讯云视立方 SDK 控制台 确认应用信息';

  @override
  String get common_client_error_sdk_not_initialized => '未登录,请调用 Login 接口';

  @override
  String get common_client_error_seat_index_not_exist => '麦位编号不存在';

  @override
  String get common_client_error_send_message_disabled_for_all => '当前房间已开启全员禁言';

  @override
  String get common_client_error_send_message_disabled_for_current =>
      '当前房间内，您已被禁言';

  @override
  String get common_client_error_start_screen_sharing_failed =>
      '开启屏幕分享失败，检查房间内是否有人正在屏幕分享';

  @override
  String get common_client_error_success => '操作成功';

  @override
  String get common_client_error_user_need_admin_permission =>
      '需要房主或者管理员权限才能操作';

  @override
  String get common_client_error_user_need_owner_permission => '需要房主权限才能操作';

  @override
  String get common_client_error_user_not_exist => '用户不存在';

  @override
  String get common_server_error_gift_ability_not_enabled =>
      '礼物功能服务暂未开启，请检查套餐包版本';

  @override
  String get common_server_error_gift_not_exist => '礼物不存在';

  @override
  String get common_server_error_gift_server_pre_verification_failed =>
      '礼物服务器预验证失败，请检查控制台配置';

  @override
  String get common_server_error_call_in_progress => '正在通话中，设备操作失败';

  @override
  String get common_ear_return_volume => '耳返音量';

  @override
  String get common_fan_count => '粉丝';

  @override
  String get live_barrage_warning_not_empty => '输入不能为空';

  @override
  String get live_clarity => '清晰度';

  @override
  String get common_seat_management => '麦控';

  @override
  String get live_room_has_been_dismissed => '房间已被解散';

  @override
  String get common_anchor_audience_list_panel_title => '在线观众';

  @override
  String get common_app_running => '运行中';

  @override
  String get common_audio_effect => '音效';

  @override
  String get common_audio_effect_settings => '音效设置';

  @override
  String get common_audio_settings => '音频设置';

  @override
  String get common_beauty_item_close => '关闭';

  @override
  String get common_beauty_item_ruddy => '红润';

  @override
  String get common_beauty_item_smooth => '磨皮';

  @override
  String get common_beauty_item_whiteness => '美白';

  @override
  String get common_beauty_panel_title => '一键美颜';

  @override
  String get common_cancel => '取消';

  @override
  String get common_change_voice => '变声';

  @override
  String get common_change_voice_child => '熊孩子';

  @override
  String get common_change_voice_ethereal => '空灵';

  @override
  String get common_change_voice_girl => '萝莉';

  @override
  String get common_change_voice_none => '原声';

  @override
  String get common_change_voice_uncle => '大叔';

  @override
  String get common_common_gift_income => '礼物收入';

  @override
  String get common_common_like_count => '点赞数量';

  @override
  String get common_common_live_duration => '直播时长';

  @override
  String get common_common_live_people_number => '累计观看';

  @override
  String get common_common_message_count => '消息数量';

  @override
  String get common_common_send_gift_people_count => '送礼人数';

  @override
  String get common_common_this_live_data => '本场直播数据';

  @override
  String get common_ear_return => '耳返';

  @override
  String get common_set_cover => '修改封面';

  @override
  String get common_end_link => '断开连麦';

  @override
  String get common_end_live => '结束直播';

  @override
  String get common_end_user => '挂断';

  @override
  String get common_entered_room => '进入房间';

  @override
  String get common_follow_anchor => '关注';

  @override
  String get common_gift_give_gift => '赠送';

  @override
  String get common_gift_title => '礼物';

  @override
  String get common_hang_up => '挂断';

  @override
  String get common_kick_user_confirm_message => '确认要删除xxx吗?';

  @override
  String get common_link_mic_manager => '连麦管理';

  @override
  String get common_link => '连麦';

  @override
  String get common_live_has_stop => '直播已结束';

  @override
  String get common_more_settings => '更多设置';

  @override
  String get common_music => '音乐';

  @override
  String get common_music_cheerful => '欢快';

  @override
  String get common_music_confirm => '确认';

  @override
  String get common_music_melancholy => '忧郁';

  @override
  String get common_music_tips_title => '温馨提示';

  @override
  String get common_music_volume => '音乐音量';

  @override
  String get common_music_wonder_world => '神奇世界';

  @override
  String get common_people_volume => '人声音量';

  @override
  String get common_accept => '同意';

  @override
  String get common_receive => '接受';

  @override
  String get common_reject => '拒绝';

  @override
  String get common_resolution_360p => '360p';

  @override
  String get common_resolution_540p => '540p';

  @override
  String get common_resolution_720p => '720p';

  @override
  String get common_reverb => '混响';

  @override
  String get common_reverb_karaoke => 'KTV';

  @override
  String get common_reverb_loud_and_loud => '洪亮';

  @override
  String get common_reverb_low => '低沉';

  @override
  String get common_reverb_metallic_sound => '金属声';

  @override
  String get common_reverb_none => '无效果';

  @override
  String get common_room_destroy => '直播已结束';

  @override
  String get common_room_info_liveroom_id => '直播房间ID：';

  @override
  String get common_sent => '送给';

  @override
  String get common_go_live => '开直播';

  @override
  String get common_server_error_already_on_the_mic => '已经处于麦上状态';

  @override
  String get common_server_error_already_on_the_mic_queue => '已经处于排麦状态';

  @override
  String get common_server_error_battle_does_not_exist_or_has_ended =>
      '该场次 battle 不存在或已结束';

  @override
  String get common_server_error_battle_session_has_ended => '该 battle 场次已经结束';

  @override
  String get common_server_error_connection_does_not_exist => '当前连线不存在或结束';

  @override
  String get common_server_error_creating_battles_too_frequently =>
      '短时间内频繁发起 battle, 稍等一会再试';

  @override
  String get common_server_error_creating_connections_too_frequent =>
      '短时间内连线过于频繁，稍等一会再试';

  @override
  String get common_server_error_creating_rooms_exceeds_the_frequency_limit =>
      '频率超过限制，例如创建房间超过频率超限，同一房间 ID， 1秒内只能创建一次';

  @override
  String get common_server_error_exceeds_the_upper_limit =>
      '超过付费上限，例如麦位数，pk场次房间数量等超过付费限制';

  @override
  String
      get common_server_error_has_exceeded_the_limit_in_connection_or_battle =>
          '超过连线和 battle 房间数量上限';

  @override
  String get common_server_error_in_other_battle => '该房间处于其他的 battle 场次中';

  @override
  String get common_server_error_insufficient_operation_permissions =>
      '您无权执行此操作';

  @override
  String get common_server_error_invalid_room_type => '无效的房间类型';

  @override
  String get common_server_error_is_connecting_with_other_rooms =>
      '当前房间与其他房间连线中';

  @override
  String
      get common_server_error_is_not_allowed_to_cancel_battle_for_room_in_battle =>
          '该房间处于 battle 中';

  @override
  String get common_server_error_metadata_no_valid_keys =>
      '删除房间 meta 数据时候，被删除的 key 没有一个存在';

  @override
  String get common_server_error_metadata_number_of_keys_exceeds_the_limit =>
      '房间 meta 数据中的 key 数量超过上限';

  @override
  String get common_server_error_metadata_size_of_value_exceeds_the_limit =>
      '房间 meta 数据中单个 key 对应的 val 超过最大字节数限制';

  @override
  String
      get common_server_error_metadata_the_size_of_key_exceeds_the_maximum_byte_limit =>
          '房间 meta 数据中的 key 大小超过了最大字节数限制';

  @override
  String get common_server_error_metadata_total_size_exceeds_the_limit =>
      '房间 meta数据中所有 key 对应的 val 总和超过最大字节数限制';

  @override
  String get common_server_error_mic_seat_is_locked => '麦位已锁定，可以尝试换一个麦位';

  @override
  String get common_server_error_no_payment_information => '无付费信息，需在控制台购买套餐包';

  @override
  String get common_server_error_no_rooms_in_the_battle_is_valid =>
      '发起的 battle 里没有一个有效的房间';

  @override
  String get common_server_error_not_a_room_member => '非房间成员';

  @override
  String get common_server_error_not_on_the_mic_queue => '没有在排麦列表中';

  @override
  String get common_server_error_not_on_the_mic_seat => '未在麦上';

  @override
  String get common_server_error_not_started_yet => '该 battle 场次还未开始';

  @override
  String get common_server_error_param_illegal => '请参数非法，请根据错误描述检查请求是否正确';

  @override
  String get common_server_error_requires_password => '当前房间需要密码才能进入';

  @override
  String get common_server_error_room_admin_quantity_exceeds_the_upper_limit =>
      '管理员数量超过上限';

  @override
  String get common_server_error_room_does_not_exist =>
      '房间不存在，或者曾经存在过，但是目前已经被解散';

  @override
  String get common_server_error_room_does_not_support_mic_ability =>
      '该房间不支持连麦';

  @override
  String get common_server_error_room_entry_password_error => '进房密码错误';

  @override
  String get common_server_error_room_id_exists => '房间ID 已被使用，请选择别的房间ID';

  @override
  String get common_server_error_room_id_has_been_occupied_by_chat =>
      '房间 ID 已被 IM 占用，可以换一个房间 ID 使用，或者先通过 IM 接口解散该群';

  @override
  String get common_server_error_room_id_has_been_used =>
      '房间 ID 已被使用，并且操作者为房主，可以直接使用';

  @override
  String get common_server_error_room_is_full => '房间成员已满';

  @override
  String get common_server_error_room_is_in_connection => '该房间已经在连线中';

  @override
  String get common_server_error_seat_is_already_occupied => '当前麦位已经有人了';

  @override
  String get common_server_error_signal_request_conflict => '信令请求冲突';

  @override
  String get common_server_error_system_internal_error => '服务器内部错误，请重试';

  @override
  String get common_server_error_tag_quantity_exceeds_upper_limit => '标签数量超上限';

  @override
  String get common_server_error_the_room_is_not_in_the_battle =>
      '该房间已经不在 battle 中';

  @override
  String get common_server_error_the_seat_list_is_empty => '连麦列表为空';

  @override
  String get common_server_error_the_seats_are_all_taken => '麦位已满';

  @override
  String get common_server_error_there_is_a_pending_battle_request =>
      '该房间存在待处理的 battle 请求';

  @override
  String get common_server_error_there_is_a_pending_connection_request =>
      '该房间存在待处理的连线请求';

  @override
  String get common_server_error_this_member_has_been_banned => '该成员已经被封禁';

  @override
  String get common_server_error_this_member_has_been_muted => '该成员已经被禁言';

  @override
  String get common_server_error_user_is_already_on_the_mic_seat => '已经有用户在麦位上';

  @override
  String get common_set_as_background => '设为背景';

  @override
  String get common_set_as_cover => '设为封面';

  @override
  String get common_settings => '设置';

  @override
  String get common_settings_bg_image => '背景';

  @override
  String get common_stream_categories_beauty => '颜值';

  @override
  String get common_stream_categories_default => '日常聊天';

  @override
  String get common_stream_categories_shopping => '购物';

  @override
  String get common_stream_categories_teach => '知识教学';

  @override
  String get common_stream_privacy_status_default => '公开';

  @override
  String get common_stream_privacy_status_privacy => '隐私';

  @override
  String get common_text_cancel_link_mic_apply => '取消连麦申请';

  @override
  String get common_text_close_link_mic => '结束连麦';

  @override
  String get common_text_link_mic_audio => '申请语音连麦';

  @override
  String get common_text_link_mic_selector => '选择连麦方式，主播同意后接通';

  @override
  String get common_text_link_mic_video => '申请视频连麦';

  @override
  String get common_tips_apply_link_mic => '接通后画面效果将自动生效';

  @override
  String get common_title_link_mic_selector => '选择连麦方式';

  @override
  String get common_title_link_video_settings => '调整视频连麦画面';

  @override
  String get common_toast_apply_link_mic => '你提交了连麦申请，请等待主播同意';

  @override
  String get common_unfollow_anchor => '取消关注';

  @override
  String get common_video_config => '视频参数';

  @override
  String get common_video_params => '视频参数';

  @override
  String get common_video_settings_item_beauty => '美颜';

  @override
  String get common_video_settings_item_flip => '翻转';

  @override
  String get common_video_settings_item_mirror => '镜像';

  @override
  String get common_video_settings_item_pip => '画中画';

  @override
  String get common_pip_toggle => '画中画开关';

  @override
  String get common_pip_toggle_description => '开启后，应用退后台时自动进入画中画模式';

  @override
  String get common_voiceroom_empty_view => '麦位没有用户，去邀请';

  @override
  String get common_voiceroom_invite => '邀请';

  @override
  String get common_voiceroom_invite_seat_canceled => '上麦邀请已被取消';

  @override
  String get common_voiceroom_lock => '锁定麦位';

  @override
  String get common_voiceroom_mute_seat => '静音';

  @override
  String get common_voiceroom_need_agree => '上麦需要房主同意';

  @override
  String get common_voiceroom_receive_seat_invitation => 'xxx 向你发来上麦邀请';

  @override
  String get common_voiceroom_take_seat => '上麦';

  @override
  String get common_voiceroom_take_seat_rejected => '上麦申请被拒绝';

  @override
  String get common_voiceroom_take_seat_timeout => '上麦申请超时';

  @override
  String get common_voiceroom_unlock => '解锁麦位';

  @override
  String get common_voiceroom_unmuted_seat => '解除静音';

  @override
  String get common_waiting_pass => '待通过';

  @override
  String get live_barrage_agree => '同意';

  @override
  String get live_barrage_btn_send => '发送';

  @override
  String get live_choose_music => '选择音乐';

  @override
  String get live_failed_to_enter_room => '进房失败';

  @override
  String get live_invalid_userId => '非法的userId';

  @override
  String get live_music_pitch => '音乐升降调';

  @override
  String get common_video_status => '视频状态';

  @override
  String get common_audio_status => '音频状态';

  @override
  String get common_device_temp => '设备温度';

  @override
  String get common_wifi_or_mobile_network => 'Wi-Fi/移动网络';

  @override
  String get common_video_stream_smooth => '开播画面流畅';

  @override
  String get common_video_stream_freezing => '开播画面卡顿';

  @override
  String get common_video_capture_closed => '视频采集已关闭';

  @override
  String get common_audio_tips_proper_volume => '适当音量确保良好观看体验';

  @override
  String get common_audio_tips_regular_checks => '定期检查确保良好观看体验';

  @override
  String get common_network_switch_tips => '避免频繁切换网络';

  @override
  String get common_live_info => '直播信息';

  @override
  String get common_rtt => '往返延时';

  @override
  String get common_down_loss => '下行丢包';

  @override
  String get common_up_loss => '上行丢包';

  @override
  String get common_normal => '正常';

  @override
  String get common_close => '关闭';

  @override
  String get common_exception => '异常';

  @override
  String get common_excellent => '优秀';

  @override
  String get common_good => '良好';

  @override
  String get common_poor => '较差';

  @override
  String get common_bad => '差';

  @override
  String get common_verybad => '非常差';

  @override
  String get common_down => '断开';

  @override
  String get common_audio_mode_default => '默认模式';

  @override
  String get common_audio_mode_speech => '人声模式';

  @override
  String get common_audio_mode_music => '音乐模式';

  @override
  String get common_device_temp_fair => '轻微发热';

  @override
  String get common_device_temp_serious => '严重发热';

  @override
  String get common_network_bad_tips => '当前网络卡顿，建议';

  @override
  String get common_switch_network => '切换网络';

  @override
  String get common_wait_connection => '等待连线';

  @override
  String get common_apply_connection => '申请连线';

  @override
  String get common_template_select => '模板选择:';

  @override
  String get common_template_dynamic_grid => '动态宫格';

  @override
  String get common_template_dynamic_float => '浮动小窗';

  @override
  String get common_template_static_grid => '静态宫格';

  @override
  String get common_template_static_float => '固定小窗';

  @override
  String get common_template_layout_settings => '布局设置';

  @override
  String get common_template_layout_co_guest => '连观众布局';

  @override
  String get common_template_layout_co_host => '主播PK布局';

  @override
  String get common_template_layout => '布局';

  @override
  String get common_video_settings => '视频设置';

  @override
  String get live_video_resolution => '清晰度';

  @override
  String get live_video_reduce_resolution => '降低清晰度';

  @override
  String get live_video_resolution_changed => '清晰度已切换至';

  @override
  String get common_template_601_ui_exception_toast =>
      '该布局模板在特定机型上会显示异常，建议更换其他布局模板';

  @override
  String get seat_locked => '已锁定';

  @override
  String get seat_unlocked => '解锁麦位';

  @override
  String get seat_no_guest => '暂无嘉宾';

  @override
  String get seat_in_battle => '正在PK';

  @override
  String get seat_cancel_invite => '取消邀请';

  @override
  String get seat_request_battle => '发起PK';

  @override
  String get seat_request_host => '邀请连线';

  @override
  String get seat_invite_battle => '邀请PK';

  @override
  String get seat_repeat_invite_tips => '正在邀请他人，可撤回申请后再发起邀请';

  @override
  String get seat_end_Battle => '结束PK';

  @override
  String get seat_too_many_guest => '请控制麦上人数在6人及以下';

  @override
  String get seat_only_show_first_six_seat =>
      '请注意，6V6 PK只展示前6个麦位，并请控制连麦人数在6人以下';

  @override
  String get seat_in_pk => 'PK中';

  @override
  String get live_end_live_tips => '确定要结束直播吗？';

  @override
  String get live_exit_live_tips => '确定要退出直播间吗？';

  @override
  String get live_cancel_request => 'xxx 取消了申请';

  @override
  String get mirror_type_auto => '自动';

  @override
  String get mirror_type_enable => '开启';

  @override
  String get mirror_type_disable => '关闭';

  @override
  String get mirror_type_change_to => '切换为xxx';

  @override
  String get common_remove => '移除';
}

/// The translations for Chinese, using the Han script (`zh_Hant`).
class LiveKitLocalizationsZhHant extends LiveKitLocalizationsZh {
  LiveKitLocalizationsZhHant() : super('zh_Hant');

  @override
  String get common_disconnect_guest_tips => '確定要斷開xxx的連麥嗎？';

  @override
  String get common_anchor_battle => '主播PK';

  @override
  String get common_anchor_end_link_tips => '當前處於連麥狀態，是否需要「關閉直播間」';

  @override
  String get common_battle_connecting => '連線中';

  @override
  String get common_battle_end_pk => '結束PK';

  @override
  String get common_battle_end_pk_tips => '確定要結束PK嗎？結束後將以當前結果為最終結果';

  @override
  String get common_battle_invitation_timeout => 'PK邀請已超時';

  @override
  String get common_battle_invitee_reject => 'xxx拒絕了 PK';

  @override
  String get common_battle_inviter_cancel => 'xxx 取消了 PK，請嘗試再次發起';

  @override
  String get common_battle_inviting => 'xxx向你發來PK邀請';

  @override
  String get common_battle_pk_end => 'PK結束';

  @override
  String get common_battle_wait_start => '等待同意PK';

  @override
  String get common_battle_wait_stop => '取消';

  @override
  String get common_connect_conflict => '被邀請連線的房間與其他房間連線中。';

  @override
  String get common_connect_error => '其他錯誤，無法發起連線';

  @override
  String get common_connect_inviting => '邀請中';

  @override
  String get common_connect_inviting_append => 'xxx向你發來連線邀請';

  @override
  String get common_connect_request_rejected => '連線申請被拒絕';

  @override
  String get common_connect_invitation_timeout => '邀請已超時';

  @override
  String get common_connection => '發起連線';

  @override
  String get common_connection_list_title => '連線中(xxx)';

  @override
  String get common_connection_room_full => '當前連線人數已達最大限制。';

  @override
  String get common_cover => '封面';

  @override
  String get common_disable_audio => '禁用音頻';

  @override
  String get common_disable_message => '禁止發言';

  @override
  String get common_disable_video => '禁用視頻';

  @override
  String get common_disconnect_tips => '確定要斷開與其他主播的連線嗎?';

  @override
  String get common_enable_audio => '啟用音頻';

  @override
  String get common_enable_message => '解除禁言';

  @override
  String get common_enable_video => '啟用視頻';

  @override
  String get common_end_connect => '斷開連線';

  @override
  String get common_end_connection_tips => '當前處於連線狀態，是否需要「斷開連線」或「關閉直播間」';

  @override
  String get common_end_pk_tips => '當前處於PK狀態，是否需要「結束PK」或「關閉直播間」';

  @override
  String get common_kick_out_of_room => '踢出房間';

  @override
  String get common_kicked_out_of_room_by_owner => '您已被踢出房間';

  @override
  String get common_more => '更多';

  @override
  String get common_more_features => '更多功能';

  @override
  String get common_mute_audio_by_master => '您已被主播靜音';

  @override
  String get common_mute_video_by_owner => '您已被主播關閉攝像頭';

  @override
  String get common_recommended_list => '推薦列表';

  @override
  String get common_link_mic_up_title => '當前麥位';

  @override
  String get common_send_message_disabled => '當前房間內，您已被禁言';

  @override
  String get common_send_message_enable => '當前房間內，您已被解除禁言';

  @override
  String get common_start_live => '開始直播';

  @override
  String get common_start_video => '開啟視頻';

  @override
  String get common_stop_video => '關閉視頻';

  @override
  String get common_terminate_room_connection_message => '確定要斷開連麥嗎？';

  @override
  String get common_un_mute_audio_by_master => '主播解除了您的靜音';

  @override
  String get common_un_mute_video_by_master => '主播解禁了你的攝像頭';

  @override
  String get common_link_guest => '連觀眾';

  @override
  String get common_link_host => '連主播';

  @override
  String get live_barrage_error_content_is_long => '內容太長，請縮減內容後再試';

  @override
  String get live_barrage_error_network => '網絡異常，請稍後再試';

  @override
  String get live_barrage_error_sensitive_word => '檢測到敏感詞，請修改後重試';

  @override
  String get live_error_connection_notexit => '邀請連線的房間不存在';

  @override
  String get live_error_connection_retry => '內部錯誤，推薦重試一次。';

  @override
  String get livelist_loading => '正在加載...';

  @override
  String get livelist_no_more_data => '沒有更多數據了';

  @override
  String get livelist_viewed_audience_count => 'xxx人看過';

  @override
  String get livelist_exit_float_window_tip => '正在直播中，請稍後再試。';

  @override
  String get livestreamcore_battle_error_conflict => '主播正在PK中，無法發起PK';

  @override
  String get livestreamcore_battle_error_other => '其他錯誤，無法發起PK';

  @override
  String get common_audience_end_link_tips => '當前處於連麥狀態，是否需要「斷開連麥」或「退出直播間」';

  @override
  String get common_exit_live => '退出直播間';

  @override
  String get common_gift_me => '我';

  @override
  String get common_resolution_1080p => '1080p';

  @override
  String get common_stream_categories => '直播分類:';

  @override
  String get common_stream_privacy_status => '直播模式:';

  @override
  String get common_voiceroom_kicked_out_of_seat => '被主持人踢下麥位';

  @override
  String get common_like => '點讚';

  @override
  String get common_apply_link_mic => '申請連麥';

  @override
  String get common_client_error_already_in_other_room =>
      '當前用戶已在別的房間內，單個 roomEngine 實例只支持用戶進入一個房間，如果要進入不同的房間請先退房或者使用新的 roomEngine 實例';

  @override
  String get common_client_error_camera_device_empty =>
      '當前無攝像頭設備，請插入攝像頭設備解決該問題';

  @override
  String get common_client_error_camera_not_authorized => '攝像頭沒有系統授權, 檢查系統授權';

  @override
  String get common_client_error_camera_occupied => '攝像頭被佔用，檢查是否有其他進程使用攝像頭';

  @override
  String get common_client_error_camera_start_fail =>
      '系統問題，打開攝像頭失敗。檢查攝像頭設備是否正常';

  @override
  String get common_client_error_connection_connecting =>
      '被邀請連線的房間已在邀請列表或者已連線。';

  @override
  String get common_client_error_exit_not_supported_for_room_owner =>
      '房主不支持退房操作，房主只能解散房間';

  @override
  String get common_client_error_failed => '暫未歸類的通用錯誤';

  @override
  String get common_client_error_freq_limit => '請求被限頻，請稍後重試';

  @override
  String get common_client_error_get_screen_sharing_target_failed =>
      '獲取屏幕分享源（屏幕和窗口）失敗，檢查屏幕錄製權限';

  @override
  String get common_client_error_invalid_parameter =>
      '調用 API 時，傳入的參數不合法，檢查入參是否合法';

  @override
  String get common_client_error_max_seat_count_limit => '最大麥位超出套餐包數量限制';

  @override
  String get common_client_error_microphone_device_empty => '當前無麥克風設備';

  @override
  String get common_client_error_microphone_not_authorized =>
      '麥克風沒有系統授權，檢查系統授權';

  @override
  String get common_client_error_microphone_occupied => '麥克風被佔用';

  @override
  String get common_client_error_microphone_start_fail =>
      '系統問題，打開麥克風失敗。檢查麥克風設備是否正常';

  @override
  String get common_client_error_open_camera_need_permission_from_admin =>
      '需要向房主或管理員申請後打開攝像頭';

  @override
  String get common_client_error_open_camera_need_seat_unlock =>
      '當前麥位視頻被鎖, 需要由房主解鎖麥位後，才能打開攝像頭';

  @override
  String get common_client_error_open_microphone_need_permission_from_admin =>
      '需要向房主或管理員申請後打開麥克風';

  @override
  String get common_client_error_open_microphone_need_seat_unlock => '當前麥位音頻被鎖';

  @override
  String get common_client_error_open_screen_share_need_permission_from_admin =>
      '需要向房主或管理員申請後打開屏幕分享';

  @override
  String get common_client_error_open_screen_share_need_seat_unlock =>
      '當前麥位視頻被鎖, 需要由房主解鎖麥位後，才能打開屏幕分享';

  @override
  String get common_client_error_operation_invalid_before_enter_room =>
      '需要進房後才可使用此功能';

  @override
  String get common_client_error_operation_not_supported_in_current_room_type =>
      '當前房間類型下不支持該操作';

  @override
  String get common_client_error_permission_denied =>
      '獲取權限失敗，當前未授權音/視頻權限，請查看是否開啟設備權限';

  @override
  String get common_client_error_repeat_operation => '重複操作';

  @override
  String get common_client_error_request_id_invalid => '信令請求 ID 無效或已經被處理過';

  @override
  String get common_client_error_request_id_repeat => '信令請求重複';

  @override
  String get common_client_error_request_no_permission =>
      '信令請求無權限，例如取消非自己發起的邀請';

  @override
  String get common_client_error_require_payment =>
      '該功能需要開通額外的套餐，請在 騰訊雲視立方 SDK 控制臺 按需開通對應套餐';

  @override
  String get common_client_error_room_id_invalid =>
      '創建房間 ID 非法，自定義 ID 必須為可列印 ASCII 字符（0x20-0x7e），最長48個字節';

  @override
  String get common_client_error_room_name_invalid =>
      '房間名稱非法，名稱最長30位元組，字符編碼必須是 UTF-8 ，如果包含中文';

  @override
  String get common_client_error_room_not_support_preloading => '當前房間不支持預加載';

  @override
  String get common_client_error_sdk_app_id_not_found =>
      '未找到 SDKAppID，請在 騰訊雲視立方 SDK 控制臺 確認應用信息';

  @override
  String get common_client_error_sdk_not_initialized => '未登錄,請調用 Login 接口';

  @override
  String get common_client_error_seat_index_not_exist => '麥位編號不存在';

  @override
  String get common_client_error_send_message_disabled_for_all => '當前房間已開啟全員禁言';

  @override
  String get common_client_error_send_message_disabled_for_current =>
      '當前房間內，您已被禁言';

  @override
  String get common_client_error_start_screen_sharing_failed =>
      '開啟屏幕分享失敗，檢查房間內是否有人正在屏幕分享';

  @override
  String get common_client_error_success => '操作成功';

  @override
  String get common_client_error_user_need_admin_permission =>
      '需要房主或者管理員權限才能操作';

  @override
  String get common_client_error_user_need_owner_permission => '需要房主權限才能操作';

  @override
  String get common_client_error_user_not_exist => '用戶不存在';

  @override
  String get common_server_error_gift_ability_not_enabled =>
      '禮物功能服務暫未開啟，請檢查套餐包版本';

  @override
  String get common_server_error_gift_not_exist => '禮物不存在';

  @override
  String get common_server_error_gift_server_pre_verification_failed =>
      '禮物伺服器預驗證失敗，請檢查控制台配置';

  @override
  String get common_server_error_call_in_progress => '正在通話中，設備操作失敗';

  @override
  String get common_ear_return_volume => '耳返音量';

  @override
  String get common_fan_count => '粉絲';

  @override
  String get live_barrage_warning_not_empty => '輸入不能為空';

  @override
  String get live_clarity => '清晰度';

  @override
  String get common_seat_management => '麥控';

  @override
  String get live_room_has_been_dismissed => '房間已被解散';

  @override
  String get common_anchor_audience_list_panel_title => '在線觀眾';

  @override
  String get common_app_running => '運行中';

  @override
  String get common_audio_effect => '音效';

  @override
  String get common_audio_effect_settings => '音效設置';

  @override
  String get common_audio_settings => '音頻設置';

  @override
  String get common_beauty_item_close => '關閉';

  @override
  String get common_beauty_item_ruddy => '紅潤';

  @override
  String get common_beauty_item_smooth => '磨皮';

  @override
  String get common_beauty_item_whiteness => '美白';

  @override
  String get common_beauty_panel_title => '一鍵美顏';

  @override
  String get common_cancel => '取消';

  @override
  String get common_change_voice => '變聲';

  @override
  String get common_change_voice_child => '熊孩子';

  @override
  String get common_change_voice_ethereal => '空靈';

  @override
  String get common_change_voice_girl => '蘿莉';

  @override
  String get common_change_voice_none => '原聲';

  @override
  String get common_change_voice_uncle => '大叔';

  @override
  String get common_common_gift_income => '禮物收入';

  @override
  String get common_common_like_count => '點讚數量';

  @override
  String get common_common_live_duration => '直播時長';

  @override
  String get common_common_live_people_number => '累計觀看';

  @override
  String get common_common_message_count => '消息數量';

  @override
  String get common_common_send_gift_people_count => '送禮人數';

  @override
  String get common_common_this_live_data => '本場直播數據';

  @override
  String get common_ear_return => '耳返';

  @override
  String get common_set_cover => '修改封面';

  @override
  String get common_end_link => '斷開連麥';

  @override
  String get common_end_live => '結束直播';

  @override
  String get common_end_user => '掛斷';

  @override
  String get common_entered_room => '進入房間';

  @override
  String get common_follow_anchor => '關注';

  @override
  String get common_gift_give_gift => '贈送';

  @override
  String get common_gift_title => '禮物';

  @override
  String get common_hang_up => '掛斷';

  @override
  String get common_kick_user_confirm_message => '確認要刪除xxx嗎?';

  @override
  String get common_link_mic_manager => '連麥管理';

  @override
  String get common_link => '連麥';

  @override
  String get common_live_has_stop => '直播已結束';

  @override
  String get common_more_settings => '更多設置';

  @override
  String get common_music => '音樂';

  @override
  String get common_music_cheerful => '歡快';

  @override
  String get common_music_confirm => '確認';

  @override
  String get common_music_melancholy => '憂鬱';

  @override
  String get common_music_tips_title => '溫馨提示';

  @override
  String get common_music_volume => '音樂音量';

  @override
  String get common_music_wonder_world => '神奇世界';

  @override
  String get common_people_volume => '人聲音量';

  @override
  String get common_accept => '同意';

  @override
  String get common_receive => '接受';

  @override
  String get common_reject => '拒絕';

  @override
  String get common_resolution_360p => '360p';

  @override
  String get common_resolution_540p => '540p';

  @override
  String get common_resolution_720p => '720p';

  @override
  String get common_reverb => '混響';

  @override
  String get common_reverb_karaoke => 'KTV';

  @override
  String get common_reverb_loud_and_loud => '洪亮';

  @override
  String get common_reverb_low => '低沉';

  @override
  String get common_reverb_metallic_sound => '金屬聲';

  @override
  String get common_reverb_none => '無效果';

  @override
  String get common_room_destroy => '直播已結束';

  @override
  String get common_room_info_liveroom_id => '直播房間ID：';

  @override
  String get common_sent => '送給';

  @override
  String get common_go_live => '開直播';

  @override
  String get common_server_error_already_on_the_mic => '已經處於麥上狀態';

  @override
  String get common_server_error_already_on_the_mic_queue => '已經處於排麥狀態';

  @override
  String get common_server_error_battle_does_not_exist_or_has_ended =>
      '該場次 battle 不存在或已結束';

  @override
  String get common_server_error_battle_session_has_ended => '該 battle 場次已經結束';

  @override
  String get common_server_error_connection_does_not_exist => '當前連線不存在或結束';

  @override
  String get common_server_error_creating_battles_too_frequently =>
      '短時間內頻繁發起 battle, 稍等一會再試';

  @override
  String get common_server_error_creating_connections_too_frequent =>
      '短時間內連線過於頻繁，稍等一會再試';

  @override
  String get common_server_error_creating_rooms_exceeds_the_frequency_limit =>
      '頻率超過限制，例如創建房間超過頻率超限，同一房間 ID， 1秒內只能創建一次';

  @override
  String get common_server_error_exceeds_the_upper_limit =>
      '超過付費上限，例如麥位數，pk場次房間數量等超過付費限制';

  @override
  String
      get common_server_error_has_exceeded_the_limit_in_connection_or_battle =>
          '超過連線和 battle 房間數量上限';

  @override
  String get common_server_error_in_other_battle => '該房間處於其他的 battle 場次中';

  @override
  String get common_server_error_insufficient_operation_permissions =>
      '您無權執行此操作';

  @override
  String get common_server_error_invalid_room_type => '無效的房間類型';

  @override
  String get common_server_error_is_connecting_with_other_rooms =>
      '當前房間與其他房間連線中';

  @override
  String
      get common_server_error_is_not_allowed_to_cancel_battle_for_room_in_battle =>
          '該房間處於 battle 中';

  @override
  String get common_server_error_metadata_no_valid_keys =>
      '刪除房間 meta 數據時候，被刪除的 key 沒有一個存在';

  @override
  String get common_server_error_metadata_number_of_keys_exceeds_the_limit =>
      '房間 meta 數據中的 key 數量超過上限';

  @override
  String get common_server_error_metadata_size_of_value_exceeds_the_limit =>
      '房間 meta 數據中單個 key 對應的 val 超過最大字節數限制';

  @override
  String
      get common_server_error_metadata_the_size_of_key_exceeds_the_maximum_byte_limit =>
          '房間 meta 數據中的 key 大小超過了最大字節數限制';

  @override
  String get common_server_error_metadata_total_size_exceeds_the_limit =>
      '房間 meta數據中所有 key 對應的 val 總和超過最大字節數限制';

  @override
  String get common_server_error_mic_seat_is_locked => '麥位已鎖定，可以嘗試換一個麥位';

  @override
  String get common_server_error_no_payment_information => '無付費信息，需在控制臺購買套餐包';

  @override
  String get common_server_error_no_rooms_in_the_battle_is_valid =>
      '發起的 battle 裡沒有一個有效的房間';

  @override
  String get common_server_error_not_a_room_member => '非房間成員';

  @override
  String get common_server_error_not_on_the_mic_queue => '沒有在排麥列表中';

  @override
  String get common_server_error_not_on_the_mic_seat => '未在麥上';

  @override
  String get common_server_error_not_started_yet => '該 battle 場次還未開始';

  @override
  String get common_server_error_param_illegal => '請參數非法，請根據錯誤描述檢查請求是否正確';

  @override
  String get common_server_error_requires_password => '當前房間需要密碼才能進入';

  @override
  String get common_server_error_room_admin_quantity_exceeds_the_upper_limit =>
      '管理員數量超過上限';

  @override
  String get common_server_error_room_does_not_exist =>
      '房間不存在，或者曾經存在過，但是目前已經被解散';

  @override
  String get common_server_error_room_does_not_support_mic_ability =>
      '該房間不支持連麥';

  @override
  String get common_server_error_room_entry_password_error => '進房密碼錯誤';

  @override
  String get common_server_error_room_id_exists => '房間ID 已被使用，請選擇別的房間ID';

  @override
  String get common_server_error_room_id_has_been_occupied_by_chat =>
      '房間 ID 已被 IM 佔用，可以換一個房間 ID 使用，或者先通過 IM 接口解散該群';

  @override
  String get common_server_error_room_id_has_been_used =>
      '房間 ID 已被使用，並且操作者為房主，可以直接使用';

  @override
  String get common_server_error_room_is_full => '房間成員已滿';

  @override
  String get common_server_error_room_is_in_connection => '該房間已經在連線中';

  @override
  String get common_server_error_seat_is_already_occupied => '當前麥位已經有人了';

  @override
  String get common_server_error_signal_request_conflict => '信令請求衝突';

  @override
  String get common_server_error_system_internal_error => '伺服器內部錯誤，請重試';

  @override
  String get common_server_error_tag_quantity_exceeds_upper_limit => '標籤數量超上限';

  @override
  String get common_server_error_the_room_is_not_in_the_battle =>
      '該房間已經不在 battle 中';

  @override
  String get common_server_error_the_seat_list_is_empty => '連麥列表為空';

  @override
  String get common_server_error_the_seats_are_all_taken => '麥位已滿';

  @override
  String get common_server_error_there_is_a_pending_battle_request =>
      '該房間存在待處理的 battle 請求';

  @override
  String get common_server_error_there_is_a_pending_connection_request =>
      '該房間存在待處理的連線請求';

  @override
  String get common_server_error_this_member_has_been_banned => '該成員已經被封禁';

  @override
  String get common_server_error_this_member_has_been_muted => '該成員已經被禁言';

  @override
  String get common_server_error_user_is_already_on_the_mic_seat => '已經有用戶在麥位上';

  @override
  String get common_set_as_background => '設為背景';

  @override
  String get common_set_as_cover => '設為封面';

  @override
  String get common_settings => '設置';

  @override
  String get common_settings_bg_image => '背景';

  @override
  String get common_stream_categories_beauty => '顏值';

  @override
  String get common_stream_categories_default => '日常聊天';

  @override
  String get common_stream_categories_shopping => '購物';

  @override
  String get common_stream_categories_teach => '知識教學';

  @override
  String get common_stream_privacy_status_default => '公開';

  @override
  String get common_stream_privacy_status_privacy => '隱私';

  @override
  String get common_text_cancel_link_mic_apply => '取消連麥申請';

  @override
  String get common_text_close_link_mic => '結束連麥';

  @override
  String get common_text_link_mic_audio => '申請語音連麥';

  @override
  String get common_text_link_mic_selector => '選擇連麥方式，主播同意後接通';

  @override
  String get common_text_link_mic_video => '申請視頻連麥';

  @override
  String get common_tips_apply_link_mic => '接通後畫面效果將自動生效';

  @override
  String get common_title_link_mic_selector => '選擇連麥方式';

  @override
  String get common_title_link_video_settings => '調整視頻連麥畫面';

  @override
  String get common_toast_apply_link_mic => '你提交了連麥申請，請等待主播同意';

  @override
  String get common_unfollow_anchor => '取消關注';

  @override
  String get common_video_config => '視頻參數';

  @override
  String get common_video_params => '視頻參數';

  @override
  String get common_video_settings_item_beauty => '美顏';

  @override
  String get common_video_settings_item_flip => '翻轉';

  @override
  String get common_video_settings_item_mirror => '鏡像';

  @override
  String get common_video_settings_item_pip => '畫中畫';

  @override
  String get common_pip_toggle => '畫中畫開關';

  @override
  String get common_pip_toggle_description => '啟用後，應用退到後台時會自動進入畫中畫模式';

  @override
  String get common_voiceroom_empty_view => '麥位沒有用戶，去邀請';

  @override
  String get common_voiceroom_invite => '邀請';

  @override
  String get common_voiceroom_invite_seat_canceled => '上麥邀請已被取消';

  @override
  String get common_voiceroom_lock => '鎖定麥位';

  @override
  String get common_voiceroom_mute_seat => '靜音';

  @override
  String get common_voiceroom_need_agree => '上麥需要房主同意';

  @override
  String get common_voiceroom_receive_seat_invitation => 'xxx 向你發來上麥邀請';

  @override
  String get common_voiceroom_take_seat => '上麥';

  @override
  String get common_voiceroom_take_seat_rejected => '上麥申請被拒絕';

  @override
  String get common_voiceroom_take_seat_timeout => '上麥申請超時';

  @override
  String get common_voiceroom_unlock => '解鎖麥位';

  @override
  String get common_voiceroom_unmuted_seat => '解除靜音';

  @override
  String get common_waiting_pass => '待通過';

  @override
  String get live_barrage_agree => '同意';

  @override
  String get live_barrage_btn_send => '發送';

  @override
  String get live_choose_music => '選擇音樂';

  @override
  String get live_failed_to_enter_room => '進房失敗';

  @override
  String get live_invalid_userId => '非法的userId';

  @override
  String get live_music_pitch => '音樂升降調';

  @override
  String get common_video_status => '視頻狀態';

  @override
  String get common_audio_status => '音頻狀態';

  @override
  String get common_device_temp => '設備溫度';

  @override
  String get common_wifi_or_mobile_network => 'Wi-Fi/移動網絡';

  @override
  String get common_video_stream_smooth => '開播畫面流暢';

  @override
  String get common_video_stream_freezing => '開播畫面卡頓';

  @override
  String get common_video_capture_closed => '視頻採集已關閉';

  @override
  String get common_audio_tips_proper_volume => '適當音量確保良好觀看體驗';

  @override
  String get common_audio_tips_regular_checks => '定期檢查確保良好觀看體驗';

  @override
  String get common_network_switch_tips => '避免頻繁切換網絡';

  @override
  String get common_live_info => '直播資訊';

  @override
  String get common_rtt => '往返延時';

  @override
  String get common_down_loss => '下行丟包';

  @override
  String get common_up_loss => '上行丟包';

  @override
  String get common_normal => '正常';

  @override
  String get common_close => '關閉';

  @override
  String get common_exception => '異常';

  @override
  String get common_excellent => '優秀';

  @override
  String get common_good => '良好';

  @override
  String get common_poor => '較差';

  @override
  String get common_bad => '差';

  @override
  String get common_verybad => '非常差';

  @override
  String get common_down => '斷開';

  @override
  String get common_audio_mode_default => '預設模式';

  @override
  String get common_audio_mode_speech => '人聲模式';

  @override
  String get common_audio_mode_music => '音樂模式';

  @override
  String get common_device_temp_fair => '輕微發熱';

  @override
  String get common_device_temp_serious => '嚴重發熱';

  @override
  String get common_network_bad_tips => '網路狀態不佳，建議';

  @override
  String get common_switch_network => '切換網路';

  @override
  String get common_wait_connection => '等待連線';

  @override
  String get common_apply_connection => '申請連線';

  @override
  String get common_template_select => '模板選擇:';

  @override
  String get common_template_dynamic_grid => '動態宮格';

  @override
  String get common_template_dynamic_float => '浮動小窗';

  @override
  String get common_template_static_grid => '靜態宮格';

  @override
  String get common_template_static_float => '固定小窗';

  @override
  String get common_template_layout_settings => '佈局設定';

  @override
  String get common_template_layout_co_guest => '連觀眾佈置';

  @override
  String get common_template_layout_co_host => '主播PK佈局';

  @override
  String get common_template_layout => '佈局';

  @override
  String get common_video_settings => '視頻設置';

  @override
  String get live_video_resolution => '清晰度';

  @override
  String get live_video_reduce_resolution => '降低清晰度';

  @override
  String get live_video_resolution_changed => '清晰度已切換至';

  @override
  String get common_template_601_ui_exception_toast =>
      '此佈局模板在特定機型上會顯示異常，建議更換其他佈局模板';

  @override
  String get seat_locked => '已鎖定';

  @override
  String get seat_unlocked => '解鎖麦位';

  @override
  String get seat_no_guest => '暫無嘉賓';

  @override
  String get seat_in_battle => '正在PK';

  @override
  String get seat_cancel_invite => '取消邀請';

  @override
  String get seat_request_battle => '發起PK';

  @override
  String get seat_request_host => '邀請連線';

  @override
  String get seat_invite_battle => '邀請PK';

  @override
  String get seat_repeat_invite_tips => '正在邀請他人，可撤回申請後再發起邀請';

  @override
  String get seat_end_Battle => '結束PK';

  @override
  String get seat_too_many_guest => '請控制麦上人数在6人及以下';

  @override
  String get seat_only_show_first_six_seat =>
      '請注意，6V6 PK只展示前6個麦位，並請控制連麦人數在6人以下';

  @override
  String get seat_in_pk => 'PK中';

  @override
  String get live_end_live_tips => '确定要結束直播嗎？';

  @override
  String get live_exit_live_tips => '确定要退出直播間嗎？';

  @override
  String get live_cancel_request => '取消了申請';

  @override
  String get mirror_type_auto => '自動';

  @override
  String get mirror_type_enable => '開啟';

  @override
  String get mirror_type_disable => '關閉';

  @override
  String get mirror_type_change_to => '切換為xxx';

  @override
  String get common_remove => '移除';
}
