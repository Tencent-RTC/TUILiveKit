import 'livekit_localizations.dart';

/// The translations for English (`en`).
class LiveKitLocalizationsEn extends LiveKitLocalizations {
  LiveKitLocalizationsEn([String locale = 'en']) : super(locale);

  @override
  String get livekit_error_success => 'Operation successful';

  @override
  String get livekit_error_failed => 'Uncategorized common error';

  @override
  String get livekit_error_freqLimit =>
      'Request frequency limited, please try again later';

  @override
  String get livekit_error_repeat_operation => 'Duplicate operation';

  @override
  String get livekit_error_sdkAppId_notFound =>
      'SDKAppID not found, please confirm the application information in the Tencent Cloud Video Cube SDK Console';

  @override
  String get livekit_error_invalidParameter =>
      'Illegal parameters were passed when calling the API, check if the input parameters are valid';

  @override
  String get livekit_error_sdkNotInitialized =>
      'Not logged in, please call the Login interface';

  @override
  String get livekit_error_permissionDenied =>
      'Permission acquisition failed, currently unauthorized for audio/video permissions, please check if device permissions are enabled';

  @override
  String get livekit_error_requirePayment =>
      'Check if the cloud service account is normal';

  @override
  String get livekit_error_cameraStartFail =>
      'System issue, failed to open the camera_ Check if the camera device is working properly';

  @override
  String get livekit_error_cameraNotAuthorized =>
      'Camera not authorized by the system, check system authorization';

  @override
  String get livekit_error_cameraOccupied =>
      'Camera is occupied, check if other processes are using the camera';

  @override
  String get livekit_error_cameraDeviceEmpty =>
      'No camera device available, please insert a camera device to resolve this issue';

  @override
  String get livekit_error_microphoneStartFail =>
      'System issue, failed to open the microphone_ Check if the microphone device is working properly';

  @override
  String get livekit_error_microphoneNotAuthorized =>
      'Microphone not authorized by the system, check system authorization';

  @override
  String get livekit_error_microphoneOccupied => 'Microphone is occupied';

  @override
  String get livekit_error_microphoneDeviceEmpty =>
      'No microphone device available';

  @override
  String get livekit_error_getScreenSharingTargetFailed =>
      'Failed to get screen sharing source (screen and window), check screen recording permissions';

  @override
  String get livekit_error_startScreenSharingFailed =>
      'Failed to start screen sharing, check if someone else is screen sharing in the room';

  @override
  String get livekit_error_roomId_notExist =>
      'The room does not exist when entering, it may have been disbanded';

  @override
  String get livekit_error_operation_invalid_beforeEnterRoom =>
      'This feature can only be used after entering the room';

  @override
  String get livekit_error_exitNotSupported_forRoomOwner =>
      'Please transfer the room owner before leaving the room';

  @override
  String get livekit_error_operation_notSupported_inCurrentRoomType =>
      'This operation is not supported in the current room type';

  @override
  String get livekit_error_operation_notSupported_inCurrentSpeechMode =>
      'This operation is not supported in the current speaking mode';

  @override
  String get livekit_error_roomId_invalid =>
      'Invalid room ID created, custom ID must be printable ASCII characters (0x20-0x7e), up to 48 bytes';

  @override
  String get livekit_error_roomId_occupied =>
      'Room ID already in use, please choose another room ID';

  @override
  String get livekit_error_roomName_invalid =>
      'Invalid room name, the name can be up to 30 bytes, and the character encoding must be UTF-8 if it contains Chinese';

  @override
  String get livekit_error_already_in_OtherRoom =>
      'The current user is already in another room, you need to leave the room before joining a new one';

  @override
  String get livekit_error_userNotExist => 'User is not exist';

  @override
  String get livekit_error_userNotEntered => 'User is not in the current room';

  @override
  String get livekit_error_user_need_OwnerPermission =>
      'Requires room owner permission to operate';

  @override
  String get livekit_error_user_need_AdminPermission =>
      'Requires room owner or administrator permission to operate';

  @override
  String get livekit_error_request_noPermission =>
      'No permission for signaling request, such as canceling an invitation that was not initiated by oneself';

  @override
  String get livekit_error_requestId_invalid =>
      'Invalid signaling request ID or has already been processed';

  @override
  String get livekit_error_repeat_requestId => 'Duplicate signaling request';

  @override
  String get livekit_error_conflict_requestId =>
      'Conflicting signaling request';

  @override
  String get livekit_error_max_seat_count_limit =>
      'Maximum number of seats exceeds the package limit';

  @override
  String get livekit_error_already_in_seat =>
      'The current user is already on the seat';

  @override
  String get livekit_error_seat_occupied =>
      'The current seat is already occupied';

  @override
  String get livekit_error_seat_locked => 'The current seat is locked';

  @override
  String get livekit_error_seat_index_not_exist => 'Seat number does not exist';

  @override
  String get livekit_error_user_not_in_seat =>
      'The current user is not on the seat';

  @override
  String get livekit_error_all_seat_occupied => 'All seats are full';

  @override
  String get livekit_error_seat_not_support_link_mic =>
      'Does not support linking microphones';

  @override
  String get livekit_error_open_microphone_need_seat_unlock =>
      'The current seat audio is locked, you need to unlock it before opening the microphone';

  @override
  String get livekit_error_open_microphone_need_permission_from_admin =>
      'You need to apply to the room owner or administrator before opening the microphone';

  @override
  String get livekit_error_open_camera_need_seat_unlock =>
      'The current seat video is locked, you need to unlock it before opening the camera';

  @override
  String get livekit_error_open_camera_need_permission_from_admin =>
      'You need to apply to the room owner or administrator before opening the camera';

  @override
  String get livekit_error_open_screen_share_need_seat_unlock =>
      'The current seat screen sharing is locked, you need to unlock it before opening the screen sharing';

  @override
  String get livekit_error_open_screen_share_need_permission_from_admin =>
      'You need to apply to the room owner or administrator before opening the screen sharing';

  @override
  String get livekit_error_send_message_disabled_for_all =>
      'All users are muted in the current room';

  @override
  String get livekit_error_send_message_disabled_for_current =>
      'You have been muted in the current room';

  @override
  String get livekit_take_seat_rejected =>
      'Take seat application has been rejected';

  @override
  String get livekit_room_destroy => 'Broadcast has been ended';

  @override
  String get livekit_kicked_out_of_seat => 'Kicked out of seat by room owner';

  @override
  String get livekit_take_seat_timeout => 'Take seat application timeout';

  @override
  String get livekit_anchor => 'Anchor';

  @override
  String get livekit_audience => 'Audience';

  @override
  String get livekit_edit_cover => 'Edit';

  @override
  String get livekit_stream_categories => 'Live type:';

  @override
  String get livekit_stream_privacy_status => 'Live mode:';

  @override
  String get livekit_stream_privacy_status_public => 'Public';

  @override
  String get livekit_stream_privacy_status_privacy => 'Privacy';

  @override
  String get livekit_stream_categories_daily_chat => 'Daily chat';

  @override
  String get livekit_stream_categories_appearance => 'Appearance';

  @override
  String get livekit_stream_categories_knowledge_teaching =>
      'Knowledge teaching';

  @override
  String get livekit_stream_categories_shopping => 'Shopping';

  @override
  String get livekit_stream_categories_music => 'Music';

  @override
  String get livekit_function_item_beauty => 'Beauty';

  @override
  String get livekit_function_item_music => 'Music';

  @override
  String get livekit_function_item_flip => 'Flip';

  @override
  String get livekit_function_item_mirror => 'Mirror';

  @override
  String get livekit_start_live => 'Go live';

  @override
  String get livekit_preset_cover => 'Preset Images';

  @override
  String get livekit_set_as_cover => 'Set as cover';

  @override
  String get livekit_stream_type => 'Live categories';

  @override
  String get livekit_stream_id => 'Stream Id';

  @override
  String get livekit_barrage_input_hint => 'Please enter barrage';

  @override
  String get livekit_barrage_btn_send => 'Send';

  @override
  String get livekit_barrage_me => 'Me';

  @override
  String get livekit_barrage_agree => 'Agree';

  @override
  String get livekit_barrage_warning_not_empty => 'input can\'t be empty!';

  @override
  String get livekit_settings => 'Settings';

  @override
  String get livekit_audio_effect => 'Audio';

  @override
  String get livekit_more_settings => 'More Config';

  @override
  String get livekit_video_params => 'Video Config';

  @override
  String get livekit_video_config => 'Video';

  @override
  String get livekit_link => 'LINK';

  @override
  String get livekit_pk => 'Anchor PK';

  @override
  String get livekit_send_gift => 'Send a Gift';

  @override
  String get livekit_like => 'like';

  @override
  String get livekit_comment => 'Comment';

  @override
  String get livekit_hidden_audience_nickname => 'Hide audience nickname';

  @override
  String get livekit_allow_share => 'Allow sharing';

  @override
  String get livekit_waiting_pass => 'Waiting';

  @override
  String get livekit_gift_give_gift => 'Give gift';

  @override
  String get livekit_gift_title => 'Gift';

  @override
  String get livekit_gift_me => 'Me';

  @override
  String get livekit_music => 'Music';

  @override
  String get livekit_music_cheerful => 'Cheerful';

  @override
  String get livekit_music_melancholy => 'Melancholy';

  @override
  String get livekit_music_wonder_world => 'Wonder World';

  @override
  String get livekit_clarity => 'Clarity';

  @override
  String get livekit_fps => 'FPS';

  @override
  String get livekit_ear_return => 'Ear return';

  @override
  String get livekit_ear_return_volume => 'Ear return volume';

  @override
  String get livekit_background_music => 'Background music';

  @override
  String get livekit_select_music => 'Select music';

  @override
  String get livekit_audio_settings => 'Audio settings';

  @override
  String get livekit_music_volume => 'Music volume';

  @override
  String get livekit_people_volume => 'Voice volume';

  @override
  String get livekit_music_pitch => 'Music pitch';

  @override
  String get livekit_change_voice => 'Voice change';

  @override
  String get livekit_change_voice_none => 'Original voice';

  @override
  String get livekit_change_voice_child => 'Naughty child';

  @override
  String get livekit_change_voice_girl => 'Lori';

  @override
  String get livekit_change_voice_uncle => 'Uncle';

  @override
  String get livekit_change_voice_metal => 'Heavy metal';

  @override
  String get livekit_change_voice_cold => 'Cold';

  @override
  String get livekit_change_voice_foreign_language => 'Foreign language accent';

  @override
  String get livekit_change_voice_trapped_beast => 'Trapped beast';

  @override
  String get livekit_change_voice_fat_house => 'fat house';

  @override
  String get livekit_change_voice_strong_current => 'Strong current';

  @override
  String get livekit_change_voice_machinery => 'Heavy machinery';

  @override
  String get livekit_change_voice_ethereal => 'Ethereal';

  @override
  String get livekit_reverb => 'Reverb';

  @override
  String get livekit_reverb_none => 'No effect';

  @override
  String get livekit_reverb_karaoke => 'KTV';

  @override
  String get livekit_reverb_small_room => 'Small room';

  @override
  String get livekit_reverb_town_hall => 'Town Hall';

  @override
  String get livekit_reverb_low => 'Low';

  @override
  String get livekit_reverb_loud_and_loud => 'loud';

  @override
  String get livekit_reverb_metallic_sound => 'Metallic sound';

  @override
  String get livekit_reverb_metallic_magnetic => 'Magnetic';

  @override
  String get livekit_reverb_metallic_ethereal => 'Ethereal';

  @override
  String get livekit_reverb_metallic_recording_studio => 'Recording studio';

  @override
  String get livekit_reverb_metallic_melodious => 'melodious';

  @override
  String get livekit_reverb_metallic_recording_studio2 => 'Recording Studio 2';

  @override
  String get livekit_tips_title => 'Warm Tips';

  @override
  String get livekit_confirm_delete_tips =>
      'Are you sure you want to delete \"%s\"?';

  @override
  String get livekit_cancel => 'Cancel';

  @override
  String get livekit_confirm => 'Confirm';

  @override
  String get livekit_waiting_link => 'Waiting Linking';

  @override
  String get livekit_live_room_list => 'Live';

  @override
  String get livekit_live_has_stop => 'Live broadcast has ended';

  @override
  String get livekit_sent => ' sent ';

  @override
  String get livekit_entered_room => 'Entered room';

  @override
  String get livekit_who_live_room => ' Live Room';

  @override
  String get livekit_gift_balance => 'Balance';

  @override
  String get livekit_gift_recharge => 'Recharge';

  @override
  String get livekit_gift_balance_insufficient => 'Balance is insufficient';

  @override
  String get livekit_preview_video_live => 'Live Video';

  @override
  String get livekit_preview_voice_live => 'Live Voice';

  @override
  String get livekit_audience_count_in_room => '%d people are watching';

  @override
  String get livekit_loading => 'Loading...';

  @override
  String get livekit_no_more_data => 'There is no more data';

  @override
  String get livekit_no_room_tip => 'Please start a live room';

  @override
  String get livekit_common_this_live_data => 'Live data';

  @override
  String get livekit_common_live_duration => 'Duration';

  @override
  String get livekit_common_gift_income => 'Gift Income';

  @override
  String get livekit_common_live_people_number => 'Total Viewers';

  @override
  String get livekit_common_message_count => 'Messages';

  @override
  String get livekit_common_send_gift_people_count => 'Gift givers';

  @override
  String get livekit_common_like_count => 'Likes';

  @override
  String get livekit_anchor_audience_list_panel_title => 'Online audience';

  @override
  String get livekit_follow_anchor => 'Follow';

  @override
  String get livekit_unfollow_anchor => 'unfollow';

  @override
  String get livekit_live_room_id => 'Live Room ID:';

  @override
  String get livekit_fan_count => 'Fans';

  @override
  String get livekit_beauty_panel_title => 'One-click beauty';

  @override
  String get livekit_beauty_item_none => 'Close';

  @override
  String get livekit_beauty_item_smooth => 'Smooth';

  @override
  String get livekit_beauty_item_whiteness => 'Whiteness';

  @override
  String get livekit_beauty_item_ruddy => 'Ruddy';

  @override
  String get livekit_resolution_360p => 'LD';

  @override
  String get livekit_resolution_540p => 'SD';

  @override
  String get livekit_resolution_720p => 'HD';

  @override
  String get livekit_resolution_1080p => 'Ultra HD';

  @override
  String get livekit_enable_audience_request_link =>
      'Allow viewers to apply for continuous microphone';

  @override
  String get livekit_link_mic_up_title => 'Current wheat position';

  @override
  String get livekit_link_mic_down_title => 'Apply for continuous wheat';

  @override
  String get livekit_link_mic_down_title_popup => 'Requesting';

  @override
  String get livekit_title_link_mic_selector => 'Choose Link Mode';

  @override
  String get livekit_text_link_mic_selector =>
      'connect upon host\\\'s approval';

  @override
  String get livekit_text_link_mic_video => 'Apply for video link';

  @override
  String get livekit_text_link_mic_audio => 'Apply for audio link';

  @override
  String get livekit_text_cancel_link_mic_apply =>
      'Cancel application for link mic';

  @override
  String get livekit_text_close_link_mic => 'End the link mic';

  @override
  String get livekit_link_mic_manager => 'Link Management';

  @override
  String get livekit_hang_up => 'Hang Up';

  @override
  String get livekit_accept => 'Accept';

  @override
  String get livekit_reject => 'Reject';

  @override
  String get livekit_toast_apply_link_mic =>
      'You have submitted a link mic request, please wait for the author approval';

  @override
  String get livekit_title_link_video_settings =>
      'Adjust the video link mic screen';

  @override
  String get livekit_btn_apply_link_mic => 'Apply for link mic';

  @override
  String get livekit_tips_apply_link_mic =>
      'The screen effect will automatically take effect after connecting';
}
