import 'livekit_localizations.dart';

// ignore_for_file: type=lint

/// The translations for English (`en`).
class LiveKitLocalizationsEn extends LiveKitLocalizations {
  LiveKitLocalizationsEn([String locale = 'en']) : super(locale);

  @override
  String get common_go_live => 'Go Live';

  @override
  String get common_link_mic_up_title => 'Current Mic';

  @override
  String get common_audience_end_link_tips =>
      'You are currently co-guesting with other streamers. Would you like to[End Co-guest] or [Exit Live] ?';

  @override
  String get common_exit_live => 'Exit Live';

  @override
  String get common_gift_me => 'Me';

  @override
  String get common_resolution_1080p => '1080p';

  @override
  String get common_stream_categories => 'Live type:';

  @override
  String get common_stream_privacy_status => 'Live Mode:';

  @override
  String get common_voiceroom_kicked_out_of_seat =>
      'Kicked out of seat by room owner';

  @override
  String get common_like => 'Likes';

  @override
  String get common_apply_link_mic => 'Apply for link mic';

  @override
  String get common_client_error_already_in_other_room =>
      'User is Already in Another Room, Single RoomEngine Instance Only Supports User Entering One Room, To Enter Different Room, Please Leave the Room or Use New RoomEngine Instance';

  @override
  String get common_client_error_camera_device_empty =>
      'No Camera Device Currently, Please Insert Camera Device to Solve the Problem';

  @override
  String get common_client_error_camera_not_authorized =>
      'Camera has No System Authorization, Check System Authorization';

  @override
  String get common_client_error_camera_occupied =>
      'Camera is Occupied, Check if Other Process is Using Camera';

  @override
  String get common_client_error_camera_start_fail =>
      'System Issue, Failed to Open Camera. Check if Camera Device is Normal';

  @override
  String get common_client_error_connection_connecting =>
      'The room you are invited to connect to is already in the invitation list or is already connected.';

  @override
  String get common_client_error_exit_not_supported_for_room_owner =>
      'Room Owner Does Not Support Leaving the Room, Room Owner Can Only Close the Room';

  @override
  String get common_client_error_failed =>
      'Temporarily Unclassified General Error';

  @override
  String get common_client_error_freq_limit =>
      'Request Rate Limited, Please Try Again Later';

  @override
  String get common_client_error_get_screen_sharing_target_failed =>
      'Failed to get screen sharing source (screen and window), check screen recording permissions';

  @override
  String get common_client_error_invalid_parameter =>
      'Passing illegal parameters when calling API, check if the parameters are legal';

  @override
  String get common_client_error_max_seat_count_limit =>
      'Maximum Seat Exceeds Package Quantity Limit';

  @override
  String get common_client_error_microphone_device_empty =>
      'No Mic Device Currently';

  @override
  String get common_client_error_microphone_not_authorized =>
      'Mic has No System Authorization, Check System Authorization';

  @override
  String get common_client_error_microphone_occupied => 'Mic is Occupied';

  @override
  String get common_client_error_microphone_start_fail =>
      'System Issue, Failed to Open Mic. Check if Mic Device is Normal';

  @override
  String get common_client_error_open_camera_need_permission_from_admin =>
      'Need to Apply to Room Owner or Administrator to Open Camera';

  @override
  String get common_client_error_open_camera_need_seat_unlock =>
      'Current Seat Video is Locked, Need Room Owner to Unlock Mic Seat Before Opening Camera';

  @override
  String get common_client_error_open_microphone_need_permission_from_admin =>
      'Need to Apply to Room Owner or Administrator to Open Mic';

  @override
  String get common_client_error_open_microphone_need_seat_unlock =>
      'Current Seat Audio is Locked';

  @override
  String get common_client_error_open_screen_share_need_permission_from_admin =>
      'Screen sharing needs to be enabled after applying to the room owner or administrator';

  @override
  String get common_client_error_open_screen_share_need_seat_unlock =>
      'The current microphone position video is locked and needs to be unlocked by the room owner before screen sharing can be enabled';

  @override
  String get common_client_error_operation_invalid_before_enter_room =>
      'This Feature Can Only Be Used After Entering the Room';

  @override
  String get common_client_error_operation_not_supported_in_current_room_type =>
      'This Operation is Not Supported in the Current Room Type';

  @override
  String get common_client_error_permission_denied =>
      'Failed to Obtain Permission, Unauthorized Audio/Video Permission, Please Check if Device Permission is Enabled';

  @override
  String get common_client_error_repeat_operation => 'Repeat Operation';

  @override
  String get common_client_error_request_id_invalid =>
      'Signaling Request ID is Invalid or Has Been Processed';

  @override
  String get common_client_error_request_id_repeat =>
      'Signal request repetition';

  @override
  String get common_client_error_request_no_permission =>
      'No Permission for Signaling Request, e.g. Canceling an Invite Not Initiated by Yourself';

  @override
  String get common_client_error_require_payment =>
      'This feature requires an additional package. Please activate the corresponding package as needed in the TRTC Console';

  @override
  String get common_client_error_room_id_invalid =>
      'Illegal Custom Room ID, Must Be Printable ASCII Characters (0x20-0x7e), Up to 48 Bytes Long';

  @override
  String get common_client_error_room_name_invalid =>
      'Illegal Room Name, Maximum 30 Bytes, Must Be UTF-8 Encoding if Contains Chinese Characters';

  @override
  String get common_client_error_room_not_support_preloading =>
      'The current room does not support preloading';

  @override
  String get common_client_error_sdk_app_id_not_found =>
      'Not Found SDKAppID, Please Confirm Application Info in TRTC Console';

  @override
  String get common_client_error_sdk_not_initialized =>
      'Not Logged In, Please Call Login API';

  @override
  String get common_client_error_seat_index_not_exist =>
      'Seat Serial Number Does Not Exist';

  @override
  String get common_client_error_send_message_disabled_for_all =>
      'All Members Muted in the Current Room';

  @override
  String get common_client_error_send_message_disabled_for_current =>
      'You Have Been Muted in the Current Room';

  @override
  String get common_client_error_start_screen_sharing_failed =>
      'Failed to Enable Screen Sharing, Check if Someone is Already Screen Sharing in the Room';

  @override
  String get common_client_error_success => 'Operation Successful';

  @override
  String get common_client_error_user_need_admin_permission =>
      'Room Owner or Administrator Permission Required for Operation';

  @override
  String get common_client_error_user_need_owner_permission =>
      'Room Owner Permission Required for Operation';

  @override
  String get common_client_error_user_not_exist => 'User is not exist';

  @override
  String get common_ear_return_volume => 'Ear Monitor Volume';

  @override
  String get common_fan_count => 'Fans';

  @override
  String get live_barrage_warning_not_empty => 'input can\'t be empty!';

  @override
  String get live_clarity => 'Clarity';

  @override
  String get common_seat_management => 'Seat';

  @override
  String get live_room_has_been_dismissed => 'Room has been dismissed';

  @override
  String get common_anchor_audience_list_panel_title => 'Online audience';

  @override
  String get common_app_running => 'Running';

  @override
  String get common_audio_effect => 'Audio';

  @override
  String get common_audio_effect_settings => 'Audio Effect Settings';

  @override
  String get common_audio_settings => 'Audio settings';

  @override
  String get common_beauty_item_close => 'Close';

  @override
  String get common_beauty_item_ruddy => 'Rosy';

  @override
  String get common_beauty_item_smooth => 'Microdermabrasion';

  @override
  String get common_beauty_item_whiteness => 'Whitening';

  @override
  String get common_beauty_panel_title => 'One-click beauty';

  @override
  String get common_cancel => 'Cancel';

  @override
  String get common_change_voice => 'Voice changer';

  @override
  String get common_change_voice_child => 'Naughty child';

  @override
  String get common_change_voice_ethereal => 'Ethereal';

  @override
  String get common_change_voice_girl => 'Loli';

  @override
  String get common_change_voice_none => 'Original';

  @override
  String get common_change_voice_uncle => 'Uncle';

  @override
  String get common_common_gift_income => 'Gift Income';

  @override
  String get common_common_like_count => 'Likes Count';

  @override
  String get common_common_live_duration => 'Duration';

  @override
  String get common_common_live_people_number => 'Total Views';

  @override
  String get common_common_message_count => 'Messages';

  @override
  String get common_common_send_gift_people_count => 'Gift givers';

  @override
  String get common_common_this_live_data => 'Live data';

  @override
  String get common_ear_return => 'Ear Monitor';

  @override
  String get common_edit_cover => 'Modify the cover';

  @override
  String get common_end_link => 'End Co-guest';

  @override
  String get common_end_live => 'End Live';

  @override
  String get common_end_user => 'End';

  @override
  String get common_end_connection => 'End Co-host';

  @override
  String get common_end_connection_tips =>
      'You are currently co-hosting with other streamers. Would you like to [End Co-host] or [End Live] ?';

  @override
  String get common_end_pk => 'End PK';

  @override
  String get common_end_pk_tips =>
      'You are currently in PK mode. Would you like to [End PK] or [End Live] ?';

  @override
  String get common_entered_room => 'Entered room';

  @override
  String get common_follow_anchor => 'Follow';

  @override
  String get common_gift_give_gift => 'Send Out';

  @override
  String get common_gift_title => 'Gift';

  @override
  String get common_hang_up => 'Hang Up';

  @override
  String get common_kick_user_confirm_message =>
      'Are you sure you want to remove %s?';

  @override
  String get common_link_mic_manager => 'Link Management';

  @override
  String get common_link => 'Link';

  @override
  String get common_live_has_stop => 'Live broadcast has ended';

  @override
  String get common_more_settings => 'More settings';

  @override
  String get common_music => 'Music';

  @override
  String get common_music_cheerful => 'Cheerful';

  @override
  String get common_music_confirm => 'Confirm';

  @override
  String get common_music_melancholy => 'Melancholy';

  @override
  String get common_music_tips_title => 'Warm Tips';

  @override
  String get common_music_volume => 'Music volume';

  @override
  String get common_music_wonder_world => 'Magical World';

  @override
  String get common_people_volume => 'Voice volume';

  @override
  String get common_receive => 'Accept';

  @override
  String get common_reject => 'Reject';

  @override
  String get common_resolution_360p => '360p';

  @override
  String get common_resolution_540p => '540p';

  @override
  String get common_resolution_720p => '720p';

  @override
  String get common_reverb => 'Reverb';

  @override
  String get common_reverb_karaoke => 'KTV';

  @override
  String get common_reverb_loud_and_loud => 'Loud';

  @override
  String get common_reverb_low => 'Low';

  @override
  String get common_reverb_metallic_sound => 'Metallic sound';

  @override
  String get common_reverb_none => 'No effect';

  @override
  String get common_room_destroy => 'Broadcast has been ended';

  @override
  String get common_room_info_liveroom_id => 'Live Room ID:';

  @override
  String get common_sent => 'Sent';

  @override
  String get common_server_error_already_on_the_mic => 'Already on the seat';

  @override
  String get common_server_error_already_on_the_mic_queue =>
      'Already on the seat queue';

  @override
  String get common_server_error_battle_does_not_exist_or_has_ended =>
      'The battle does not exist or has ended';

  @override
  String get common_server_error_battle_session_has_ended =>
      'The battle session has ended';

  @override
  String get common_server_error_connection_does_not_exist =>
      'The current connection does not exist or has ended';

  @override
  String get common_server_error_creating_battles_too_frequently =>
      'creating battles too frequently. Wait a moment and try again';

  @override
  String get common_server_error_creating_connections_too_frequent =>
      'creating connections too frequent in a short time. Wait a moment and try again';

  @override
  String get common_server_error_creating_rooms_exceeds_the_frequency_limit =>
      'Creating rooms exceeds the frequency limit, the same room ID can only be created once within 1 second';

  @override
  String get common_server_error_exceeds_the_upper_limit =>
      'Exceeds the upper limit, for example, the number of microphone seats, the number of PK match rooms, etc., exceeds the payment limit';

  @override
  String
      get common_server_error_has_exceeded_the_limit_in_connection_or_battle =>
          'The room number has exceeded the limit in connection or battle';

  @override
  String get common_server_error_in_other_battle =>
      'The room is already in other battle';

  @override
  String get common_server_error_insufficient_operation_permissions =>
      'You do not have permission to perform this operation';

  @override
  String get common_server_error_invalid_room_type => 'Invalid room type';

  @override
  String get common_server_error_is_connecting_with_other_rooms =>
      'The current room is connecting with other rooms';

  @override
  String
      get common_server_error_is_not_allowed_to_cancel_battle_for_room_in_battle =>
          'It\'s not allowed to cancel battle for room in battle';

  @override
  String get common_server_error_metadata_no_valid_keys =>
      'There is no valid keys when delete metadata';

  @override
  String get common_server_error_metadata_number_of_keys_exceeds_the_limit =>
      'The number of keys in the room\'s Metadata exceeds the limit';

  @override
  String get common_server_error_metadata_size_of_value_exceeds_the_limit =>
      'The size of value in the room\'s Metadata exceeds the maximum byte limit';

  @override
  String get common_server_error_metadata_the_size_of_key_exceeds_the_maximum_byte_limit =>
      'The size of key in the room\'s Metadata exceeds the maximum byte limit';

  @override
  String get common_server_error_metadata_total_size_exceeds_the_limit =>
      'The total size of all value in the room\'s Metadata exceeds the maximum byte limit';

  @override
  String get common_server_error_mic_seat_is_locked =>
      'The seat is locked. You can try another seat';

  @override
  String get common_server_error_no_payment_information =>
      'No payment information, you need to purchase a package in the console';

  @override
  String get common_server_error_no_rooms_in_the_battle_is_valid =>
      'None of the rooms in the battle is valid';

  @override
  String get common_server_error_not_a_room_member => 'Not a room member';

  @override
  String get common_server_error_not_on_the_mic_queue =>
      'Not on the seat queue';

  @override
  String get common_server_error_not_on_the_mic_seat => 'Not on the seat';

  @override
  String get common_server_error_not_started_yet =>
      'The battle has not started yet';

  @override
  String get common_server_error_param_illegal =>
      'The parameter is illegal. Check whether the request is correct according to the error description';

  @override
  String get common_server_error_requires_password =>
      'The current room requires a password for entry';

  @override
  String get common_server_error_room_admin_quantity_exceeds_the_upper_limit =>
      'The admin quantity exceeds the upper limit';

  @override
  String get common_server_error_room_does_not_exist =>
      'The room does not exist, or it once existed but has now been dissolved';

  @override
  String get common_server_error_room_does_not_support_mic_ability =>
      'The room does not support seat ability';

  @override
  String get common_server_error_room_entry_password_error =>
      'Room Entry Password Error';

  @override
  String get common_server_error_room_id_exists =>
      'The room ID already exists. Please select another room ID';

  @override
  String get common_server_error_room_id_has_been_occupied_by_chat =>
      'The room ID has been occupied by Chat. You can use a different room ID or dissolve the group first';

  @override
  String get common_server_error_room_id_has_been_used =>
      'The room ID has been used, and the operator is the room owner, it can be used directly';

  @override
  String get common_server_error_room_is_full => 'The room is full';

  @override
  String get common_server_error_room_is_in_connection =>
      'The room is already in connection';

  @override
  String get common_server_error_seat_is_already_occupied =>
      'The current seat is already occupied';

  @override
  String get common_server_error_signal_request_conflict =>
      'Signal request conflict';

  @override
  String get common_server_error_system_internal_error =>
      'Server internal error, please retry';

  @override
  String get common_server_error_tag_quantity_exceeds_upper_limit =>
      'Tag quantity Exceeds Upper limit';

  @override
  String get common_server_error_the_room_is_not_in_the_battle =>
      'The room isn‘t in the battle';

  @override
  String get common_server_error_the_seat_list_is_empty =>
      'The seat list is empty';

  @override
  String get common_server_error_the_seats_are_all_taken =>
      'The seats are all taken.';

  @override
  String get common_server_error_there_is_a_pending_battle_request =>
      'There is a pending battle request for this room';

  @override
  String get common_server_error_there_is_a_pending_connection_request =>
      'There is a pending connection request for this room';

  @override
  String get common_server_error_this_member_has_been_banned =>
      'This member has been banned';

  @override
  String get common_server_error_this_member_has_been_muted =>
      'This member has been muted';

  @override
  String get common_server_error_user_is_already_on_the_mic_seat =>
      'The user is already on the seat';

  @override
  String get common_set_as_background => 'Set as background';

  @override
  String get common_set_as_cover => 'Set as cover';

  @override
  String get common_settings => 'Settings';

  @override
  String get common_settings_bg_image => 'Background';

  @override
  String get common_stream_categories_beauty => 'Beauty';

  @override
  String get common_stream_categories_default => 'Daily chat';

  @override
  String get common_stream_categories_shopping => 'Shopping';

  @override
  String get common_stream_categories_teach => 'Knowledge Teaching';

  @override
  String get common_stream_privacy_status_default => 'Public';

  @override
  String get common_stream_privacy_status_privacy => 'Privacy';

  @override
  String get common_text_cancel_link_mic_apply =>
      'Cancel application for link mic';

  @override
  String get common_text_close_link_mic => 'End Link';

  @override
  String get common_text_link_mic_audio => 'Apply for audio link';

  @override
  String get common_text_link_mic_selector => 'connect upon host\'s approval';

  @override
  String get common_text_link_mic_video => 'Apply for video link';

  @override
  String get common_tips_apply_link_mic =>
      'The screen effect will automatically take effect after connecting';

  @override
  String get common_title_preset_cover => 'Preset Images';

  @override
  String get common_title_link_mic_selector => 'Choose Link Mode';

  @override
  String get common_title_link_video_settings => 'Adjust the video link screen';

  @override
  String get common_toast_apply_link_mic =>
      'You have submitted a link mic request, please wait for the author approval';

  @override
  String get common_unfollow_anchor => 'Unfollow';

  @override
  String get common_video_config => 'Video';

  @override
  String get common_video_params => 'Video Config';

  @override
  String get common_video_settings_item_beauty => 'Beauty';

  @override
  String get common_video_settings_item_flip => 'Flip';

  @override
  String get common_video_settings_item_mirror => 'Mirror';

  @override
  String get common_voiceroom_empty_view =>
      'No users in the seat, go to invite';

  @override
  String get common_voiceroom_invite => 'Invite';

  @override
  String get common_voiceroom_invite_seat_canceled =>
      'Seat invitation has been canceled';

  @override
  String get common_voiceroom_lock => 'Lock Seat';

  @override
  String get common_voiceroom_mute_seat => 'Mute';

  @override
  String get common_voiceroom_need_agree => 'Require owner\'s consent to speak';

  @override
  String get common_voiceroom_receive_seat_invitation =>
      '%1\$s invites you to take seat';

  @override
  String get common_voiceroom_take_seat => 'Take Seat';

  @override
  String get common_voiceroom_take_seat_rejected =>
      'Take seat application has been rejected';

  @override
  String get common_voiceroom_take_seat_timeout =>
      'Take seat application timeout';

  @override
  String get common_voiceroom_unlock => 'Unlock Seat';

  @override
  String get common_voiceroom_unmuted_seat => 'Unmute';

  @override
  String get common_waiting_pass => 'Waiting';

  @override
  String get live_barrage_agree => 'Agree';

  @override
  String get live_barrage_btn_send => 'Send';

  @override
  String get live_choose_music => 'Choose Music';

  @override
  String get live_failed_to_enter_room => 'Failed to enter room';

  @override
  String get live_invalid_userId => 'Invalid userId';

  @override
  String get live_music_pitch => 'Music Pitch';

  @override
  String get common_accept => 'Accept';

  @override
  String get livelist_viewed_audience_count => 'xxx people viewed';

  @override
  String get livelist_loading => 'Loading...';

  @override
  String get livelist_no_more_data => 'There is no more data';

  @override
  String get common_anchor_battle => 'Battle';

  @override
  String get common_anchor_end_link_tips =>
      'You are currently co-guesting with other streamers. Would you like to [End Live] ?';

  @override
  String get common_battle_connecting => 'Connecting';

  @override
  String get common_battle_end_pk => 'End PK';

  @override
  String get common_battle_end_pk_tips =>
      'Are you sure you want to end the battle? The current result will be the final result after the end';

  @override
  String get common_battle_invitation_timeout =>
      'Battle request has been timeout';

  @override
  String get common_battle_invitee_reject => 'xxx rejected battle';

  @override
  String get common_battle_inviter_cancel =>
      'xxx canceled battle, please try to initiate it again';

  @override
  String get common_battle_inviting => 'xxx invite you to battle together';

  @override
  String get common_battle_pk_end => 'PK End';

  @override
  String get common_battle_wait_start => 'Waiting for battle';

  @override
  String get common_battle_wait_stop => 'Cancel';

  @override
  String get common_connect_conflict =>
      'The room you are invited to connect to is connected to another room.';

  @override
  String get common_connect_error => 'Other errors, cannot connect.';

  @override
  String get common_connect_inviting => 'Waiting';

  @override
  String get common_connect_inviting_append =>
      'xxx invite you to host together';

  @override
  String get common_connect_request_rejected =>
      'Connection application has been rejected';

  @override
  String get common_connection => 'Start Co-hosting';

  @override
  String get common_connection_list_title => 'Connecting Streamers(xxx)';

  @override
  String get common_connection_room_full =>
      'The number of co-hosting has exceeded the maximum limit.';

  @override
  String get common_send_message_enable =>
      'You have been unmuted in the current room';

  @override
  String get common_link_host => 'Host';

  @override
  String get common_link_guest => 'Guest';

  @override
  String get common_recommended_list => 'Suggested Hosts';

  @override
  String get common_disconnection => 'Disconnect';

  @override
  String get common_disconnect_tips =>
      'Are you sure you want to disconnect from other streamers?';

  @override
  String get common_end_connect => 'Disconnect';

  @override
  String get common_more => 'More';

  @override
  String get common_more_features => 'More Features';

  @override
  String get live_error_connection_notexit =>
      'The room you are invited to connect to does not exist';

  @override
  String get live_error_connection_retry =>
      'Internal error, it is recommended to try again.';

  @override
  String get livestreamcore_battle_error_conflict =>
      'The anchor is in the battle and cannot initiate the battle';

  @override
  String get livestreamcore_battle_error_other =>
      'The other error, cannot initiate the battle';

  @override
  String get common_text_terminate_connection => 'Disconnection';

  @override
  String get common_text_leave_room => 'Leave live room';

  @override
  String get common_text_terminate_connection_tips =>
      'You are currently connected, do you want to "DisConnection" or "Leave live room"?';
}
