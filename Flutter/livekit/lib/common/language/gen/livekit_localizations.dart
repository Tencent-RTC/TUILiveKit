import 'dart:async';

import 'package:flutter/foundation.dart';
import 'package:flutter/widgets.dart';
import 'package:flutter_localizations/flutter_localizations.dart';
import 'package:intl/intl.dart' as intl;

import 'livekit_localizations_en.dart';
import 'livekit_localizations_zh.dart';

// ignore_for_file: type=lint

/// Callers can lookup localized strings with an instance of LiveKitLocalizations
/// returned by `LiveKitLocalizations.of(context)`.
///
/// Applications need to include `LiveKitLocalizations.delegate()` in their app's
/// `localizationDelegates` list, and the locales they support in the app's
/// `supportedLocales` list. For example:
///
/// ```dart
/// import 'gen/livekit_localizations.dart';
///
/// return MaterialApp(
///   localizationsDelegates: LiveKitLocalizations.localizationsDelegates,
///   supportedLocales: LiveKitLocalizations.supportedLocales,
///   home: MyApplicationHome(),
/// );
/// ```
///
/// ## Update pubspec.yaml
///
/// Please make sure to update your pubspec.yaml to include the following
/// packages:
///
/// ```yaml
/// dependencies:
///   # Internationalization support.
///   flutter_localizations:
///     sdk: flutter
///   intl: any # Use the pinned version from flutter_localizations
///
///   # Rest of dependencies
/// ```
///
/// ## iOS Applications
///
/// iOS applications define key application metadata, including supported
/// locales, in an Info.plist file that is built into the application bundle.
/// To configure the locales supported by your app, you’ll need to edit this
/// file.
///
/// First, open your project’s ios/Runner.xcworkspace Xcode workspace file.
/// Then, in the Project Navigator, open the Info.plist file under the Runner
/// project’s Runner folder.
///
/// Next, select the Information Property List item, select Add Item from the
/// Editor menu, then select Localizations from the pop-up menu.
///
/// Select and expand the newly-created Localizations item then, for each
/// locale your application supports, add a new item and select the locale
/// you wish to add from the pop-up menu in the Value field. This list should
/// be consistent with the languages listed in the LiveKitLocalizations.supportedLocales
/// property.
abstract class LiveKitLocalizations {
  LiveKitLocalizations(String locale)
      : localeName = intl.Intl.canonicalizedLocale(locale.toString());

  final String localeName;

  static LiveKitLocalizations? of(BuildContext context) {
    return Localizations.of<LiveKitLocalizations>(
        context, LiveKitLocalizations);
  }

  static const LocalizationsDelegate<LiveKitLocalizations> delegate =
      _LiveKitLocalizationsDelegate();

  /// A list of this localizations delegate along with the default localizations
  /// delegates.
  ///
  /// Returns a list of localizations delegates containing this delegate along with
  /// GlobalMaterialLocalizations.delegate, GlobalCupertinoLocalizations.delegate,
  /// and GlobalWidgetsLocalizations.delegate.
  ///
  /// Additional delegates can be added by appending to this list in
  /// MaterialApp. This list does not have to be used at all if a custom list
  /// of delegates is preferred or required.
  static const List<LocalizationsDelegate<dynamic>> localizationsDelegates =
      <LocalizationsDelegate<dynamic>>[
    delegate,
    GlobalMaterialLocalizations.delegate,
    GlobalCupertinoLocalizations.delegate,
    GlobalWidgetsLocalizations.delegate,
  ];

  /// A list of this localizations delegate's supported locales.
  static const List<Locale> supportedLocales = <Locale>[
    Locale('en'),
    Locale('zh'),
    Locale.fromSubtags(languageCode: 'zh', scriptCode: 'Hant')
  ];

  /// No description provided for @common_anchor_battle.
  ///
  /// In en, this message translates to:
  /// **'Battle'**
  String get common_anchor_battle;

  /// No description provided for @common_anchor_end_link_tips.
  ///
  /// In en, this message translates to:
  /// **'You are currently co-guesting with other streamers. Would you like to [End Live] ?'**
  String get common_anchor_end_link_tips;

  /// No description provided for @common_battle_connecting.
  ///
  /// In en, this message translates to:
  /// **'Connecting'**
  String get common_battle_connecting;

  /// No description provided for @common_battle_end_pk.
  ///
  /// In en, this message translates to:
  /// **'End PK'**
  String get common_battle_end_pk;

  /// No description provided for @common_battle_end_pk_tips.
  ///
  /// In en, this message translates to:
  /// **'Are you sure you want to end the battle? The current result will be the final result after the end'**
  String get common_battle_end_pk_tips;

  /// No description provided for @common_battle_invitation_timeout.
  ///
  /// In en, this message translates to:
  /// **'Battle request has been timeout'**
  String get common_battle_invitation_timeout;

  /// No description provided for @common_battle_invitee_reject.
  ///
  /// In en, this message translates to:
  /// **'rejected battle'**
  String get common_battle_invitee_reject;

  /// No description provided for @common_battle_inviter_cancel.
  ///
  /// In en, this message translates to:
  /// **'canceled battle, please try to initiate it again'**
  String get common_battle_inviter_cancel;

  /// No description provided for @common_battle_inviting.
  ///
  /// In en, this message translates to:
  /// **'invite you to battle together'**
  String get common_battle_inviting;

  /// No description provided for @common_battle_pk_end.
  ///
  /// In en, this message translates to:
  /// **'PK End'**
  String get common_battle_pk_end;

  /// No description provided for @common_battle_wait_start.
  ///
  /// In en, this message translates to:
  /// **'Waiting for battle'**
  String get common_battle_wait_start;

  /// No description provided for @common_battle_wait_stop.
  ///
  /// In en, this message translates to:
  /// **'Cancel'**
  String get common_battle_wait_stop;

  /// No description provided for @common_connect_conflict.
  ///
  /// In en, this message translates to:
  /// **'The room you are invited to connect to is connected to another room.'**
  String get common_connect_conflict;

  /// No description provided for @common_connect_error.
  ///
  /// In en, this message translates to:
  /// **'Other errors, cannot connect.'**
  String get common_connect_error;

  /// No description provided for @common_connect_inviting.
  ///
  /// In en, this message translates to:
  /// **'Waiting'**
  String get common_connect_inviting;

  /// No description provided for @common_connect_inviting_append.
  ///
  /// In en, this message translates to:
  /// **'invite you to host together'**
  String get common_connect_inviting_append;

  /// No description provided for @common_connect_request_rejected.
  ///
  /// In en, this message translates to:
  /// **'Connection application has been rejected'**
  String get common_connect_request_rejected;

  /// No description provided for @common_connect_invitation_timeout.
  ///
  /// In en, this message translates to:
  /// **'Invitation has timed out'**
  String get common_connect_invitation_timeout;

  /// No description provided for @common_connection.
  ///
  /// In en, this message translates to:
  /// **'Start Co-hosting'**
  String get common_connection;

  /// No description provided for @common_connection_list_title.
  ///
  /// In en, this message translates to:
  /// **'Connecting Streamers(xxx)'**
  String get common_connection_list_title;

  /// No description provided for @common_connection_room_full.
  ///
  /// In en, this message translates to:
  /// **'The number of co-hosting has exceeded the maximum limit.'**
  String get common_connection_room_full;

  /// No description provided for @common_cover.
  ///
  /// In en, this message translates to:
  /// **'Cover'**
  String get common_cover;

  /// No description provided for @common_disconnect_tips.
  ///
  /// In en, this message translates to:
  /// **'Are you sure you want to disconnect from other streamers?'**
  String get common_disconnect_tips;

  /// No description provided for @common_end_connect.
  ///
  /// In en, this message translates to:
  /// **'Disconnect'**
  String get common_end_connect;

  /// No description provided for @common_end_connection_tips.
  ///
  /// In en, this message translates to:
  /// **'You are currently co-hosting with other streamers. Would you like to [End Co-host] or [End Live] ?'**
  String get common_end_connection_tips;

  /// No description provided for @common_end_pk_tips.
  ///
  /// In en, this message translates to:
  /// **'You are currently in PK mode. Would you like to [End PK] or [End Live] ?'**
  String get common_end_pk_tips;

  /// No description provided for @common_more.
  ///
  /// In en, this message translates to:
  /// **'More'**
  String get common_more;

  /// No description provided for @common_more_features.
  ///
  /// In en, this message translates to:
  /// **'More Features'**
  String get common_more_features;

  /// No description provided for @common_recommended_list.
  ///
  /// In en, this message translates to:
  /// **'Suggested Hosts'**
  String get common_recommended_list;

  /// No description provided for @common_link_mic_up_title.
  ///
  /// In en, this message translates to:
  /// **'Current Mic'**
  String get common_link_mic_up_title;

  /// No description provided for @common_send_message_disabled.
  ///
  /// In en, this message translates to:
  /// **'You have been muted in the current room'**
  String get common_send_message_disabled;

  /// No description provided for @common_send_message_enable.
  ///
  /// In en, this message translates to:
  /// **'You have been unmuted in the current room'**
  String get common_send_message_enable;

  /// No description provided for @common_start_live.
  ///
  /// In en, this message translates to:
  /// **'Start Live'**
  String get common_start_live;

  /// No description provided for @common_link_guest.
  ///
  /// In en, this message translates to:
  /// **'Guest'**
  String get common_link_guest;

  /// No description provided for @common_link_host.
  ///
  /// In en, this message translates to:
  /// **'Host'**
  String get common_link_host;

  /// No description provided for @live_error_connection_notexit.
  ///
  /// In en, this message translates to:
  /// **'The room you are invited to connect to does not exist'**
  String get live_error_connection_notexit;

  /// No description provided for @live_error_connection_retry.
  ///
  /// In en, this message translates to:
  /// **'Internal error, it is recommended to try again.'**
  String get live_error_connection_retry;

  /// No description provided for @livelist_loading.
  ///
  /// In en, this message translates to:
  /// **'Loading...'**
  String get livelist_loading;

  /// No description provided for @livelist_no_more_data.
  ///
  /// In en, this message translates to:
  /// **'There is no more data'**
  String get livelist_no_more_data;

  /// No description provided for @livelist_viewed_audience_count.
  ///
  /// In en, this message translates to:
  /// **'xxx people viewed'**
  String get livelist_viewed_audience_count;

  /// No description provided for @livestreamcore_battle_error_conflict.
  ///
  /// In en, this message translates to:
  /// **'The anchor is in the battle and cannot initiate the battle'**
  String get livestreamcore_battle_error_conflict;

  /// No description provided for @livestreamcore_battle_error_other.
  ///
  /// In en, this message translates to:
  /// **'The other error, cannot initiate the battle'**
  String get livestreamcore_battle_error_other;

  /// No description provided for @common_audience_end_link_tips.
  ///
  /// In en, this message translates to:
  /// **'You are currently co-guesting with other streamers. Would you like to [End Co-guest] or [Exit Live] ?'**
  String get common_audience_end_link_tips;

  /// No description provided for @common_exit_live.
  ///
  /// In en, this message translates to:
  /// **'Exit Live'**
  String get common_exit_live;

  /// No description provided for @common_gift_me.
  ///
  /// In en, this message translates to:
  /// **'Me'**
  String get common_gift_me;

  /// No description provided for @common_resolution_1080p.
  ///
  /// In en, this message translates to:
  /// **'1080p'**
  String get common_resolution_1080p;

  /// No description provided for @common_stream_categories.
  ///
  /// In en, this message translates to:
  /// **'Live type:'**
  String get common_stream_categories;

  /// No description provided for @common_stream_privacy_status.
  ///
  /// In en, this message translates to:
  /// **'Live Mode:'**
  String get common_stream_privacy_status;

  /// No description provided for @common_voiceroom_kicked_out_of_seat.
  ///
  /// In en, this message translates to:
  /// **'Kicked out of seat by room owner'**
  String get common_voiceroom_kicked_out_of_seat;

  /// No description provided for @common_like.
  ///
  /// In en, this message translates to:
  /// **'Likes'**
  String get common_like;

  /// No description provided for @common_apply_link_mic.
  ///
  /// In en, this message translates to:
  /// **'Apply for link mic'**
  String get common_apply_link_mic;

  /// No description provided for @common_client_error_already_in_other_room.
  ///
  /// In en, this message translates to:
  /// **'User is Already in Another Room, Single RoomEngine Instance Only Supports User Entering One Room, To Enter Different Room, Please Leave the Room or Use New RoomEngine Instance'**
  String get common_client_error_already_in_other_room;

  /// No description provided for @common_client_error_camera_device_empty.
  ///
  /// In en, this message translates to:
  /// **'No Camera Device Currently, Please Insert Camera Device to Solve the Problem'**
  String get common_client_error_camera_device_empty;

  /// No description provided for @common_client_error_camera_not_authorized.
  ///
  /// In en, this message translates to:
  /// **'Camera has No System Authorization, Check System Authorization'**
  String get common_client_error_camera_not_authorized;

  /// No description provided for @common_client_error_camera_occupied.
  ///
  /// In en, this message translates to:
  /// **'Camera is Occupied, Check if Other Process is Using Camera'**
  String get common_client_error_camera_occupied;

  /// No description provided for @common_client_error_camera_start_fail.
  ///
  /// In en, this message translates to:
  /// **'System Issue, Failed to Open Camera. Check if Camera Device is Normal'**
  String get common_client_error_camera_start_fail;

  /// No description provided for @common_client_error_connection_connecting.
  ///
  /// In en, this message translates to:
  /// **'The room you are invited to connect to is already in the invitation list or is already connected.'**
  String get common_client_error_connection_connecting;

  /// No description provided for @common_client_error_exit_not_supported_for_room_owner.
  ///
  /// In en, this message translates to:
  /// **'Room Owner Does Not Support Leaving the Room, Room Owner Can Only Close the Room'**
  String get common_client_error_exit_not_supported_for_room_owner;

  /// No description provided for @common_client_error_failed.
  ///
  /// In en, this message translates to:
  /// **'Temporarily Unclassified General Error'**
  String get common_client_error_failed;

  /// No description provided for @common_client_error_freq_limit.
  ///
  /// In en, this message translates to:
  /// **'Request Rate Limited, Please Try Again Later'**
  String get common_client_error_freq_limit;

  /// No description provided for @common_client_error_get_screen_sharing_target_failed.
  ///
  /// In en, this message translates to:
  /// **'Failed to get screen sharing source (screen and window), check screen recording permissions'**
  String get common_client_error_get_screen_sharing_target_failed;

  /// No description provided for @common_client_error_invalid_parameter.
  ///
  /// In en, this message translates to:
  /// **'Passing illegal parameters when calling API, check if the parameters are legal'**
  String get common_client_error_invalid_parameter;

  /// No description provided for @common_client_error_max_seat_count_limit.
  ///
  /// In en, this message translates to:
  /// **'Maximum Seat Exceeds Package Quantity Limit'**
  String get common_client_error_max_seat_count_limit;

  /// No description provided for @common_client_error_microphone_device_empty.
  ///
  /// In en, this message translates to:
  /// **'No Mic Device Currently'**
  String get common_client_error_microphone_device_empty;

  /// No description provided for @common_client_error_microphone_not_authorized.
  ///
  /// In en, this message translates to:
  /// **'Mic has No System Authorization, Check System Authorization'**
  String get common_client_error_microphone_not_authorized;

  /// No description provided for @common_client_error_microphone_occupied.
  ///
  /// In en, this message translates to:
  /// **'Mic is Occupied'**
  String get common_client_error_microphone_occupied;

  /// No description provided for @common_client_error_microphone_start_fail.
  ///
  /// In en, this message translates to:
  /// **'System Issue, Failed to Open Mic. Check if Mic Device is Normal'**
  String get common_client_error_microphone_start_fail;

  /// No description provided for @common_client_error_open_camera_need_permission_from_admin.
  ///
  /// In en, this message translates to:
  /// **'Need to Apply to Room Owner or Administrator to Open Camera'**
  String get common_client_error_open_camera_need_permission_from_admin;

  /// No description provided for @common_client_error_open_camera_need_seat_unlock.
  ///
  /// In en, this message translates to:
  /// **'Current Seat Video is Locked, Need Room Owner to Unlock Mic Seat Before Opening Camera'**
  String get common_client_error_open_camera_need_seat_unlock;

  /// No description provided for @common_client_error_open_microphone_need_permission_from_admin.
  ///
  /// In en, this message translates to:
  /// **'Need to Apply to Room Owner or Administrator to Open Mic'**
  String get common_client_error_open_microphone_need_permission_from_admin;

  /// No description provided for @common_client_error_open_microphone_need_seat_unlock.
  ///
  /// In en, this message translates to:
  /// **'Current Seat Audio is Locked'**
  String get common_client_error_open_microphone_need_seat_unlock;

  /// No description provided for @common_client_error_open_screen_share_need_permission_from_admin.
  ///
  /// In en, this message translates to:
  /// **'Screen sharing needs to be enabled after applying to the room owner or administrator'**
  String get common_client_error_open_screen_share_need_permission_from_admin;

  /// No description provided for @common_client_error_open_screen_share_need_seat_unlock.
  ///
  /// In en, this message translates to:
  /// **'The current microphone position video is locked and needs to be unlocked by the room owner before screen sharing can be enabled'**
  String get common_client_error_open_screen_share_need_seat_unlock;

  /// No description provided for @common_client_error_operation_invalid_before_enter_room.
  ///
  /// In en, this message translates to:
  /// **'This Feature Can Only Be Used After Entering the Room'**
  String get common_client_error_operation_invalid_before_enter_room;

  /// No description provided for @common_client_error_operation_not_supported_in_current_room_type.
  ///
  /// In en, this message translates to:
  /// **'This Operation is Not Supported in the Current Room Type'**
  String get common_client_error_operation_not_supported_in_current_room_type;

  /// No description provided for @common_client_error_permission_denied.
  ///
  /// In en, this message translates to:
  /// **'Failed to Obtain Permission, Unauthorized Audio/Video Permission, Please Check if Device Permission is Enabled'**
  String get common_client_error_permission_denied;

  /// No description provided for @common_client_error_repeat_operation.
  ///
  /// In en, this message translates to:
  /// **'Repeat Operation'**
  String get common_client_error_repeat_operation;

  /// No description provided for @common_client_error_request_id_invalid.
  ///
  /// In en, this message translates to:
  /// **'Signaling Request ID is Invalid or Has Been Processed'**
  String get common_client_error_request_id_invalid;

  /// No description provided for @common_client_error_request_id_repeat.
  ///
  /// In en, this message translates to:
  /// **'Signal request repetition'**
  String get common_client_error_request_id_repeat;

  /// No description provided for @common_client_error_request_no_permission.
  ///
  /// In en, this message translates to:
  /// **'No Permission for Signaling Request, e.g. Canceling an Invite Not Initiated by Yourself'**
  String get common_client_error_request_no_permission;

  /// No description provided for @common_client_error_require_payment.
  ///
  /// In en, this message translates to:
  /// **'This feature requires an additional package. Please activate the corresponding package as needed in the TRTC Console'**
  String get common_client_error_require_payment;

  /// No description provided for @common_client_error_room_id_invalid.
  ///
  /// In en, this message translates to:
  /// **'Illegal Custom Room ID, Must Be Printable ASCII Characters (0x20-0x7e), Up to 48 Bytes Long'**
  String get common_client_error_room_id_invalid;

  /// No description provided for @common_client_error_room_name_invalid.
  ///
  /// In en, this message translates to:
  /// **'Illegal Room Name, Maximum 30 Bytes, Must Be UTF-8 Encoding if Contains Chinese Characters'**
  String get common_client_error_room_name_invalid;

  /// No description provided for @common_client_error_room_not_support_preloading.
  ///
  /// In en, this message translates to:
  /// **'The current room does not support preloading'**
  String get common_client_error_room_not_support_preloading;

  /// No description provided for @common_client_error_sdk_app_id_not_found.
  ///
  /// In en, this message translates to:
  /// **'Not Found SDKAppID, Please Confirm Application Info in TRTC Console'**
  String get common_client_error_sdk_app_id_not_found;

  /// No description provided for @common_client_error_sdk_not_initialized.
  ///
  /// In en, this message translates to:
  /// **'Not Logged In, Please Call Login API'**
  String get common_client_error_sdk_not_initialized;

  /// No description provided for @common_client_error_seat_index_not_exist.
  ///
  /// In en, this message translates to:
  /// **'Seat Serial Number Does Not Exist'**
  String get common_client_error_seat_index_not_exist;

  /// No description provided for @common_client_error_send_message_disabled_for_all.
  ///
  /// In en, this message translates to:
  /// **'All Members Muted in the Current Room'**
  String get common_client_error_send_message_disabled_for_all;

  /// No description provided for @common_client_error_send_message_disabled_for_current.
  ///
  /// In en, this message translates to:
  /// **'You Have Been Muted in the Current Room'**
  String get common_client_error_send_message_disabled_for_current;

  /// No description provided for @common_client_error_start_screen_sharing_failed.
  ///
  /// In en, this message translates to:
  /// **'Failed to Enable Screen Sharing, Check if Someone is Already Screen Sharing in the Room'**
  String get common_client_error_start_screen_sharing_failed;

  /// No description provided for @common_client_error_success.
  ///
  /// In en, this message translates to:
  /// **'Operation Successful'**
  String get common_client_error_success;

  /// No description provided for @common_client_error_user_need_admin_permission.
  ///
  /// In en, this message translates to:
  /// **'Room Owner or Administrator Permission Required for Operation'**
  String get common_client_error_user_need_admin_permission;

  /// No description provided for @common_client_error_user_need_owner_permission.
  ///
  /// In en, this message translates to:
  /// **'Room Owner Permission Required for Operation'**
  String get common_client_error_user_need_owner_permission;

  /// No description provided for @common_client_error_user_not_exist.
  ///
  /// In en, this message translates to:
  /// **'User is not exist'**
  String get common_client_error_user_not_exist;

  /// No description provided for @common_server_error_gift_ability_not_enabled.
  ///
  /// In en, this message translates to:
  /// **'Gift service is not enabled yet please check your package version'**
  String get common_server_error_gift_ability_not_enabled;

  /// No description provided for @common_server_error_gift_not_exist.
  ///
  /// In en, this message translates to:
  /// **'Gift does not exist'**
  String get common_server_error_gift_not_exist;

  /// No description provided for @common_server_error_gift_server_pre_verification_failed.
  ///
  /// In en, this message translates to:
  /// **'Gift server pre-verification failed please check console configuration'**
  String get common_server_error_gift_server_pre_verification_failed;

  /// No description provided for @common_server_error_call_in_progress.
  ///
  /// In en, this message translates to:
  /// **'The device operation failed while in a call'**
  String get common_server_error_call_in_progress;

  /// No description provided for @common_ear_return_volume.
  ///
  /// In en, this message translates to:
  /// **'Ear Monitor Volume'**
  String get common_ear_return_volume;

  /// No description provided for @common_fan_count.
  ///
  /// In en, this message translates to:
  /// **'Fans'**
  String get common_fan_count;

  /// No description provided for @live_barrage_warning_not_empty.
  ///
  /// In en, this message translates to:
  /// **'input can\'t be empty!'**
  String get live_barrage_warning_not_empty;

  /// No description provided for @live_clarity.
  ///
  /// In en, this message translates to:
  /// **'Clarity'**
  String get live_clarity;

  /// No description provided for @common_seat_management.
  ///
  /// In en, this message translates to:
  /// **'Seat'**
  String get common_seat_management;

  /// No description provided for @live_room_has_been_dismissed.
  ///
  /// In en, this message translates to:
  /// **'Room has been dismissed'**
  String get live_room_has_been_dismissed;

  /// No description provided for @common_anchor_audience_list_panel_title.
  ///
  /// In en, this message translates to:
  /// **'Online audience'**
  String get common_anchor_audience_list_panel_title;

  /// No description provided for @common_app_running.
  ///
  /// In en, this message translates to:
  /// **'Running'**
  String get common_app_running;

  /// No description provided for @common_audio_effect.
  ///
  /// In en, this message translates to:
  /// **'Audio'**
  String get common_audio_effect;

  /// No description provided for @common_audio_effect_settings.
  ///
  /// In en, this message translates to:
  /// **'Audio Effect Settings'**
  String get common_audio_effect_settings;

  /// No description provided for @common_audio_settings.
  ///
  /// In en, this message translates to:
  /// **'Audio settings'**
  String get common_audio_settings;

  /// No description provided for @common_beauty_item_close.
  ///
  /// In en, this message translates to:
  /// **'Close'**
  String get common_beauty_item_close;

  /// No description provided for @common_beauty_item_ruddy.
  ///
  /// In en, this message translates to:
  /// **'Rosy'**
  String get common_beauty_item_ruddy;

  /// No description provided for @common_beauty_item_smooth.
  ///
  /// In en, this message translates to:
  /// **'Microdermabrasion'**
  String get common_beauty_item_smooth;

  /// No description provided for @common_beauty_item_whiteness.
  ///
  /// In en, this message translates to:
  /// **'Whitening'**
  String get common_beauty_item_whiteness;

  /// No description provided for @common_beauty_panel_title.
  ///
  /// In en, this message translates to:
  /// **'One-click beauty'**
  String get common_beauty_panel_title;

  /// No description provided for @common_cancel.
  ///
  /// In en, this message translates to:
  /// **'Cancel'**
  String get common_cancel;

  /// No description provided for @common_change_voice.
  ///
  /// In en, this message translates to:
  /// **'Voice changer'**
  String get common_change_voice;

  /// No description provided for @common_change_voice_child.
  ///
  /// In en, this message translates to:
  /// **'Naughty child'**
  String get common_change_voice_child;

  /// No description provided for @common_change_voice_ethereal.
  ///
  /// In en, this message translates to:
  /// **'Ethereal'**
  String get common_change_voice_ethereal;

  /// No description provided for @common_change_voice_girl.
  ///
  /// In en, this message translates to:
  /// **'Loli'**
  String get common_change_voice_girl;

  /// No description provided for @common_change_voice_none.
  ///
  /// In en, this message translates to:
  /// **'Original'**
  String get common_change_voice_none;

  /// No description provided for @common_change_voice_uncle.
  ///
  /// In en, this message translates to:
  /// **'Uncle'**
  String get common_change_voice_uncle;

  /// No description provided for @common_common_gift_income.
  ///
  /// In en, this message translates to:
  /// **'Gift Income'**
  String get common_common_gift_income;

  /// No description provided for @common_common_like_count.
  ///
  /// In en, this message translates to:
  /// **'Likes Count'**
  String get common_common_like_count;

  /// No description provided for @common_common_live_duration.
  ///
  /// In en, this message translates to:
  /// **'Duration'**
  String get common_common_live_duration;

  /// No description provided for @common_common_live_people_number.
  ///
  /// In en, this message translates to:
  /// **'Total Views'**
  String get common_common_live_people_number;

  /// No description provided for @common_common_message_count.
  ///
  /// In en, this message translates to:
  /// **'Messages'**
  String get common_common_message_count;

  /// No description provided for @common_common_send_gift_people_count.
  ///
  /// In en, this message translates to:
  /// **'Gift givers'**
  String get common_common_send_gift_people_count;

  /// No description provided for @common_common_this_live_data.
  ///
  /// In en, this message translates to:
  /// **'Live data'**
  String get common_common_this_live_data;

  /// No description provided for @common_ear_return.
  ///
  /// In en, this message translates to:
  /// **'Ear Monitor'**
  String get common_ear_return;

  /// No description provided for @common_set_cover.
  ///
  /// In en, this message translates to:
  /// **'Set Cover'**
  String get common_set_cover;

  /// No description provided for @common_end_link.
  ///
  /// In en, this message translates to:
  /// **'End Co-guest'**
  String get common_end_link;

  /// No description provided for @common_end_live.
  ///
  /// In en, this message translates to:
  /// **'End Live'**
  String get common_end_live;

  /// No description provided for @common_end_user.
  ///
  /// In en, this message translates to:
  /// **'End'**
  String get common_end_user;

  /// No description provided for @common_entered_room.
  ///
  /// In en, this message translates to:
  /// **'Entered room'**
  String get common_entered_room;

  /// No description provided for @common_follow_anchor.
  ///
  /// In en, this message translates to:
  /// **'Follow'**
  String get common_follow_anchor;

  /// No description provided for @common_gift_give_gift.
  ///
  /// In en, this message translates to:
  /// **'Send Out'**
  String get common_gift_give_gift;

  /// No description provided for @common_gift_title.
  ///
  /// In en, this message translates to:
  /// **'Gift'**
  String get common_gift_title;

  /// No description provided for @common_hang_up.
  ///
  /// In en, this message translates to:
  /// **'Hang Up'**
  String get common_hang_up;

  /// No description provided for @common_kick_user_confirm_message.
  ///
  /// In en, this message translates to:
  /// **'Are you sure you want to remove xxx?'**
  String get common_kick_user_confirm_message;

  /// No description provided for @common_link_mic_manager.
  ///
  /// In en, this message translates to:
  /// **'Link Management'**
  String get common_link_mic_manager;

  /// No description provided for @common_link.
  ///
  /// In en, this message translates to:
  /// **'Link'**
  String get common_link;

  /// No description provided for @common_live_has_stop.
  ///
  /// In en, this message translates to:
  /// **'Live broadcast has ended'**
  String get common_live_has_stop;

  /// No description provided for @common_more_settings.
  ///
  /// In en, this message translates to:
  /// **'More settings'**
  String get common_more_settings;

  /// No description provided for @common_music.
  ///
  /// In en, this message translates to:
  /// **'Music'**
  String get common_music;

  /// No description provided for @common_music_cheerful.
  ///
  /// In en, this message translates to:
  /// **'Cheerful'**
  String get common_music_cheerful;

  /// No description provided for @common_music_confirm.
  ///
  /// In en, this message translates to:
  /// **'Confirm'**
  String get common_music_confirm;

  /// No description provided for @common_music_melancholy.
  ///
  /// In en, this message translates to:
  /// **'Melancholy'**
  String get common_music_melancholy;

  /// No description provided for @common_music_tips_title.
  ///
  /// In en, this message translates to:
  /// **'Warm Tips'**
  String get common_music_tips_title;

  /// No description provided for @common_music_volume.
  ///
  /// In en, this message translates to:
  /// **'Music volume'**
  String get common_music_volume;

  /// No description provided for @common_music_wonder_world.
  ///
  /// In en, this message translates to:
  /// **'Magical World'**
  String get common_music_wonder_world;

  /// No description provided for @common_people_volume.
  ///
  /// In en, this message translates to:
  /// **'Voice volume'**
  String get common_people_volume;

  /// No description provided for @common_accept.
  ///
  /// In en, this message translates to:
  /// **'Accept'**
  String get common_accept;

  /// No description provided for @common_receive.
  ///
  /// In en, this message translates to:
  /// **'Accept'**
  String get common_receive;

  /// No description provided for @common_reject.
  ///
  /// In en, this message translates to:
  /// **'Reject'**
  String get common_reject;

  /// No description provided for @common_resolution_360p.
  ///
  /// In en, this message translates to:
  /// **'360p'**
  String get common_resolution_360p;

  /// No description provided for @common_resolution_540p.
  ///
  /// In en, this message translates to:
  /// **'540p'**
  String get common_resolution_540p;

  /// No description provided for @common_resolution_720p.
  ///
  /// In en, this message translates to:
  /// **'720p'**
  String get common_resolution_720p;

  /// No description provided for @common_reverb.
  ///
  /// In en, this message translates to:
  /// **'Reverb'**
  String get common_reverb;

  /// No description provided for @common_reverb_karaoke.
  ///
  /// In en, this message translates to:
  /// **'KTV'**
  String get common_reverb_karaoke;

  /// No description provided for @common_reverb_loud_and_loud.
  ///
  /// In en, this message translates to:
  /// **'Loud'**
  String get common_reverb_loud_and_loud;

  /// No description provided for @common_reverb_low.
  ///
  /// In en, this message translates to:
  /// **'Low'**
  String get common_reverb_low;

  /// No description provided for @common_reverb_metallic_sound.
  ///
  /// In en, this message translates to:
  /// **'Metallic sound'**
  String get common_reverb_metallic_sound;

  /// No description provided for @common_reverb_none.
  ///
  /// In en, this message translates to:
  /// **'No effect'**
  String get common_reverb_none;

  /// No description provided for @common_room_destroy.
  ///
  /// In en, this message translates to:
  /// **'Broadcast has been ended'**
  String get common_room_destroy;

  /// No description provided for @common_room_info_liveroom_id.
  ///
  /// In en, this message translates to:
  /// **'Live Room ID:'**
  String get common_room_info_liveroom_id;

  /// No description provided for @common_sent.
  ///
  /// In en, this message translates to:
  /// **'Sent'**
  String get common_sent;

  /// No description provided for @common_go_live.
  ///
  /// In en, this message translates to:
  /// **'Go Live'**
  String get common_go_live;

  /// No description provided for @common_server_error_already_on_the_mic.
  ///
  /// In en, this message translates to:
  /// **'Already on the seat'**
  String get common_server_error_already_on_the_mic;

  /// No description provided for @common_server_error_already_on_the_mic_queue.
  ///
  /// In en, this message translates to:
  /// **'Already on the seat queue'**
  String get common_server_error_already_on_the_mic_queue;

  /// No description provided for @common_server_error_battle_does_not_exist_or_has_ended.
  ///
  /// In en, this message translates to:
  /// **'The battle does not exist or has ended'**
  String get common_server_error_battle_does_not_exist_or_has_ended;

  /// No description provided for @common_server_error_battle_session_has_ended.
  ///
  /// In en, this message translates to:
  /// **'The battle session has ended'**
  String get common_server_error_battle_session_has_ended;

  /// No description provided for @common_server_error_connection_does_not_exist.
  ///
  /// In en, this message translates to:
  /// **'The current connection does not exist or has ended'**
  String get common_server_error_connection_does_not_exist;

  /// No description provided for @common_server_error_creating_battles_too_frequently.
  ///
  /// In en, this message translates to:
  /// **'creating battles too frequently. Wait a moment and try again'**
  String get common_server_error_creating_battles_too_frequently;

  /// No description provided for @common_server_error_creating_connections_too_frequent.
  ///
  /// In en, this message translates to:
  /// **'creating connections too frequent in a short time. Wait a moment and try again'**
  String get common_server_error_creating_connections_too_frequent;

  /// No description provided for @common_server_error_creating_rooms_exceeds_the_frequency_limit.
  ///
  /// In en, this message translates to:
  /// **'Creating rooms exceeds the frequency limit, the same room ID can only be created once within 1 second'**
  String get common_server_error_creating_rooms_exceeds_the_frequency_limit;

  /// No description provided for @common_server_error_exceeds_the_upper_limit.
  ///
  /// In en, this message translates to:
  /// **'Exceeds the upper limit, for example, the number of microphone seats, the number of PK match rooms, etc., exceeds the payment limit'**
  String get common_server_error_exceeds_the_upper_limit;

  /// No description provided for @common_server_error_has_exceeded_the_limit_in_connection_or_battle.
  ///
  /// In en, this message translates to:
  /// **'The room number has exceeded the limit in connection or battle'**
  String get common_server_error_has_exceeded_the_limit_in_connection_or_battle;

  /// No description provided for @common_server_error_in_other_battle.
  ///
  /// In en, this message translates to:
  /// **'The room is already in other battle'**
  String get common_server_error_in_other_battle;

  /// No description provided for @common_server_error_insufficient_operation_permissions.
  ///
  /// In en, this message translates to:
  /// **'You do not have permission to perform this operation'**
  String get common_server_error_insufficient_operation_permissions;

  /// No description provided for @common_server_error_invalid_room_type.
  ///
  /// In en, this message translates to:
  /// **'Invalid room type'**
  String get common_server_error_invalid_room_type;

  /// No description provided for @common_server_error_is_connecting_with_other_rooms.
  ///
  /// In en, this message translates to:
  /// **'The current room is connecting with other rooms'**
  String get common_server_error_is_connecting_with_other_rooms;

  /// No description provided for @common_server_error_is_not_allowed_to_cancel_battle_for_room_in_battle.
  ///
  /// In en, this message translates to:
  /// **'It\'s not allowed to cancel battle for room in battle'**
  String
      get common_server_error_is_not_allowed_to_cancel_battle_for_room_in_battle;

  /// No description provided for @common_server_error_metadata_no_valid_keys.
  ///
  /// In en, this message translates to:
  /// **'There is no valid keys when delete metadata'**
  String get common_server_error_metadata_no_valid_keys;

  /// No description provided for @common_server_error_metadata_number_of_keys_exceeds_the_limit.
  ///
  /// In en, this message translates to:
  /// **'The number of keys in the room\'s Metadata exceeds the limit'**
  String get common_server_error_metadata_number_of_keys_exceeds_the_limit;

  /// No description provided for @common_server_error_metadata_size_of_value_exceeds_the_limit.
  ///
  /// In en, this message translates to:
  /// **'The size of value in the room\'s Metadata exceeds the maximum byte limit'**
  String get common_server_error_metadata_size_of_value_exceeds_the_limit;

  /// No description provided for @common_server_error_metadata_the_size_of_key_exceeds_the_maximum_byte_limit.
  ///
  /// In en, this message translates to:
  /// **'The size of key in the room\'s Metadata exceeds the maximum byte limit'**
  String
      get common_server_error_metadata_the_size_of_key_exceeds_the_maximum_byte_limit;

  /// No description provided for @common_server_error_metadata_total_size_exceeds_the_limit.
  ///
  /// In en, this message translates to:
  /// **'The total size of all value in the room\'s Metadata exceeds the maximum byte limit'**
  String get common_server_error_metadata_total_size_exceeds_the_limit;

  /// No description provided for @common_server_error_mic_seat_is_locked.
  ///
  /// In en, this message translates to:
  /// **'The seat is locked. You can try another seat'**
  String get common_server_error_mic_seat_is_locked;

  /// No description provided for @common_server_error_no_payment_information.
  ///
  /// In en, this message translates to:
  /// **'No payment information, you need to purchase a package in the console'**
  String get common_server_error_no_payment_information;

  /// No description provided for @common_server_error_no_rooms_in_the_battle_is_valid.
  ///
  /// In en, this message translates to:
  /// **'None of the rooms in the battle is valid'**
  String get common_server_error_no_rooms_in_the_battle_is_valid;

  /// No description provided for @common_server_error_not_a_room_member.
  ///
  /// In en, this message translates to:
  /// **'Not a room member'**
  String get common_server_error_not_a_room_member;

  /// No description provided for @common_server_error_not_on_the_mic_queue.
  ///
  /// In en, this message translates to:
  /// **'Not on the seat queue'**
  String get common_server_error_not_on_the_mic_queue;

  /// No description provided for @common_server_error_not_on_the_mic_seat.
  ///
  /// In en, this message translates to:
  /// **'Not on the seat'**
  String get common_server_error_not_on_the_mic_seat;

  /// No description provided for @common_server_error_not_started_yet.
  ///
  /// In en, this message translates to:
  /// **'The battle has not started yet'**
  String get common_server_error_not_started_yet;

  /// No description provided for @common_server_error_param_illegal.
  ///
  /// In en, this message translates to:
  /// **'The parameter is illegal. Check whether the request is correct according to the error description'**
  String get common_server_error_param_illegal;

  /// No description provided for @common_server_error_requires_password.
  ///
  /// In en, this message translates to:
  /// **'The current room requires a password for entry'**
  String get common_server_error_requires_password;

  /// No description provided for @common_server_error_room_admin_quantity_exceeds_the_upper_limit.
  ///
  /// In en, this message translates to:
  /// **'The admin quantity exceeds the upper limit'**
  String get common_server_error_room_admin_quantity_exceeds_the_upper_limit;

  /// No description provided for @common_server_error_room_does_not_exist.
  ///
  /// In en, this message translates to:
  /// **'The room does not exist, or it once existed but has now been dissolved'**
  String get common_server_error_room_does_not_exist;

  /// No description provided for @common_server_error_room_does_not_support_mic_ability.
  ///
  /// In en, this message translates to:
  /// **'The room does not support seat ability'**
  String get common_server_error_room_does_not_support_mic_ability;

  /// No description provided for @common_server_error_room_entry_password_error.
  ///
  /// In en, this message translates to:
  /// **'Room Entry Password Error'**
  String get common_server_error_room_entry_password_error;

  /// No description provided for @common_server_error_room_id_exists.
  ///
  /// In en, this message translates to:
  /// **'The room ID already exists. Please select another room ID'**
  String get common_server_error_room_id_exists;

  /// No description provided for @common_server_error_room_id_has_been_occupied_by_chat.
  ///
  /// In en, this message translates to:
  /// **'The room ID has been occupied by Chat. You can use a different room ID or dissolve the group first'**
  String get common_server_error_room_id_has_been_occupied_by_chat;

  /// No description provided for @common_server_error_room_id_has_been_used.
  ///
  /// In en, this message translates to:
  /// **'The room ID has been used, and the operator is the room owner, it can be used directly'**
  String get common_server_error_room_id_has_been_used;

  /// No description provided for @common_server_error_room_is_full.
  ///
  /// In en, this message translates to:
  /// **'The room is full'**
  String get common_server_error_room_is_full;

  /// No description provided for @common_server_error_room_is_in_connection.
  ///
  /// In en, this message translates to:
  /// **'The room is already in connection'**
  String get common_server_error_room_is_in_connection;

  /// No description provided for @common_server_error_seat_is_already_occupied.
  ///
  /// In en, this message translates to:
  /// **'The current seat is already occupied'**
  String get common_server_error_seat_is_already_occupied;

  /// No description provided for @common_server_error_signal_request_conflict.
  ///
  /// In en, this message translates to:
  /// **'Signal request conflict'**
  String get common_server_error_signal_request_conflict;

  /// No description provided for @common_server_error_system_internal_error.
  ///
  /// In en, this message translates to:
  /// **'Server internal error, please retry'**
  String get common_server_error_system_internal_error;

  /// No description provided for @common_server_error_tag_quantity_exceeds_upper_limit.
  ///
  /// In en, this message translates to:
  /// **'Tag quantity Exceeds Upper limit'**
  String get common_server_error_tag_quantity_exceeds_upper_limit;

  /// No description provided for @common_server_error_the_room_is_not_in_the_battle.
  ///
  /// In en, this message translates to:
  /// **'The room isn‘t in the battle'**
  String get common_server_error_the_room_is_not_in_the_battle;

  /// No description provided for @common_server_error_the_seat_list_is_empty.
  ///
  /// In en, this message translates to:
  /// **'The seat list is empty'**
  String get common_server_error_the_seat_list_is_empty;

  /// No description provided for @common_server_error_the_seats_are_all_taken.
  ///
  /// In en, this message translates to:
  /// **'The seats are all taken.'**
  String get common_server_error_the_seats_are_all_taken;

  /// No description provided for @common_server_error_there_is_a_pending_battle_request.
  ///
  /// In en, this message translates to:
  /// **'There is a pending battle request for this room'**
  String get common_server_error_there_is_a_pending_battle_request;

  /// No description provided for @common_server_error_there_is_a_pending_connection_request.
  ///
  /// In en, this message translates to:
  /// **'There is a pending connection request for this room'**
  String get common_server_error_there_is_a_pending_connection_request;

  /// No description provided for @common_server_error_this_member_has_been_banned.
  ///
  /// In en, this message translates to:
  /// **'This member has been banned'**
  String get common_server_error_this_member_has_been_banned;

  /// No description provided for @common_server_error_this_member_has_been_muted.
  ///
  /// In en, this message translates to:
  /// **'This member has been muted'**
  String get common_server_error_this_member_has_been_muted;

  /// No description provided for @common_server_error_user_is_already_on_the_mic_seat.
  ///
  /// In en, this message translates to:
  /// **'The user is already on the seat'**
  String get common_server_error_user_is_already_on_the_mic_seat;

  /// No description provided for @common_set_as_background.
  ///
  /// In en, this message translates to:
  /// **'Set as background'**
  String get common_set_as_background;

  /// No description provided for @common_set_as_cover.
  ///
  /// In en, this message translates to:
  /// **'Set as cover'**
  String get common_set_as_cover;

  /// No description provided for @common_settings.
  ///
  /// In en, this message translates to:
  /// **'Settings'**
  String get common_settings;

  /// No description provided for @common_settings_bg_image.
  ///
  /// In en, this message translates to:
  /// **'Background'**
  String get common_settings_bg_image;

  /// No description provided for @common_stream_categories_beauty.
  ///
  /// In en, this message translates to:
  /// **'Beauty'**
  String get common_stream_categories_beauty;

  /// No description provided for @common_stream_categories_default.
  ///
  /// In en, this message translates to:
  /// **'Daily chat'**
  String get common_stream_categories_default;

  /// No description provided for @common_stream_categories_shopping.
  ///
  /// In en, this message translates to:
  /// **'Shopping'**
  String get common_stream_categories_shopping;

  /// No description provided for @common_stream_categories_teach.
  ///
  /// In en, this message translates to:
  /// **'Knowledge Teaching'**
  String get common_stream_categories_teach;

  /// No description provided for @common_stream_privacy_status_default.
  ///
  /// In en, this message translates to:
  /// **'Public'**
  String get common_stream_privacy_status_default;

  /// No description provided for @common_stream_privacy_status_privacy.
  ///
  /// In en, this message translates to:
  /// **'Privacy'**
  String get common_stream_privacy_status_privacy;

  /// No description provided for @common_text_cancel_link_mic_apply.
  ///
  /// In en, this message translates to:
  /// **'Cancel application for link mic'**
  String get common_text_cancel_link_mic_apply;

  /// No description provided for @common_text_close_link_mic.
  ///
  /// In en, this message translates to:
  /// **'End Link'**
  String get common_text_close_link_mic;

  /// No description provided for @common_text_link_mic_audio.
  ///
  /// In en, this message translates to:
  /// **'Apply for audio link'**
  String get common_text_link_mic_audio;

  /// No description provided for @common_text_link_mic_selector.
  ///
  /// In en, this message translates to:
  /// **'connect upon host\'s approval'**
  String get common_text_link_mic_selector;

  /// No description provided for @common_text_link_mic_video.
  ///
  /// In en, this message translates to:
  /// **'Apply for video link'**
  String get common_text_link_mic_video;

  /// No description provided for @common_tips_apply_link_mic.
  ///
  /// In en, this message translates to:
  /// **'The screen effect will automatically take effect after connecting'**
  String get common_tips_apply_link_mic;

  /// No description provided for @common_title_link_mic_selector.
  ///
  /// In en, this message translates to:
  /// **'Choose Link Mode'**
  String get common_title_link_mic_selector;

  /// No description provided for @common_title_link_video_settings.
  ///
  /// In en, this message translates to:
  /// **'Adjust the video link screen'**
  String get common_title_link_video_settings;

  /// No description provided for @common_toast_apply_link_mic.
  ///
  /// In en, this message translates to:
  /// **'You have submitted a link mic request, please wait for the author approval'**
  String get common_toast_apply_link_mic;

  /// No description provided for @common_unfollow_anchor.
  ///
  /// In en, this message translates to:
  /// **'Unfollow'**
  String get common_unfollow_anchor;

  /// No description provided for @common_video_config.
  ///
  /// In en, this message translates to:
  /// **'Video'**
  String get common_video_config;

  /// No description provided for @common_video_params.
  ///
  /// In en, this message translates to:
  /// **'Video Config'**
  String get common_video_params;

  /// No description provided for @common_video_settings_item_beauty.
  ///
  /// In en, this message translates to:
  /// **'Beauty'**
  String get common_video_settings_item_beauty;

  /// No description provided for @common_video_settings_item_flip.
  ///
  /// In en, this message translates to:
  /// **'Flip'**
  String get common_video_settings_item_flip;

  /// No description provided for @common_video_settings_item_mirror.
  ///
  /// In en, this message translates to:
  /// **'Mirror'**
  String get common_video_settings_item_mirror;

  /// No description provided for @common_voiceroom_empty_view.
  ///
  /// In en, this message translates to:
  /// **'No users in the seat, go to invite'**
  String get common_voiceroom_empty_view;

  /// No description provided for @common_voiceroom_invite.
  ///
  /// In en, this message translates to:
  /// **'Invite'**
  String get common_voiceroom_invite;

  /// No description provided for @common_voiceroom_invite_seat_canceled.
  ///
  /// In en, this message translates to:
  /// **'Seat invitation has been canceled'**
  String get common_voiceroom_invite_seat_canceled;

  /// No description provided for @common_voiceroom_lock.
  ///
  /// In en, this message translates to:
  /// **'Lock Seat'**
  String get common_voiceroom_lock;

  /// No description provided for @common_voiceroom_mute_seat.
  ///
  /// In en, this message translates to:
  /// **'Mute'**
  String get common_voiceroom_mute_seat;

  /// No description provided for @common_voiceroom_need_agree.
  ///
  /// In en, this message translates to:
  /// **'Require owner\'s consent to speak'**
  String get common_voiceroom_need_agree;

  /// No description provided for @common_voiceroom_receive_seat_invitation.
  ///
  /// In en, this message translates to:
  /// **'xxx invites you to take seat'**
  String get common_voiceroom_receive_seat_invitation;

  /// No description provided for @common_voiceroom_take_seat.
  ///
  /// In en, this message translates to:
  /// **'Take Seat'**
  String get common_voiceroom_take_seat;

  /// No description provided for @common_voiceroom_take_seat_rejected.
  ///
  /// In en, this message translates to:
  /// **'Take seat application has been rejected'**
  String get common_voiceroom_take_seat_rejected;

  /// No description provided for @common_voiceroom_take_seat_timeout.
  ///
  /// In en, this message translates to:
  /// **'Take seat application timeout'**
  String get common_voiceroom_take_seat_timeout;

  /// No description provided for @common_voiceroom_unlock.
  ///
  /// In en, this message translates to:
  /// **'Unlock Seat'**
  String get common_voiceroom_unlock;

  /// No description provided for @common_voiceroom_unmuted_seat.
  ///
  /// In en, this message translates to:
  /// **'Unmute'**
  String get common_voiceroom_unmuted_seat;

  /// No description provided for @common_waiting_pass.
  ///
  /// In en, this message translates to:
  /// **'Waiting'**
  String get common_waiting_pass;

  /// No description provided for @live_barrage_agree.
  ///
  /// In en, this message translates to:
  /// **'Agree'**
  String get live_barrage_agree;

  /// No description provided for @live_barrage_btn_send.
  ///
  /// In en, this message translates to:
  /// **'Send'**
  String get live_barrage_btn_send;

  /// No description provided for @live_choose_music.
  ///
  /// In en, this message translates to:
  /// **'Choose Music'**
  String get live_choose_music;

  /// No description provided for @live_failed_to_enter_room.
  ///
  /// In en, this message translates to:
  /// **'Failed to enter room'**
  String get live_failed_to_enter_room;

  /// No description provided for @live_invalid_userId.
  ///
  /// In en, this message translates to:
  /// **'Invalid userId'**
  String get live_invalid_userId;

  /// No description provided for @live_music_pitch.
  ///
  /// In en, this message translates to:
  /// **'Music Pitch'**
  String get live_music_pitch;

  /// No description provided for @common_video_status.
  ///
  /// In en, this message translates to:
  /// **'Video Status'**
  String get common_video_status;

  /// No description provided for @common_audio_status.
  ///
  /// In en, this message translates to:
  /// **'Audio Status'**
  String get common_audio_status;

  /// No description provided for @common_device_temp.
  ///
  /// In en, this message translates to:
  /// **'Device Temperature'**
  String get common_device_temp;

  /// No description provided for @common_wifi_or_mobile_network.
  ///
  /// In en, this message translates to:
  /// **'Wi-Fi/Mobile Network'**
  String get common_wifi_or_mobile_network;

  /// No description provided for @common_video_stream_smooth.
  ///
  /// In en, this message translates to:
  /// **'Smooth streaming'**
  String get common_video_stream_smooth;

  /// No description provided for @common_video_stream_freezing.
  ///
  /// In en, this message translates to:
  /// **'Freezing streaming'**
  String get common_video_stream_freezing;

  /// No description provided for @common_video_capture_closed.
  ///
  /// In en, this message translates to:
  /// **'Video capture disabled'**
  String get common_video_capture_closed;

  /// No description provided for @common_audio_tips_proper_volume.
  ///
  /// In en, this message translates to:
  /// **'Proper volume ensures good viewing experience'**
  String get common_audio_tips_proper_volume;

  /// No description provided for @common_audio_tips_regular_checks.
  ///
  /// In en, this message translates to:
  /// **'Regular checks ensure good viewing experience'**
  String get common_audio_tips_regular_checks;

  /// No description provided for @common_network_switch_tips.
  ///
  /// In en, this message translates to:
  /// **'Avoid frequent network switching'**
  String get common_network_switch_tips;

  /// No description provided for @common_live_info.
  ///
  /// In en, this message translates to:
  /// **'Live Information'**
  String get common_live_info;

  /// No description provided for @common_rtt.
  ///
  /// In en, this message translates to:
  /// **'RTT'**
  String get common_rtt;

  /// No description provided for @common_down_loss.
  ///
  /// In en, this message translates to:
  /// **'Down Loss'**
  String get common_down_loss;

  /// No description provided for @common_up_loss.
  ///
  /// In en, this message translates to:
  /// **'Up Loss'**
  String get common_up_loss;

  /// No description provided for @common_normal.
  ///
  /// In en, this message translates to:
  /// **'normal'**
  String get common_normal;

  /// No description provided for @common_close.
  ///
  /// In en, this message translates to:
  /// **'close'**
  String get common_close;

  /// No description provided for @common_exception.
  ///
  /// In en, this message translates to:
  /// **'exception'**
  String get common_exception;

  /// No description provided for @common_excellent.
  ///
  /// In en, this message translates to:
  /// **'excellent'**
  String get common_excellent;

  /// No description provided for @common_good.
  ///
  /// In en, this message translates to:
  /// **'good'**
  String get common_good;

  /// No description provided for @common_poor.
  ///
  /// In en, this message translates to:
  /// **'poor'**
  String get common_poor;

  /// No description provided for @common_bad.
  ///
  /// In en, this message translates to:
  /// **'bad'**
  String get common_bad;

  /// No description provided for @common_verybad.
  ///
  /// In en, this message translates to:
  /// **'veryBad'**
  String get common_verybad;

  /// No description provided for @common_down.
  ///
  /// In en, this message translates to:
  /// **'down'**
  String get common_down;

  /// No description provided for @common_audio_mode_default.
  ///
  /// In en, this message translates to:
  /// **'Default'**
  String get common_audio_mode_default;

  /// No description provided for @common_audio_mode_speech.
  ///
  /// In en, this message translates to:
  /// **'Speech'**
  String get common_audio_mode_speech;

  /// No description provided for @common_audio_mode_music.
  ///
  /// In en, this message translates to:
  /// **'Music'**
  String get common_audio_mode_music;

  /// No description provided for @common_device_temp_fair.
  ///
  /// In en, this message translates to:
  /// **'fair'**
  String get common_device_temp_fair;

  /// No description provided for @common_device_temp_serious.
  ///
  /// In en, this message translates to:
  /// **'serious'**
  String get common_device_temp_serious;

  /// No description provided for @common_network_bad_tips.
  ///
  /// In en, this message translates to:
  /// **'Network lag detected suggestions'**
  String get common_network_bad_tips;

  /// No description provided for @common_switch_network.
  ///
  /// In en, this message translates to:
  /// **'Switch Network'**
  String get common_switch_network;

  /// No description provided for @common_wait_connection.
  ///
  /// In en, this message translates to:
  /// **'Conn Wait'**
  String get common_wait_connection;

  /// No description provided for @common_apply_connection.
  ///
  /// In en, this message translates to:
  /// **'Conn Join'**
  String get common_apply_connection;

  /// No description provided for @common_template_select.
  ///
  /// In en, this message translates to:
  /// **'Template:'**
  String get common_template_select;

  /// No description provided for @common_template_dynamic_grid.
  ///
  /// In en, this message translates to:
  /// **'Dynamic grid layout'**
  String get common_template_dynamic_grid;

  /// No description provided for @common_template_dynamic_float.
  ///
  /// In en, this message translates to:
  /// **'Dynamic float layout'**
  String get common_template_dynamic_float;

  /// No description provided for @common_template_static_grid.
  ///
  /// In en, this message translates to:
  /// **'Static grid layout'**
  String get common_template_static_grid;

  /// No description provided for @common_template_static_float.
  ///
  /// In en, this message translates to:
  /// **'Static float layout'**
  String get common_template_static_float;

  /// No description provided for @common_template_layout_settings.
  ///
  /// In en, this message translates to:
  /// **'Layout settings'**
  String get common_template_layout_settings;

  /// No description provided for @common_template_layout_co_guest.
  ///
  /// In en, this message translates to:
  /// **'Layout co-guest'**
  String get common_template_layout_co_guest;

  /// No description provided for @common_template_layout_co_host.
  ///
  /// In en, this message translates to:
  /// **'Layout co-host'**
  String get common_template_layout_co_host;

  /// No description provided for @common_template_layout.
  ///
  /// In en, this message translates to:
  /// **'Layout'**
  String get common_template_layout;

  /// No description provided for @common_video_settings.
  ///
  /// In en, this message translates to:
  /// **'Video settings'**
  String get common_video_settings;

  /// No description provided for @live_video_resolution.
  ///
  /// In en, this message translates to:
  /// **'Resolution'**
  String get live_video_resolution;

  /// No description provided for @live_video_reduce_resolution.
  ///
  /// In en, this message translates to:
  /// **'Reduce resolution'**
  String get live_video_reduce_resolution;

  /// No description provided for @live_video_resolution_changed.
  ///
  /// In en, this message translates to:
  /// **'resolution changed to'**
  String get live_video_resolution_changed;
}

class _LiveKitLocalizationsDelegate
    extends LocalizationsDelegate<LiveKitLocalizations> {
  const _LiveKitLocalizationsDelegate();

  @override
  Future<LiveKitLocalizations> load(Locale locale) {
    return SynchronousFuture<LiveKitLocalizations>(
        lookupLiveKitLocalizations(locale));
  }

  @override
  bool isSupported(Locale locale) =>
      <String>['en', 'zh'].contains(locale.languageCode);

  @override
  bool shouldReload(_LiveKitLocalizationsDelegate old) => false;
}

LiveKitLocalizations lookupLiveKitLocalizations(Locale locale) {
  // Lookup logic when language+script codes are specified.
  switch (locale.languageCode) {
    case 'zh':
      {
        switch (locale.scriptCode) {
          case 'Hant':
            return LiveKitLocalizationsZhHant();
        }
        break;
      }
  }

  // Lookup logic when only language code is specified.
  switch (locale.languageCode) {
    case 'en':
      return LiveKitLocalizationsEn();
    case 'zh':
      return LiveKitLocalizationsZh();
  }

  throw FlutterError(
      'LiveKitLocalizations.delegate failed to load unsupported locale "$locale". This is likely '
      'an issue with the localizations generation tool. Please file an issue '
      'on GitHub with a reproducible sample app and the gen-l10n configuration '
      'that was used.');
}
