import 'dart:async';

import 'package:flutter/foundation.dart';
import 'package:flutter/widgets.dart';
import 'package:flutter_localizations/flutter_localizations.dart';
import 'package:intl/intl.dart' as intl;

import 'livekit_localizations_en.dart';
import 'livekit_localizations_zh.dart';

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
    Locale('zh')
  ];

  /// No description provided for @livekit_error_success.
  ///
  /// In en, this message translates to:
  /// **'Operation successful'**
  String get livekit_error_success;

  /// No description provided for @livekit_error_failed.
  ///
  /// In en, this message translates to:
  /// **'Uncategorized common error'**
  String get livekit_error_failed;

  /// No description provided for @livekit_error_freqLimit.
  ///
  /// In en, this message translates to:
  /// **'Request frequency limited, please try again later'**
  String get livekit_error_freqLimit;

  /// No description provided for @livekit_error_repeat_operation.
  ///
  /// In en, this message translates to:
  /// **'Duplicate operation'**
  String get livekit_error_repeat_operation;

  /// No description provided for @livekit_error_sdkAppId_notFound.
  ///
  /// In en, this message translates to:
  /// **'SDKAppID not found, please confirm the application information in the Tencent Cloud Video Cube SDK Console'**
  String get livekit_error_sdkAppId_notFound;

  /// No description provided for @livekit_error_invalidParameter.
  ///
  /// In en, this message translates to:
  /// **'Illegal parameters were passed when calling the API, check if the input parameters are valid'**
  String get livekit_error_invalidParameter;

  /// No description provided for @livekit_error_sdkNotInitialized.
  ///
  /// In en, this message translates to:
  /// **'Not logged in, please call the Login interface'**
  String get livekit_error_sdkNotInitialized;

  /// No description provided for @livekit_error_permissionDenied.
  ///
  /// In en, this message translates to:
  /// **'Permission acquisition failed, currently unauthorized for audio/video permissions, please check if device permissions are enabled'**
  String get livekit_error_permissionDenied;

  /// No description provided for @livekit_error_requirePayment.
  ///
  /// In en, this message translates to:
  /// **'Check if the cloud service account is normal'**
  String get livekit_error_requirePayment;

  /// No description provided for @livekit_error_cameraStartFail.
  ///
  /// In en, this message translates to:
  /// **'System issue, failed to open the camera_ Check if the camera device is working properly'**
  String get livekit_error_cameraStartFail;

  /// No description provided for @livekit_error_cameraNotAuthorized.
  ///
  /// In en, this message translates to:
  /// **'Camera not authorized by the system, check system authorization'**
  String get livekit_error_cameraNotAuthorized;

  /// No description provided for @livekit_error_cameraOccupied.
  ///
  /// In en, this message translates to:
  /// **'Camera is occupied, check if other processes are using the camera'**
  String get livekit_error_cameraOccupied;

  /// No description provided for @livekit_error_cameraDeviceEmpty.
  ///
  /// In en, this message translates to:
  /// **'No camera device available, please insert a camera device to resolve this issue'**
  String get livekit_error_cameraDeviceEmpty;

  /// No description provided for @livekit_error_microphoneStartFail.
  ///
  /// In en, this message translates to:
  /// **'System issue, failed to open the microphone_ Check if the microphone device is working properly'**
  String get livekit_error_microphoneStartFail;

  /// No description provided for @livekit_error_microphoneNotAuthorized.
  ///
  /// In en, this message translates to:
  /// **'Microphone not authorized by the system, check system authorization'**
  String get livekit_error_microphoneNotAuthorized;

  /// No description provided for @livekit_error_microphoneOccupied.
  ///
  /// In en, this message translates to:
  /// **'Microphone is occupied'**
  String get livekit_error_microphoneOccupied;

  /// No description provided for @livekit_error_microphoneDeviceEmpty.
  ///
  /// In en, this message translates to:
  /// **'No microphone device available'**
  String get livekit_error_microphoneDeviceEmpty;

  /// No description provided for @livekit_error_getScreenSharingTargetFailed.
  ///
  /// In en, this message translates to:
  /// **'Failed to get screen sharing source (screen and window), check screen recording permissions'**
  String get livekit_error_getScreenSharingTargetFailed;

  /// No description provided for @livekit_error_startScreenSharingFailed.
  ///
  /// In en, this message translates to:
  /// **'Failed to start screen sharing, check if someone else is screen sharing in the room'**
  String get livekit_error_startScreenSharingFailed;

  /// No description provided for @livekit_error_roomId_notExist.
  ///
  /// In en, this message translates to:
  /// **'The room does not exist when entering, it may have been disbanded'**
  String get livekit_error_roomId_notExist;

  /// No description provided for @livekit_error_operation_invalid_beforeEnterRoom.
  ///
  /// In en, this message translates to:
  /// **'This feature can only be used after entering the room'**
  String get livekit_error_operation_invalid_beforeEnterRoom;

  /// No description provided for @livekit_error_exitNotSupported_forRoomOwner.
  ///
  /// In en, this message translates to:
  /// **'Please transfer the room owner before leaving the room'**
  String get livekit_error_exitNotSupported_forRoomOwner;

  /// No description provided for @livekit_error_operation_notSupported_inCurrentRoomType.
  ///
  /// In en, this message translates to:
  /// **'This operation is not supported in the current room type'**
  String get livekit_error_operation_notSupported_inCurrentRoomType;

  /// No description provided for @livekit_error_operation_notSupported_inCurrentSpeechMode.
  ///
  /// In en, this message translates to:
  /// **'This operation is not supported in the current speaking mode'**
  String get livekit_error_operation_notSupported_inCurrentSpeechMode;

  /// No description provided for @livekit_error_roomId_invalid.
  ///
  /// In en, this message translates to:
  /// **'Invalid room ID created, custom ID must be printable ASCII characters (0x20-0x7e), up to 48 bytes'**
  String get livekit_error_roomId_invalid;

  /// No description provided for @livekit_error_roomId_occupied.
  ///
  /// In en, this message translates to:
  /// **'Room ID already in use, please choose another room ID'**
  String get livekit_error_roomId_occupied;

  /// No description provided for @livekit_error_roomName_invalid.
  ///
  /// In en, this message translates to:
  /// **'Invalid room name, the name can be up to 30 bytes, and the character encoding must be UTF-8 if it contains Chinese'**
  String get livekit_error_roomName_invalid;

  /// No description provided for @livekit_error_already_in_OtherRoom.
  ///
  /// In en, this message translates to:
  /// **'The current user is already in another room, you need to leave the room before joining a new one'**
  String get livekit_error_already_in_OtherRoom;

  /// No description provided for @livekit_error_userNotExist.
  ///
  /// In en, this message translates to:
  /// **'User is not exist'**
  String get livekit_error_userNotExist;

  /// No description provided for @livekit_error_userNotEntered.
  ///
  /// In en, this message translates to:
  /// **'User is not in the current room'**
  String get livekit_error_userNotEntered;

  /// No description provided for @livekit_error_user_need_OwnerPermission.
  ///
  /// In en, this message translates to:
  /// **'Requires room owner permission to operate'**
  String get livekit_error_user_need_OwnerPermission;

  /// No description provided for @livekit_error_user_need_AdminPermission.
  ///
  /// In en, this message translates to:
  /// **'Requires room owner or administrator permission to operate'**
  String get livekit_error_user_need_AdminPermission;

  /// No description provided for @livekit_error_request_noPermission.
  ///
  /// In en, this message translates to:
  /// **'No permission for signaling request, such as canceling an invitation that was not initiated by oneself'**
  String get livekit_error_request_noPermission;

  /// No description provided for @livekit_error_requestId_invalid.
  ///
  /// In en, this message translates to:
  /// **'Invalid signaling request ID or has already been processed'**
  String get livekit_error_requestId_invalid;

  /// No description provided for @livekit_error_repeat_requestId.
  ///
  /// In en, this message translates to:
  /// **'Duplicate signaling request'**
  String get livekit_error_repeat_requestId;

  /// No description provided for @livekit_error_conflict_requestId.
  ///
  /// In en, this message translates to:
  /// **'Conflicting signaling request'**
  String get livekit_error_conflict_requestId;

  /// No description provided for @livekit_error_max_seat_count_limit.
  ///
  /// In en, this message translates to:
  /// **'Maximum number of seats exceeds the package limit'**
  String get livekit_error_max_seat_count_limit;

  /// No description provided for @livekit_error_already_in_seat.
  ///
  /// In en, this message translates to:
  /// **'The current user is already on the seat'**
  String get livekit_error_already_in_seat;

  /// No description provided for @livekit_error_seat_occupied.
  ///
  /// In en, this message translates to:
  /// **'The current seat is already occupied'**
  String get livekit_error_seat_occupied;

  /// No description provided for @livekit_error_seat_locked.
  ///
  /// In en, this message translates to:
  /// **'The current seat is locked'**
  String get livekit_error_seat_locked;

  /// No description provided for @livekit_error_seat_index_not_exist.
  ///
  /// In en, this message translates to:
  /// **'Seat number does not exist'**
  String get livekit_error_seat_index_not_exist;

  /// No description provided for @livekit_error_user_not_in_seat.
  ///
  /// In en, this message translates to:
  /// **'The current user is not on the seat'**
  String get livekit_error_user_not_in_seat;

  /// No description provided for @livekit_error_all_seat_occupied.
  ///
  /// In en, this message translates to:
  /// **'All seats are full'**
  String get livekit_error_all_seat_occupied;

  /// No description provided for @livekit_error_seat_not_support_link_mic.
  ///
  /// In en, this message translates to:
  /// **'Does not support linking microphones'**
  String get livekit_error_seat_not_support_link_mic;

  /// No description provided for @livekit_error_open_microphone_need_seat_unlock.
  ///
  /// In en, this message translates to:
  /// **'The current seat audio is locked, you need to unlock it before opening the microphone'**
  String get livekit_error_open_microphone_need_seat_unlock;

  /// No description provided for @livekit_error_open_microphone_need_permission_from_admin.
  ///
  /// In en, this message translates to:
  /// **'You need to apply to the room owner or administrator before opening the microphone'**
  String get livekit_error_open_microphone_need_permission_from_admin;

  /// No description provided for @livekit_error_open_camera_need_seat_unlock.
  ///
  /// In en, this message translates to:
  /// **'The current seat video is locked, you need to unlock it before opening the camera'**
  String get livekit_error_open_camera_need_seat_unlock;

  /// No description provided for @livekit_error_open_camera_need_permission_from_admin.
  ///
  /// In en, this message translates to:
  /// **'You need to apply to the room owner or administrator before opening the camera'**
  String get livekit_error_open_camera_need_permission_from_admin;

  /// No description provided for @livekit_error_open_screen_share_need_seat_unlock.
  ///
  /// In en, this message translates to:
  /// **'The current seat screen sharing is locked, you need to unlock it before opening the screen sharing'**
  String get livekit_error_open_screen_share_need_seat_unlock;

  /// No description provided for @livekit_error_open_screen_share_need_permission_from_admin.
  ///
  /// In en, this message translates to:
  /// **'You need to apply to the room owner or administrator before opening the screen sharing'**
  String get livekit_error_open_screen_share_need_permission_from_admin;

  /// No description provided for @livekit_error_send_message_disabled_for_all.
  ///
  /// In en, this message translates to:
  /// **'All users are muted in the current room'**
  String get livekit_error_send_message_disabled_for_all;

  /// No description provided for @livekit_error_send_message_disabled_for_current.
  ///
  /// In en, this message translates to:
  /// **'You have been muted in the current room'**
  String get livekit_error_send_message_disabled_for_current;

  /// No description provided for @livekit_take_seat_rejected.
  ///
  /// In en, this message translates to:
  /// **'Take seat application has been rejected'**
  String get livekit_take_seat_rejected;

  /// No description provided for @livekit_room_destroy.
  ///
  /// In en, this message translates to:
  /// **'Broadcast has been ended'**
  String get livekit_room_destroy;

  /// No description provided for @livekit_kicked_out_of_seat.
  ///
  /// In en, this message translates to:
  /// **'Kicked out of seat by room owner'**
  String get livekit_kicked_out_of_seat;

  /// No description provided for @livekit_take_seat_timeout.
  ///
  /// In en, this message translates to:
  /// **'Take seat application timeout'**
  String get livekit_take_seat_timeout;

  /// No description provided for @livekit_anchor.
  ///
  /// In en, this message translates to:
  /// **'Anchor'**
  String get livekit_anchor;

  /// No description provided for @livekit_audience.
  ///
  /// In en, this message translates to:
  /// **'Audience'**
  String get livekit_audience;

  /// No description provided for @livekit_edit_cover.
  ///
  /// In en, this message translates to:
  /// **'Edit'**
  String get livekit_edit_cover;

  /// No description provided for @livekit_stream_categories.
  ///
  /// In en, this message translates to:
  /// **'Live type:'**
  String get livekit_stream_categories;

  /// No description provided for @livekit_stream_privacy_status.
  ///
  /// In en, this message translates to:
  /// **'Live mode:'**
  String get livekit_stream_privacy_status;

  /// No description provided for @livekit_stream_privacy_status_public.
  ///
  /// In en, this message translates to:
  /// **'Public'**
  String get livekit_stream_privacy_status_public;

  /// No description provided for @livekit_stream_privacy_status_privacy.
  ///
  /// In en, this message translates to:
  /// **'Privacy'**
  String get livekit_stream_privacy_status_privacy;

  /// No description provided for @livekit_stream_categories_daily_chat.
  ///
  /// In en, this message translates to:
  /// **'Daily chat'**
  String get livekit_stream_categories_daily_chat;

  /// No description provided for @livekit_stream_categories_appearance.
  ///
  /// In en, this message translates to:
  /// **'Appearance'**
  String get livekit_stream_categories_appearance;

  /// No description provided for @livekit_stream_categories_knowledge_teaching.
  ///
  /// In en, this message translates to:
  /// **'Knowledge teaching'**
  String get livekit_stream_categories_knowledge_teaching;

  /// No description provided for @livekit_stream_categories_shopping.
  ///
  /// In en, this message translates to:
  /// **'Shopping'**
  String get livekit_stream_categories_shopping;

  /// No description provided for @livekit_stream_categories_music.
  ///
  /// In en, this message translates to:
  /// **'Music'**
  String get livekit_stream_categories_music;

  /// No description provided for @livekit_function_item_beauty.
  ///
  /// In en, this message translates to:
  /// **'Beauty'**
  String get livekit_function_item_beauty;

  /// No description provided for @livekit_function_item_music.
  ///
  /// In en, this message translates to:
  /// **'Music'**
  String get livekit_function_item_music;

  /// No description provided for @livekit_function_item_flip.
  ///
  /// In en, this message translates to:
  /// **'Flip'**
  String get livekit_function_item_flip;

  /// No description provided for @livekit_function_item_mirror.
  ///
  /// In en, this message translates to:
  /// **'Mirror'**
  String get livekit_function_item_mirror;

  /// No description provided for @livekit_start_live.
  ///
  /// In en, this message translates to:
  /// **'Go live'**
  String get livekit_start_live;

  /// No description provided for @livekit_preset_cover.
  ///
  /// In en, this message translates to:
  /// **'Preset Images'**
  String get livekit_preset_cover;

  /// No description provided for @livekit_set_as_cover.
  ///
  /// In en, this message translates to:
  /// **'Set as cover'**
  String get livekit_set_as_cover;

  /// No description provided for @livekit_stream_type.
  ///
  /// In en, this message translates to:
  /// **'Live categories'**
  String get livekit_stream_type;

  /// No description provided for @livekit_stream_id.
  ///
  /// In en, this message translates to:
  /// **'Stream Id'**
  String get livekit_stream_id;

  /// No description provided for @livekit_barrage_input_hint.
  ///
  /// In en, this message translates to:
  /// **'Please enter barrage'**
  String get livekit_barrage_input_hint;

  /// No description provided for @livekit_barrage_btn_send.
  ///
  /// In en, this message translates to:
  /// **'Send'**
  String get livekit_barrage_btn_send;

  /// No description provided for @livekit_barrage_me.
  ///
  /// In en, this message translates to:
  /// **'Me'**
  String get livekit_barrage_me;

  /// No description provided for @livekit_barrage_agree.
  ///
  /// In en, this message translates to:
  /// **'Agree'**
  String get livekit_barrage_agree;

  /// No description provided for @livekit_barrage_warning_not_empty.
  ///
  /// In en, this message translates to:
  /// **'input can\'t be empty!'**
  String get livekit_barrage_warning_not_empty;

  /// No description provided for @livekit_settings.
  ///
  /// In en, this message translates to:
  /// **'Settings'**
  String get livekit_settings;

  /// No description provided for @livekit_audio_effect.
  ///
  /// In en, this message translates to:
  /// **'Audio'**
  String get livekit_audio_effect;

  /// No description provided for @livekit_more_settings.
  ///
  /// In en, this message translates to:
  /// **'More Config'**
  String get livekit_more_settings;

  /// No description provided for @livekit_video_params.
  ///
  /// In en, this message translates to:
  /// **'Video Config'**
  String get livekit_video_params;

  /// No description provided for @livekit_video_config.
  ///
  /// In en, this message translates to:
  /// **'Video'**
  String get livekit_video_config;

  /// No description provided for @livekit_link.
  ///
  /// In en, this message translates to:
  /// **'LINK'**
  String get livekit_link;

  /// No description provided for @livekit_pk.
  ///
  /// In en, this message translates to:
  /// **'Anchor PK'**
  String get livekit_pk;

  /// No description provided for @livekit_send_gift.
  ///
  /// In en, this message translates to:
  /// **'Send a Gift'**
  String get livekit_send_gift;

  /// No description provided for @livekit_like.
  ///
  /// In en, this message translates to:
  /// **'like'**
  String get livekit_like;

  /// No description provided for @livekit_comment.
  ///
  /// In en, this message translates to:
  /// **'Comment'**
  String get livekit_comment;

  /// No description provided for @livekit_hidden_audience_nickname.
  ///
  /// In en, this message translates to:
  /// **'Hide audience nickname'**
  String get livekit_hidden_audience_nickname;

  /// No description provided for @livekit_allow_share.
  ///
  /// In en, this message translates to:
  /// **'Allow sharing'**
  String get livekit_allow_share;

  /// No description provided for @livekit_waiting_pass.
  ///
  /// In en, this message translates to:
  /// **'Waiting'**
  String get livekit_waiting_pass;

  /// No description provided for @livekit_gift_give_gift.
  ///
  /// In en, this message translates to:
  /// **'Give gift'**
  String get livekit_gift_give_gift;

  /// No description provided for @livekit_gift_title.
  ///
  /// In en, this message translates to:
  /// **'Gift'**
  String get livekit_gift_title;

  /// No description provided for @livekit_gift_me.
  ///
  /// In en, this message translates to:
  /// **'Me'**
  String get livekit_gift_me;

  /// No description provided for @livekit_music.
  ///
  /// In en, this message translates to:
  /// **'Music'**
  String get livekit_music;

  /// No description provided for @livekit_music_cheerful.
  ///
  /// In en, this message translates to:
  /// **'Cheerful'**
  String get livekit_music_cheerful;

  /// No description provided for @livekit_music_melancholy.
  ///
  /// In en, this message translates to:
  /// **'Melancholy'**
  String get livekit_music_melancholy;

  /// No description provided for @livekit_music_wonder_world.
  ///
  /// In en, this message translates to:
  /// **'Wonder World'**
  String get livekit_music_wonder_world;

  /// No description provided for @livekit_clarity.
  ///
  /// In en, this message translates to:
  /// **'Clarity'**
  String get livekit_clarity;

  /// No description provided for @livekit_fps.
  ///
  /// In en, this message translates to:
  /// **'FPS'**
  String get livekit_fps;

  /// No description provided for @livekit_ear_return.
  ///
  /// In en, this message translates to:
  /// **'Ear return'**
  String get livekit_ear_return;

  /// No description provided for @livekit_ear_return_volume.
  ///
  /// In en, this message translates to:
  /// **'Ear return volume'**
  String get livekit_ear_return_volume;

  /// No description provided for @livekit_background_music.
  ///
  /// In en, this message translates to:
  /// **'Background music'**
  String get livekit_background_music;

  /// No description provided for @livekit_select_music.
  ///
  /// In en, this message translates to:
  /// **'Select music'**
  String get livekit_select_music;

  /// No description provided for @livekit_audio_settings.
  ///
  /// In en, this message translates to:
  /// **'Audio settings'**
  String get livekit_audio_settings;

  /// No description provided for @livekit_music_volume.
  ///
  /// In en, this message translates to:
  /// **'Music volume'**
  String get livekit_music_volume;

  /// No description provided for @livekit_people_volume.
  ///
  /// In en, this message translates to:
  /// **'Voice volume'**
  String get livekit_people_volume;

  /// No description provided for @livekit_music_pitch.
  ///
  /// In en, this message translates to:
  /// **'Music pitch'**
  String get livekit_music_pitch;

  /// No description provided for @livekit_change_voice.
  ///
  /// In en, this message translates to:
  /// **'Voice change'**
  String get livekit_change_voice;

  /// No description provided for @livekit_change_voice_none.
  ///
  /// In en, this message translates to:
  /// **'Original voice'**
  String get livekit_change_voice_none;

  /// No description provided for @livekit_change_voice_child.
  ///
  /// In en, this message translates to:
  /// **'Naughty child'**
  String get livekit_change_voice_child;

  /// No description provided for @livekit_change_voice_girl.
  ///
  /// In en, this message translates to:
  /// **'Lori'**
  String get livekit_change_voice_girl;

  /// No description provided for @livekit_change_voice_uncle.
  ///
  /// In en, this message translates to:
  /// **'Uncle'**
  String get livekit_change_voice_uncle;

  /// No description provided for @livekit_change_voice_metal.
  ///
  /// In en, this message translates to:
  /// **'Heavy metal'**
  String get livekit_change_voice_metal;

  /// No description provided for @livekit_change_voice_cold.
  ///
  /// In en, this message translates to:
  /// **'Cold'**
  String get livekit_change_voice_cold;

  /// No description provided for @livekit_change_voice_foreign_language.
  ///
  /// In en, this message translates to:
  /// **'Foreign language accent'**
  String get livekit_change_voice_foreign_language;

  /// No description provided for @livekit_change_voice_trapped_beast.
  ///
  /// In en, this message translates to:
  /// **'Trapped beast'**
  String get livekit_change_voice_trapped_beast;

  /// No description provided for @livekit_change_voice_fat_house.
  ///
  /// In en, this message translates to:
  /// **'fat house'**
  String get livekit_change_voice_fat_house;

  /// No description provided for @livekit_change_voice_strong_current.
  ///
  /// In en, this message translates to:
  /// **'Strong current'**
  String get livekit_change_voice_strong_current;

  /// No description provided for @livekit_change_voice_machinery.
  ///
  /// In en, this message translates to:
  /// **'Heavy machinery'**
  String get livekit_change_voice_machinery;

  /// No description provided for @livekit_change_voice_ethereal.
  ///
  /// In en, this message translates to:
  /// **'Ethereal'**
  String get livekit_change_voice_ethereal;

  /// No description provided for @livekit_reverb.
  ///
  /// In en, this message translates to:
  /// **'Reverb'**
  String get livekit_reverb;

  /// No description provided for @livekit_reverb_none.
  ///
  /// In en, this message translates to:
  /// **'No effect'**
  String get livekit_reverb_none;

  /// No description provided for @livekit_reverb_karaoke.
  ///
  /// In en, this message translates to:
  /// **'KTV'**
  String get livekit_reverb_karaoke;

  /// No description provided for @livekit_reverb_small_room.
  ///
  /// In en, this message translates to:
  /// **'Small room'**
  String get livekit_reverb_small_room;

  /// No description provided for @livekit_reverb_town_hall.
  ///
  /// In en, this message translates to:
  /// **'Town Hall'**
  String get livekit_reverb_town_hall;

  /// No description provided for @livekit_reverb_low.
  ///
  /// In en, this message translates to:
  /// **'Low'**
  String get livekit_reverb_low;

  /// No description provided for @livekit_reverb_loud_and_loud.
  ///
  /// In en, this message translates to:
  /// **'loud'**
  String get livekit_reverb_loud_and_loud;

  /// No description provided for @livekit_reverb_metallic_sound.
  ///
  /// In en, this message translates to:
  /// **'Metallic sound'**
  String get livekit_reverb_metallic_sound;

  /// No description provided for @livekit_reverb_metallic_magnetic.
  ///
  /// In en, this message translates to:
  /// **'Magnetic'**
  String get livekit_reverb_metallic_magnetic;

  /// No description provided for @livekit_reverb_metallic_ethereal.
  ///
  /// In en, this message translates to:
  /// **'Ethereal'**
  String get livekit_reverb_metallic_ethereal;

  /// No description provided for @livekit_reverb_metallic_recording_studio.
  ///
  /// In en, this message translates to:
  /// **'Recording studio'**
  String get livekit_reverb_metallic_recording_studio;

  /// No description provided for @livekit_reverb_metallic_melodious.
  ///
  /// In en, this message translates to:
  /// **'melodious'**
  String get livekit_reverb_metallic_melodious;

  /// No description provided for @livekit_reverb_metallic_recording_studio2.
  ///
  /// In en, this message translates to:
  /// **'Recording Studio 2'**
  String get livekit_reverb_metallic_recording_studio2;

  /// No description provided for @livekit_tips_title.
  ///
  /// In en, this message translates to:
  /// **'Warm Tips'**
  String get livekit_tips_title;

  /// No description provided for @livekit_confirm_delete_tips.
  ///
  /// In en, this message translates to:
  /// **'Are you sure you want to delete \"%s\"?'**
  String get livekit_confirm_delete_tips;

  /// No description provided for @livekit_cancel.
  ///
  /// In en, this message translates to:
  /// **'Cancel'**
  String get livekit_cancel;

  /// No description provided for @livekit_confirm.
  ///
  /// In en, this message translates to:
  /// **'Confirm'**
  String get livekit_confirm;

  /// No description provided for @livekit_waiting_link.
  ///
  /// In en, this message translates to:
  /// **'Waiting Linking'**
  String get livekit_waiting_link;

  /// No description provided for @livekit_live_room_list.
  ///
  /// In en, this message translates to:
  /// **'Live'**
  String get livekit_live_room_list;

  /// No description provided for @livekit_live_has_stop.
  ///
  /// In en, this message translates to:
  /// **'Live broadcast has ended'**
  String get livekit_live_has_stop;

  /// No description provided for @livekit_sent.
  ///
  /// In en, this message translates to:
  /// **' sent '**
  String get livekit_sent;

  /// No description provided for @livekit_entered_room.
  ///
  /// In en, this message translates to:
  /// **'Entered room'**
  String get livekit_entered_room;

  /// No description provided for @livekit_who_live_room.
  ///
  /// In en, this message translates to:
  /// **' Live Room'**
  String get livekit_who_live_room;

  /// No description provided for @livekit_gift_balance.
  ///
  /// In en, this message translates to:
  /// **'Balance'**
  String get livekit_gift_balance;

  /// No description provided for @livekit_gift_recharge.
  ///
  /// In en, this message translates to:
  /// **'Recharge'**
  String get livekit_gift_recharge;

  /// No description provided for @livekit_gift_balance_insufficient.
  ///
  /// In en, this message translates to:
  /// **'Balance is insufficient'**
  String get livekit_gift_balance_insufficient;

  /// No description provided for @livekit_preview_video_live.
  ///
  /// In en, this message translates to:
  /// **'Live Video'**
  String get livekit_preview_video_live;

  /// No description provided for @livekit_preview_voice_live.
  ///
  /// In en, this message translates to:
  /// **'Live Voice'**
  String get livekit_preview_voice_live;

  /// No description provided for @livekit_audience_count_in_room.
  ///
  /// In en, this message translates to:
  /// **'%d people are watching'**
  String get livekit_audience_count_in_room;

  /// No description provided for @livekit_loading.
  ///
  /// In en, this message translates to:
  /// **'Loading...'**
  String get livekit_loading;

  /// No description provided for @livekit_no_more_data.
  ///
  /// In en, this message translates to:
  /// **'There is no more data'**
  String get livekit_no_more_data;

  /// No description provided for @livekit_no_room_tip.
  ///
  /// In en, this message translates to:
  /// **'Please start a live room'**
  String get livekit_no_room_tip;

  /// No description provided for @livekit_common_this_live_data.
  ///
  /// In en, this message translates to:
  /// **'Live data'**
  String get livekit_common_this_live_data;

  /// No description provided for @livekit_common_live_duration.
  ///
  /// In en, this message translates to:
  /// **'Duration'**
  String get livekit_common_live_duration;

  /// No description provided for @livekit_common_gift_income.
  ///
  /// In en, this message translates to:
  /// **'Gift Income'**
  String get livekit_common_gift_income;

  /// No description provided for @livekit_common_live_people_number.
  ///
  /// In en, this message translates to:
  /// **'Total Viewers'**
  String get livekit_common_live_people_number;

  /// No description provided for @livekit_common_message_count.
  ///
  /// In en, this message translates to:
  /// **'Messages'**
  String get livekit_common_message_count;

  /// No description provided for @livekit_common_send_gift_people_count.
  ///
  /// In en, this message translates to:
  /// **'Gift givers'**
  String get livekit_common_send_gift_people_count;

  /// No description provided for @livekit_common_like_count.
  ///
  /// In en, this message translates to:
  /// **'Likes'**
  String get livekit_common_like_count;

  /// No description provided for @livekit_anchor_audience_list_panel_title.
  ///
  /// In en, this message translates to:
  /// **'Online audience'**
  String get livekit_anchor_audience_list_panel_title;

  /// No description provided for @livekit_follow_anchor.
  ///
  /// In en, this message translates to:
  /// **'Follow'**
  String get livekit_follow_anchor;

  /// No description provided for @livekit_unfollow_anchor.
  ///
  /// In en, this message translates to:
  /// **'unfollow'**
  String get livekit_unfollow_anchor;

  /// No description provided for @livekit_live_room_id.
  ///
  /// In en, this message translates to:
  /// **'Live Room ID:'**
  String get livekit_live_room_id;

  /// No description provided for @livekit_fan_count.
  ///
  /// In en, this message translates to:
  /// **'Fans'**
  String get livekit_fan_count;

  /// No description provided for @livekit_beauty_panel_title.
  ///
  /// In en, this message translates to:
  /// **'One-click beauty'**
  String get livekit_beauty_panel_title;

  /// No description provided for @livekit_beauty_item_none.
  ///
  /// In en, this message translates to:
  /// **'Close'**
  String get livekit_beauty_item_none;

  /// No description provided for @livekit_beauty_item_smooth.
  ///
  /// In en, this message translates to:
  /// **'Smooth'**
  String get livekit_beauty_item_smooth;

  /// No description provided for @livekit_beauty_item_whiteness.
  ///
  /// In en, this message translates to:
  /// **'Whiteness'**
  String get livekit_beauty_item_whiteness;

  /// No description provided for @livekit_beauty_item_ruddy.
  ///
  /// In en, this message translates to:
  /// **'Ruddy'**
  String get livekit_beauty_item_ruddy;

  /// No description provided for @livekit_resolution_360p.
  ///
  /// In en, this message translates to:
  /// **'LD'**
  String get livekit_resolution_360p;

  /// No description provided for @livekit_resolution_540p.
  ///
  /// In en, this message translates to:
  /// **'SD'**
  String get livekit_resolution_540p;

  /// No description provided for @livekit_resolution_720p.
  ///
  /// In en, this message translates to:
  /// **'HD'**
  String get livekit_resolution_720p;

  /// No description provided for @livekit_resolution_1080p.
  ///
  /// In en, this message translates to:
  /// **'Ultra HD'**
  String get livekit_resolution_1080p;

  /// No description provided for @livekit_enable_audience_request_link.
  ///
  /// In en, this message translates to:
  /// **'Allow viewers to apply for continuous microphone'**
  String get livekit_enable_audience_request_link;

  /// No description provided for @livekit_link_mic_up_title.
  ///
  /// In en, this message translates to:
  /// **'Current wheat position'**
  String get livekit_link_mic_up_title;

  /// No description provided for @livekit_link_mic_down_title.
  ///
  /// In en, this message translates to:
  /// **'Apply for continuous wheat'**
  String get livekit_link_mic_down_title;

  /// No description provided for @livekit_link_mic_down_title_popup.
  ///
  /// In en, this message translates to:
  /// **'Requesting'**
  String get livekit_link_mic_down_title_popup;

  /// No description provided for @livekit_title_link_mic_selector.
  ///
  /// In en, this message translates to:
  /// **'Choose Link Mode'**
  String get livekit_title_link_mic_selector;

  /// No description provided for @livekit_text_link_mic_selector.
  ///
  /// In en, this message translates to:
  /// **'connect upon host\\\'s approval'**
  String get livekit_text_link_mic_selector;

  /// No description provided for @livekit_text_link_mic_video.
  ///
  /// In en, this message translates to:
  /// **'Apply for video link'**
  String get livekit_text_link_mic_video;

  /// No description provided for @livekit_text_link_mic_audio.
  ///
  /// In en, this message translates to:
  /// **'Apply for audio link'**
  String get livekit_text_link_mic_audio;

  /// No description provided for @livekit_text_cancel_link_mic_apply.
  ///
  /// In en, this message translates to:
  /// **'Cancel application for link mic'**
  String get livekit_text_cancel_link_mic_apply;

  /// No description provided for @livekit_text_close_link_mic.
  ///
  /// In en, this message translates to:
  /// **'End the link mic'**
  String get livekit_text_close_link_mic;

  /// No description provided for @livekit_link_mic_manager.
  ///
  /// In en, this message translates to:
  /// **'Link Management'**
  String get livekit_link_mic_manager;

  /// No description provided for @livekit_hang_up.
  ///
  /// In en, this message translates to:
  /// **'Hang Up'**
  String get livekit_hang_up;

  /// No description provided for @livekit_accept.
  ///
  /// In en, this message translates to:
  /// **'Accept'**
  String get livekit_accept;

  /// No description provided for @livekit_reject.
  ///
  /// In en, this message translates to:
  /// **'Reject'**
  String get livekit_reject;

  /// No description provided for @livekit_toast_apply_link_mic.
  ///
  /// In en, this message translates to:
  /// **'You have submitted a link mic request, please wait for the author approval'**
  String get livekit_toast_apply_link_mic;

  /// No description provided for @livekit_title_link_video_settings.
  ///
  /// In en, this message translates to:
  /// **'Adjust the video link mic screen'**
  String get livekit_title_link_video_settings;

  /// No description provided for @livekit_btn_apply_link_mic.
  ///
  /// In en, this message translates to:
  /// **'Apply for link mic'**
  String get livekit_btn_apply_link_mic;

  /// No description provided for @livekit_tips_apply_link_mic.
  ///
  /// In en, this message translates to:
  /// **'The screen effect will automatically take effect after connecting'**
  String get livekit_tips_apply_link_mic;
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
