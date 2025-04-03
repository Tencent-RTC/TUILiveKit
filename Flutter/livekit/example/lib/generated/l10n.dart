// GENERATED CODE - DO NOT MODIFY BY HAND
import 'package:flutter/material.dart';
import 'package:intl/intl.dart';
import 'intl/messages_all.dart';

// **************************************************************************
// Generator: Flutter Intl IDE plugin
// Made by Localizely
// **************************************************************************

// ignore_for_file: non_constant_identifier_names, lines_longer_than_80_chars
// ignore_for_file: join_return_with_assignment, prefer_final_in_for_each
// ignore_for_file: avoid_redundant_argument_values, avoid_escaping_inner_quotes

class S {
  S();

  static S? _current;

  static S get current {
    assert(
      _current != null,
      'No instance of S was loaded. Try to initialize the S delegate before accessing S.current.',
    );
    return _current!;
  }

  static const AppLocalizationDelegate delegate = AppLocalizationDelegate();

  static Future<S> load(Locale locale) {
    final name =
        (locale.countryCode?.isEmpty ?? false)
            ? locale.languageCode
            : locale.toString();
    final localeName = Intl.canonicalizedLocale(name);
    return initializeMessages(localeName).then((_) {
      Intl.defaultLocale = localeName;
      final instance = S();
      S._current = instance;

      return instance;
    });
  }

  static S of(BuildContext context) {
    final instance = S.maybeOf(context);
    assert(
      instance != null,
      'No instance of S present in the widget tree. Did you add S.delegate in localizationsDelegates?',
    );
    return instance!;
  }

  static S? maybeOf(BuildContext context) {
    return Localizations.of<S>(context, S);
  }

  /// `Tencent RTC`
  String get app_trtc {
    return Intl.message('Tencent RTC', name: 'app_trtc', desc: '', args: []);
  }

  /// `User ID`
  String get app_user_id {
    return Intl.message('User ID', name: 'app_user_id', desc: '', args: []);
  }

  /// `Please enter your UserID`
  String get app_enter_user_id {
    return Intl.message(
      'Please enter your UserID',
      name: 'app_enter_user_id',
      desc: '',
      args: [],
    );
  }

  /// `Login`
  String get app_login {
    return Intl.message('Login', name: 'app_login', desc: '', args: []);
  }

  /// `Logout`
  String get app_logout {
    return Intl.message('Logout', name: 'app_logout', desc: '', args: []);
  }

  /// `Confirm`
  String get app_confirm {
    return Intl.message('Confirm', name: 'app_confirm', desc: '', args: []);
  }

  /// `Cancel`
  String get app_cancel {
    return Intl.message('Cancel', name: 'app_cancel', desc: '', args: []);
  }

  /// `Enter your user nickname`
  String get app_enter_nickname {
    return Intl.message(
      'Enter your user nickname',
      name: 'app_enter_nickname',
      desc: '',
      args: [],
    );
  }

  /// `Tencent Cloud`
  String get app_tencent_cloud {
    return Intl.message(
      'Tencent Cloud',
      name: 'app_tencent_cloud',
      desc: '',
      args: [],
    );
  }

  /// `Login failed`
  String get app_login_fail {
    return Intl.message(
      'Login failed',
      name: 'app_login_fail',
      desc: '',
      args: [],
    );
  }

  /// `Continue`
  String get app_next {
    return Intl.message('Continue', name: 'app_next', desc: '', args: []);
  }

  /// `Nickname`
  String get app_nick_name {
    return Intl.message('Nickname', name: 'app_nick_name', desc: '', args: []);
  }

  /// `Room ID`
  String get app_room_id {
    return Intl.message('Room ID', name: 'app_room_id', desc: '', args: []);
  }

  /// `Anchor`
  String get app_anchor {
    return Intl.message('Anchor', name: 'app_anchor', desc: '', args: []);
  }

  /// `Audience`
  String get app_audience {
    return Intl.message('Audience', name: 'app_audience', desc: '', args: []);
  }

  /// `Me`
  String get app_me {
    return Intl.message('Me', name: 'app_me', desc: '', args: []);
  }

  /// `Live`
  String get app_live {
    return Intl.message('Live', name: 'app_live', desc: '', args: []);
  }

  /// `Likes`
  String get app_follow_count {
    return Intl.message('Likes', name: 'app_follow_count', desc: '', args: []);
  }

  /// `Fans`
  String get app_fans_count {
    return Intl.message('Fans', name: 'app_fans_count', desc: '', args: []);
  }

  /// `Name`
  String get app_set_nickname {
    return Intl.message('Name', name: 'app_set_nickname', desc: '', args: []);
  }

  /// `Save`
  String get app_save {
    return Intl.message('Save', name: 'app_save', desc: '', args: []);
  }

  /// `Living`
  String get app_video {
    return Intl.message('Living', name: 'app_video', desc: '', args: []);
  }

  /// `Live preview/Beauty filters/Multi-hostK`
  String get app_video_description {
    return Intl.message(
      'Live preview/Beauty filters/Multi-hostK',
      name: 'app_video_description',
      desc: '',
      args: [],
    );
  }

  /// `Voice`
  String get app_voice {
    return Intl.message('Voice', name: 'app_voice', desc: '', args: []);
  }

  /// `High audio quality/Large room/Smooth mic on/off`
  String get app_voice_description {
    return Intl.message(
      'High audio quality/Large room/Smooth mic on/off',
      name: 'app_voice_description',
      desc: '',
      args: [],
    );
  }

  /// `{xxx} Broadcast`
  String app_broadcast(Object xxx) {
    return Intl.message(
      '$xxx Broadcast',
      name: 'app_broadcast',
      desc: '',
      args: [xxx],
    );
  }
}

class AppLocalizationDelegate extends LocalizationsDelegate<S> {
  const AppLocalizationDelegate();

  List<Locale> get supportedLocales {
    return const <Locale>[
      Locale.fromSubtags(languageCode: 'en'),
      Locale.fromSubtags(languageCode: 'zh'),
    ];
  }

  @override
  bool isSupported(Locale locale) => _isSupported(locale);
  @override
  Future<S> load(Locale locale) => S.load(locale);
  @override
  bool shouldReload(AppLocalizationDelegate old) => false;

  bool _isSupported(Locale locale) {
    for (var supportedLocale in supportedLocales) {
      if (supportedLocale.languageCode == locale.languageCode) {
        return true;
      }
    }
    return false;
  }
}
