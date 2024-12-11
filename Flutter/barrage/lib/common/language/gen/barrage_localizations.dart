import 'dart:async';

import 'package:flutter/foundation.dart';
import 'package:flutter/widgets.dart';
import 'package:flutter_localizations/flutter_localizations.dart';
import 'package:intl/intl.dart' as intl;

import 'barrage_localizations_en.dart';
import 'barrage_localizations_zh.dart';

/// Callers can lookup localized strings with an instance of BarrageLocalizations
/// returned by `BarrageLocalizations.of(context)`.
///
/// Applications need to include `BarrageLocalizations.delegate()` in their app's
/// `localizationDelegates` list, and the locales they support in the app's
/// `supportedLocales` list. For example:
///
/// ```dart
/// import 'gen/barrage_localizations.dart';
///
/// return MaterialApp(
///   localizationsDelegates: BarrageLocalizations.localizationsDelegates,
///   supportedLocales: BarrageLocalizations.supportedLocales,
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
/// be consistent with the languages listed in the BarrageLocalizations.supportedLocales
/// property.
abstract class BarrageLocalizations {
  BarrageLocalizations(String locale)
      : localeName = intl.Intl.canonicalizedLocale(locale.toString());

  final String localeName;

  static BarrageLocalizations? of(BuildContext context) {
    return Localizations.of<BarrageLocalizations>(
        context, BarrageLocalizations);
  }

  static const LocalizationsDelegate<BarrageLocalizations> delegate =
      _BarrageLocalizationsDelegate();

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

  /// No description provided for @barrage_let_us_chat.
  ///
  /// In en, this message translates to:
  /// **'Let\\\'s talk'**
  String get barrage_let_us_chat;

  /// No description provided for @barrage_send.
  ///
  /// In en, this message translates to:
  /// **'Send'**
  String get barrage_send;

  /// No description provided for @barrage_anchor.
  ///
  /// In en, this message translates to:
  /// **'Anchor'**
  String get barrage_anchor;
}

class _BarrageLocalizationsDelegate
    extends LocalizationsDelegate<BarrageLocalizations> {
  const _BarrageLocalizationsDelegate();

  @override
  Future<BarrageLocalizations> load(Locale locale) {
    return SynchronousFuture<BarrageLocalizations>(
        lookupBarrageLocalizations(locale));
  }

  @override
  bool isSupported(Locale locale) =>
      <String>['en', 'zh'].contains(locale.languageCode);

  @override
  bool shouldReload(_BarrageLocalizationsDelegate old) => false;
}

BarrageLocalizations lookupBarrageLocalizations(Locale locale) {
  // Lookup logic when only language code is specified.
  switch (locale.languageCode) {
    case 'en':
      return BarrageLocalizationsEn();
    case 'zh':
      return BarrageLocalizationsZh();
  }

  throw FlutterError(
      'BarrageLocalizations.delegate failed to load unsupported locale "$locale". This is likely '
      'an issue with the localizations generation tool. Please file an issue '
      'on GitHub with a reproducible sample app and the gen-l10n configuration '
      'that was used.');
}
