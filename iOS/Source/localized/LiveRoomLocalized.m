//
//  LiveRoomLocalized.m
//  Pods
//
//  Created by abyyxwang on 2021/5/6.
//  Copyright © 2022 Tencent. All rights reserved.

#import "LiveRoomLocalized.h"
#import "TUICore/TUIGlobalization.h"

#pragma mark - Base
NSBundle *liveRoomBundle(void) {
    NSURL *liveRoomKitBundleURL = [[NSBundle mainBundle] URLForResource:@"TUILiveRoomKitBundle" withExtension:@"bundle"];
    NSBundle *liveRoomBundle = [NSBundle bundleWithURL:liveRoomKitBundleURL];
    return liveRoomBundle;
}

NSString *liveRoomLocalizeFromTable(NSString *key, NSString *table) {
    NSString *bundlePath = [liveRoomBundle() pathForResource:[TUIGlobalization tk_localizableLanguageKey] ?: @"" ofType:@"lproj"];
    NSBundle *bundle = [NSBundle bundleWithPath:bundlePath];
    return [bundle localizedStringForKey:key value:@"" table:table];
}

#pragma mark - Replace String
NSString *localizeReplaceXX(NSString *origin, NSString *xxx_replace) {
    if (xxx_replace == nil) { xxx_replace = @"";}
    return [origin stringByReplacingOccurrencesOfString:@"xxx" withString:xxx_replace];
}

NSString *localizeReplace(NSString *origin, NSString *xxx_replace, NSString *yyy_replace) {
    if (yyy_replace == nil) { yyy_replace = @"";}
    return [localizeReplaceXX(origin, xxx_replace) stringByReplacingOccurrencesOfString:@"yyy" withString:yyy_replace];
}

NSString *localizeReplaceThreeCharacter(NSString *origin, NSString *xxx_replace, NSString *yyy_replace, NSString *zzz_replace) {
    if (zzz_replace == nil) { zzz_replace = @"";}
    return [localizeReplace(origin, xxx_replace, yyy_replace) stringByReplacingOccurrencesOfString:@"zzz" withString:zzz_replace];
}

NSString *localizeReplaceFourCharacter(NSString *origin, NSString *xxx_replace, NSString *yyy_replace, NSString *zzz_replace, NSString *mmm_replace) {
    if (mmm_replace == nil) { mmm_replace = @"";}
    return [localizeReplaceThreeCharacter(origin, xxx_replace, yyy_replace, zzz_replace)
 stringByReplacingOccurrencesOfString:@"mmm" withString:mmm_replace];
}

NSString *localizeReplaceFiveCharacter(NSString *origin, NSString *xxx_replace, NSString
 *yyy_replace, NSString *zzz_replace, NSString *mmm_replace, NSString *nnn_replace) {
    if (nnn_replace == nil) { nnn_replace = @"";}
    return [localizeReplaceFourCharacter(origin, xxx_replace, yyy_replace, zzz_replace,
 mmm_replace) stringByReplacingOccurrencesOfString:@"nnn" withString:nnn_replace];
}


#pragma mark - LiveRoom
NSString *const liveRoom_Localize_TableName = @"LiveRoomLocalized";
NSString *liveRoomLocalize(NSString *key) {
    return liveRoomLocalizeFromTable(key, liveRoom_Localize_TableName);
}
