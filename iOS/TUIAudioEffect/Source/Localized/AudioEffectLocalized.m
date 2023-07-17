//
//  AudioEffectLocalized.m
//  TUIAudioEffect
//
//  Created by jack on 2021/9/27.
//

#import "AudioEffectLocalized.h"
#import <TUICore/TUIGlobalization.h>

#pragma mark - Base

NSBundle *TUIAudioEffectBundle(void) {
    NSURL *audioEffectKitBundleURL = [[NSBundle mainBundle] URLForResource:@"TUIAudioEffectKitBundle" withExtension:@"bundle"];
    return [NSBundle bundleWithURL:audioEffectKitBundleURL];
}

NSString *TUIAudioEffectLocalizeFromTable(NSString *key, NSString *table) {
    NSString *bundlePath = [TUIAudioEffectBundle() pathForResource:[TUIGlobalization tk_localizableLanguageKey] ?: @"" ofType:@"lproj"];
    NSBundle *bundle = [NSBundle bundleWithPath:bundlePath];
    return [bundle localizedStringForKey:key value:@"" table:table];
}

NSString *TUIAudioEffectLocalizeFromTableAndCommon(NSString *key, NSString *common, NSString *table) {
    return TUIAudioEffectLocalizeFromTable(key, table);
}

#pragma mark - AudioEffect

NSString *const TUIAudioEffect_Localize_TableName = @"AudioEffectLocalized";
NSString *TUIAudioEffectLocalize(NSString *key) {
    return TUIAudioEffectLocalizeFromTable(key, TUIAudioEffect_Localize_TableName);
}
