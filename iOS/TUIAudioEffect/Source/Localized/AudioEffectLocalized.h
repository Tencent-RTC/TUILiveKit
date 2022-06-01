//
//  AudioEffectLocalized.h
//  TUIAudioEffect
//
//  Created by jack on 2021/9/27.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Base
extern NSBundle *TUIAudioEffectBundle(void);

#pragma mark - AudioEffect String

extern NSString *const TUIAudioEffect_Localize_TableName;
extern NSString *TUIAudioEffectLocalize(NSString *key);

NS_ASSUME_NONNULL_END
