//
//  TUIAudioEffectDefine.h
//  Pods
//
//  Created by jack on 2021/9/27.
//

#ifndef TUIAudioEffectDefine_h
#define TUIAudioEffectDefine_h

#import <TUICore/TUIDefine.h>
#import <Masonry/Masonry.h>

#import "AudioEffectLocalized.h"

// Categories
#import <TUICore/UIColor+TUIHexColor.h>
#import <TUICore/UIView+TUIUtil.h>

// Theme
#import "TUILiveThemeConfig.h"
// Model
#import "TUIAudioEffectModel.h"

#import "txg_log_audio.h"

#define TUIAudioEffectBundle          TUIAudioEffectBundle()
#define TUIAEImageNamed(imageName) [UIImage imageNamed:imageName inBundle:TUIAudioEffectBundle compatibleWithTraitCollection:nil]

#define TUIAEMakeColorHexString(hexString) [UIColor tui_colorWithHex:hexString]

#endif /* TUIAudioEffectDefine_h */
