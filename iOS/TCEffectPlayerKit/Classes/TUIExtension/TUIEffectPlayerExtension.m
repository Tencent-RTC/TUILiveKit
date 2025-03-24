//
//  TUIEffectPlayerExtension.m
//  TCEffectPlayerKit
//
//  Created by krabyu on 2024/7/18.
//

#import "TUIEffectPlayerExtension.h"
#import "TUICore/TUICore.h"
#import "TCConstant.h"
#import <TCEffectPlayer/TCEffectAnimView.h>

@interface TUIEffectPlayerExtension() <TUIExtensionProtocol, TCEPAnimViewDelegate>

@end

@implementation TUIEffectPlayerExtension

+ (void)load {
    [TUICore registerExtension: KEY_GET_VIEW object:[TUIEffectPlayerExtension sharedInstance]];
}

+ (instancetype) sharedInstance {
    static TUIEffectPlayerExtension* extension;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        extension = [[TUIEffectPlayerExtension alloc] init];
    });
    return extension;
}
    
#pragma mark - TUIExtensionProtocol
- (NSArray<TUIExtensionInfo *> *)onGetExtension:(NSString *)key param:(NSDictionary *)param {
    NSMutableArray<TUIExtensionInfo *> *resultExtensionInfoList = [NSMutableArray array];
    if (!key || ![param isKindOfClass:[NSDictionary class]]) {
        return nil;
    }
    if ([key isEqualToString: KEY_GET_VIEW]) {
        TCEffectAnimView* animationView = [[TCEffectAnimView alloc] init];
        [animationView setLoop:FALSE];
        NSDictionary *info = @{KEY_GET_VIEW: animationView};
        TUIExtensionInfo *resultExtensionInfo = [[TUIExtensionInfo alloc] init];
        resultExtensionInfo.data = info;
        [resultExtensionInfoList addObject:resultExtensionInfo];
        return resultExtensionInfoList;
    }
    
    return nil;
}

- (void)onPlayEvent:(ITCEffectPlayer *)player
              event:(int)EvtID
          withParam:(NSDictionary *)param {
    NSLog(@"event:%d param:%@",EvtID, param);
}

@end
