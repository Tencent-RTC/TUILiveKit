//
//  TUIEffectPlayerService.m
//  TCEffectPlayerKit
//
//  Created by krabyu on 2024/7/18.
//

#import "TUIEffectPlayerService.h"
#import "TUICore/TUICore.h"
#import "TCConstant.h"
#import <TCEffectPlayer/TCEffectAnimView.h>
#import <TCMediaX/TCMediaX.h>

@interface TUIEffectPlayerService() <TUIServiceProtocol,TCEPAnimViewDelegate>
@property (nonatomic, copy) TUICallServiceResultCallback callback;
@end

@implementation TUIEffectPlayerService
+ (void)load {
    [TUICore registerService:KEY_SERVICE_NAME object: [TUIEffectPlayerService sharedInstance]];
}

+ (instancetype) sharedInstance {
    static TUIEffectPlayerService* service;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        service = [[TUIEffectPlayerService alloc] init];
    });
    return service;
}

- (id)onCall:(NSString *)method param:(nullable NSDictionary *)param resultCallback:(TUICallServiceResultCallback)resultCallback {
    if ([method isEqualToString: KEY_METHOD_SET_LICENSE]) {
        NSString *licenseUrl = param[KEY_PARAM_LICENSE_URL];
        NSString *licenseKey = param[KEY_PARAM_LICENSE_KEY];
        if (licenseUrl && licenseKey ) {
            [[TCMediaXBase getInstance] setLicenceURL:licenseUrl key:licenseKey];
        }
    } else if ([method isEqualToString:KEY_METHOD_START_PLAY]) {
        NSString * url = param[KEY_PARAM_PLAY_URL];
        UIView* view = param[KEY_PARAM_VIEW];
        if ([view isKindOfClass: [TCEffectAnimView class]]) {
            TCEffectAnimView * animationView = (TCEffectAnimView* )view;
            [animationView setEffectPlayerDelegate:self];
            [animationView startPlay:url];
            self.callback = resultCallback;
        }
    } else if ([method isEqualToString:KEY_METHOD_STOP_PLAY]) {
        UIView* view = param[KEY_PARAM_VIEW];
        if ([view isKindOfClass: [TCEffectAnimView class]]) {
            TCEffectAnimView * animationView = (TCEffectAnimView* )view;
            [animationView setEffectPlayerDelegate:NULL];
            [animationView stopPlay];
        }
    }
    return nil;
}
    
#pragma mark - TCEPAnimViewDelegate
-(void) onPlayEvent:(ITCEffectPlayer *)player event:(int)EvtID withParam:(NSDictionary *)param {
    int event_play_stop = 2006;
    if (EvtID == event_play_stop && self.callback) {
        self.callback(0, @"play finished", @{@"":@""});
    }
}

@end
