//
//  TRTCCloudAnction.h
//  TRTCVoiceRoomOCDemo
//
//  Created by abyyxwang on 2020/7/12.
//  Copyright © 2020 tencent. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "TRTCLiveRoomDef.h"
#import "TUILiveRoomKit.h"

NS_ASSUME_NONNULL_BEGIN

@class TXBeautyManager;
@interface TRTCCloudAnction : NSObject

@property (nonatomic, copy, nullable) NSString *roomId;
@property (nonatomic, copy, nullable) NSString *curroomUUID;
@property (nonatomic, strong, readonly) TXBeautyManager *beautyManager;

- (void)setupWithUserId:(NSString *)userId urlDomain:(NSString * _Nullable)urlDomain sdkAppId:(int)sdkAppid userSig:(NSString *)userSig;

- (void)reset;

- (void)enterRoomWithRoomID:(NSString *)roomID userId:(NSString *)userId role:(TRTCRoleType)role;

- (void)switchRole:(TRTCRoleType)role;

- (void)exitRoom;

- (void)setupVideoParam:(BOOL)isOwner;

- (void)startLocalPreview:(BOOL)frontCamera view:(UIView *)view;

- (void)stopLocalPreview;

- (void)startPublish:(NSString *)streamId;

- (void)stopPublish;

- (void)startPlay:(NSString *)userId streamID:(NSString *)streamID view:(UIView *)view
 usesCDN:(BOOL)usesCDN roomId:(NSString * _Nullable)roomId callback:(Callback _Nullable)callback;

- (void)startTRTCPlay:(NSString *)userId;

- (void)stopPlay:(NSString *)userId usesCDN:(BOOL)usesCDN;

- (void)stopAllPlay:(BOOL)usesCDN;

- (void)onFirstVideoFrame:(NSString *)userId;

- (void)playCallBackWithUserId:(NSString *)userId code:(NSInteger)code message:(NSString *)message;

- (void)togglePlay:(BOOL)usesCDN;

- (BOOL)isUserPlaying:(NSString *)userId;

- (void)startRoomPK:(NSString *)roomId userId:(NSString *)userId;

- (void)updateMixingParams:(BOOL)shouldMix isRoomPK:(BOOL)isRoomPK;

- (CGRect)rectWithIndex:(int)index width:(CGFloat)width height:(CGFloat)height padding:(CGFloat)padding;

- (NSString *)cdnUrlForUser:(NSString *)userId roomId:(NSString *)roomId;

- (void)setFilter:(UIImage *)image;

- (void)setFilterConcentration:(float)concentration;

- (void)setGreenScreenFile:(NSURL *)fileUrl;

@end

NS_ASSUME_NONNULL_END
