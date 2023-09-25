//
//  TRTCCloudAnction.m
//  TRTCVoiceRoomOCDemo
//
//  Created by abyyxwang on 2020/7/12.
//  Copyright © 2020 tencent. All rights reserved.
//

#import "TRTCCloudAnction.h"
#import "TUILiveRoomKit.h"
#import "TXLiveRoomCommonDef.h"
#import <MJRefresh/MJRefresh.h>
#import "LiveRoomLocalized.h"
#import <MJExtension/MJExtension.h>

static int gTrtcLivePlayTimeOut = 5;
static const int TC_COMPONENT_LIVEROOM = 4;
static const int TC_TRTC_FRAMEWORK     = 1;

@interface PlayInfo : NSObject

@property (nonatomic, strong) UIView *videoView;
@property (nonatomic, copy) NSString *streamId;
@property (nonatomic, copy) NSString *roomId;
@property (nonatomic, strong) TXLivePlayer *cdnPlayer;

- (instancetype)initWithVideoView:(UIView *)videoView streamId:(NSString *)streamId roomId:(NSString *)roomId;

@end

@implementation PlayInfo

- (instancetype)initWithVideoView:(UIView *)videoView streamId:(NSString *)streamId roomId:(NSString *)roomId {
    self = [super init];
    if (self) {
        self.videoView = videoView;
        self.streamId = streamId;
        self.roomId = roomId;
    }
    return self;
}

- (TXLivePlayer *)cdnPlayer {
    if (!_cdnPlayer) {
        _cdnPlayer = [[TXLivePlayer alloc] init];
    }
    return _cdnPlayer;
}

@end

@class TRTCCloudAnction;
@interface TRTCCloudCdnDelegate : NSObject<TXLivePlayListener>

@property (nonatomic, copy) NSString *streamId;
@property (nonatomic, weak)  TRTCCloudAnction *action;

- (instancetype)initWithStreamId:(NSString *)streamId;

@end

@implementation TRTCCloudCdnDelegate

- (instancetype)initWithStreamId:(NSString *)streamId {
    self = [super init];
    if (self) {
        self.streamId = streamId;
    }
    return self;
}

- (void)onPlayEvent:(int)evtID withParam:(NSDictionary *)param {
    if (evtID == PLAY_EVT_RCV_FIRST_I_FRAME) {
        if (self.action) {
            [self.action playCallBackWithUserId:self.streamId code:0 message:@""];
            self.action = nil;
        }
    } else if (evtID < 0) {
        if (self.action) {
            [self.action playCallBackWithUserId:self.streamId code:-1 message:@""];
            self.action = nil;
        }
    }
}

- (void)onNetStatus:(NSDictionary *)param {
    
}

@end


@interface TRTCCloudAnction ()

@property (nonatomic, strong) NSMutableDictionary<NSString *, Callback> *playCallbackMap;
@property (nonatomic, strong) NSMutableDictionary<NSString *, PlayInfo *> *userPlayInfo;
@property (nonatomic, copy) NSString *userId;
@property (nonatomic, copy) NSString *urlDomain;
@property (nonatomic, assign) int sdkAppId;
@property (nonatomic, copy) NSString *userSig;

@property (nonatomic, assign) BOOL isEnterRoom;
@property (nonatomic, strong) TRTCTranscodingConfig *mixConfig;
@end

@implementation TRTCCloudAnction

-(NSMutableDictionary<NSString *,Callback> *)playCallbackMap {
    if (!_playCallbackMap) {
        _playCallbackMap = [[NSMutableDictionary alloc] initWithCapacity:2];
    }
    return _playCallbackMap;
}

- (NSMutableDictionary<NSString *,PlayInfo *> *)userPlayInfo {
    if (!_userPlayInfo) {
        _userPlayInfo = [[NSMutableDictionary alloc] initWithCapacity:2];
    }
    return _userPlayInfo;
}

- (TXBeautyManager *)beautyManager {
    return [[TRTCCloud sharedInstance] getBeautyManager];
}

- (TRTCTranscodingConfig *)mixConfig {
    if (!_mixConfig) {
        _mixConfig = [[TRTCTranscodingConfig alloc] init];
        _mixConfig.appId = self.sdkAppId;
        _mixConfig.videoWidth = 1080;
        _mixConfig.videoHeight = 1920;
        _mixConfig.videoBitrate    = 3500;
        _mixConfig.videoFramerate  = 30;
        _mixConfig.videoGOP        = 2;
        _mixConfig.audioSampleRate = 48000;
        _mixConfig.audioBitrate    = 64;
        _mixConfig.audioChannels   = 2;
    }
    return _mixConfig;
}

#pragma mark - Public method
- (void)setupWithUserId:(NSString *)userId urlDomain:(NSString *)urlDomain sdkAppId:(int)sdkAppid userSig:(NSString *)userSig {
    self.userId = userId;
    self.urlDomain = urlDomain;
    self.sdkAppId = sdkAppid;
    self.userSig = userSig;
}

- (void)reset {
    self.userId = nil;
    self.urlDomain = nil;
    self.sdkAppId = 0;
    self.userSig = nil;
}

- (void)enterRoomWithRoomID:(NSString *)roomID userId:(NSString *)userId role:(TRTCRoleType)role {
    if (!self.userId || self.sdkAppId == 0 || self.isEnterRoom) {
        return;
    }
    self.isEnterRoom = YES;
    self.curroomUUID = [[[NSUUID alloc] init] UUIDString];
    TRTCParams *params = [[TRTCParams alloc] init];
    params.sdkAppId = self.sdkAppId;
    params.userSig = self.userSig;
    params.userId = self.userId;
    params.roomId = [roomID intValue];
    params.role = role;
    [self setFramework];
    [[TRTCCloud sharedInstance] enterRoom:params appScene:TRTCAppSceneLIVE];
}

- (void)setFramework {
    NSDictionary *jsonDic = @{@"api": @"setFramework",
                              @"params":@{@"framework": @(TC_TRTC_FRAMEWORK),
                                          @"component": @(TC_COMPONENT_LIVEROOM)}};
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:jsonDic options:NSJSONWritingPrettyPrinted error:nil];
    NSString *jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
    TRTCLog(@"jsonString = %@",jsonString);
    [[TRTCCloud sharedInstance] callExperimentalAPI: jsonString];
}

- (void)switchRole:(TRTCRoleType)role {
    [[TRTCCloud sharedInstance] switchRole:role];
}

- (void)exitRoom {
    [self.playCallbackMap removeAllObjects];
    [[TRTCCloud sharedInstance] exitRoom];
    self.isEnterRoom = NO;
    self.curroomUUID = nil;
}

- (void)enableHEVCEncode:(BOOL)enableHEVC {
    NSDictionary *jsonDic = @{@"api": @"enableHevcEncode",
                              @"params":@{@"enable": @(enableHEVC)}};
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:jsonDic options:NSJSONWritingPrettyPrinted error:nil];
    NSString *jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
    [[TRTCCloud sharedInstance] callExperimentalAPI: jsonString];
}

- (void)setupVideoParam:(BOOL)isOwner {
    TRTCVideoEncParam *videoParam = [[TRTCVideoEncParam alloc] init];
    if (isOwner) {
        videoParam.videoResolution = TRTCVideoResolution_1920_1080;
        videoParam.videoBitrate = 3500;
        videoParam.minVideoBitrate = 3200;
        videoParam.videoFps = 24;
    } else {
        videoParam.videoResolution = TRTCVideoResolution_480_270;
        videoParam.videoBitrate = 400;
        videoParam.videoFps = 24;
    }
    [[TRTCCloud sharedInstance] setVideoEncoderParam:videoParam];
    [self enableHEVCEncode:YES];
}

- (void)startLocalPreview:(BOOL)frontCamera view:(UIView *)view {
    [[TRTCCloud sharedInstance] startLocalPreview:frontCamera view:view];
}

- (void)stopLocalPreview {
    [[TRTCCloud sharedInstance] stopLocalPreview];
}

- (void)startPublish:(NSString *)streamId {
    if (!self.userId || !self.roomId) {
        return;
    }
    [self enterRoomWithRoomID:self.roomId userId:self.userId role:TRTCRoleAnchor];
    [[TRTCCloud sharedInstance] startLocalAudio];
    if (streamId && streamId.length > 0) {
        [[TRTCCloud sharedInstance] startPublishing:streamId type:TRTCVideoStreamTypeBig];
    }
}

- (void)stopPublish {
    [[TRTCCloud sharedInstance] stopLocalAudio];
    [[TRTCCloud sharedInstance] stopPublishing];
}

- (void)startPlay:(NSString *)userId streamID:(NSString *)streamID view:(UIView *)view
 usesCDN:(BOOL)usesCDN roomId:(NSString *)roomId callback:(Callback)callback {
    PlayInfo *info = self.userPlayInfo[userId];
    if (info) {
        if (callback) {
            callback(-1, liveRoomLocalize(@"Demo.TRTC.LiveRoom.donotreplaypls"));
        }
        return;
    }
    PlayInfo* playInfo = [[PlayInfo alloc] initWithVideoView:view streamId:streamID roomId:roomId];
    self.userPlayInfo[userId] = playInfo;
    if (usesCDN) {
        if (streamID && callback) {
            self.playCallbackMap[streamID] = callback;
        }
        [self startCDNPlay:playInfo.cdnPlayer streamId:streamID view:view];
    } else {
       if (userId && callback) {
            self.playCallbackMap[userId] = callback;
        }
        [[TRTCCloud sharedInstance] startRemoteView:userId view:view];
        NSString* blockId= [NSString stringWithFormat:@"%ld",(long)((NSInteger)callback)];
        @weakify(self)
        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(gTrtcLivePlayTimeOut * NSEC_PER_SEC)), dispatch_get_main_queue(), ^{
            @strongify(self)
            if (!self || !userId) {
                return;
            }
            NSString* goalBlockId= [NSString stringWithFormat:@"%ld",(long)((NSInteger)self.playCallbackMap[userId])];
            if ([goalBlockId isEqualToString:blockId]) {
                [self playCallBackWithUserId:userId code:-1 message:liveRoomLocalize(@"Demo.TRTC.LiveRoom.timeouttonotplay")];
            }
        });
    }
}

- (void)startTRTCPlay:(NSString *)userId {
    PlayInfo* info = self.userPlayInfo[userId];
    UIView* view = info.videoView;
    if (view) {
        [[TRTCCloud sharedInstance] startRemoteView:userId view:view];
    }
}

- (void)stopPlay:(NSString *)userId usesCDN:(BOOL)usesCDN {
    PlayInfo* playInfo = self.userPlayInfo[userId];
    if (usesCDN) {
        if (playInfo.streamId) {
            [self playCallBackWithUserId:playInfo.streamId code:-1 message:liveRoomLocalize(@"Demo.TRTC.LiveRoom.stopplaying")];
        }
        [self stopCdnPlay:playInfo.cdnPlayer];
    } else {
        [self playCallBackWithUserId:userId code:-1 message:liveRoomLocalize(@"Demo.TRTC.LiveRoom.stopplaying")];
        [[TRTCCloud sharedInstance] stopRemoteView:userId];
    }
    [self.userPlayInfo removeObjectForKey:userId];
}

- (void)stopAllPlay:(BOOL)usesCDN {
    if (usesCDN) {
        [[self.userPlayInfo allValues] enumerateObjectsUsingBlock:^(PlayInfo * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
            [self stopCdnPlay:obj.cdnPlayer];
        }];
    } else {
       [[TRTCCloud sharedInstance] stopAllRemoteView];
    }
    [self.userPlayInfo removeAllObjects];
}

- (void)onFirstVideoFrame:(NSString *)userId {
    [self playCallBackWithUserId:userId code:0 message:@""];
}

- (void)playCallBackWithUserId:(NSString *)userId code:(NSInteger)code message:(NSString *)message {
    Callback callback = self.playCallbackMap[userId];
    if (callback) {
        [self.playCallbackMap removeObjectForKey:userId];
        callback((int)code, message);
    }
}

- (void)togglePlay:(BOOL)usesCDN {
    if (usesCDN) {
        [self exitRoom];
        [self.userPlayInfo enumerateKeysAndObjectsUsingBlock:^(NSString * _Nonnull key, PlayInfo * _Nonnull obj, BOOL * _Nonnull stop) {
            [self startCDNPlay:obj.cdnPlayer streamId:obj.streamId view:obj.videoView];
        }];
    } else {
        [self.userPlayInfo enumerateKeysAndObjectsUsingBlock:^(NSString * _Nonnull key, PlayInfo * _Nonnull obj, BOOL * _Nonnull stop) {
            [self stopCdnPlay:obj.cdnPlayer];
        }];
        [self switchRole:TRTCRoleAudience];
    }
}

- (BOOL)isUserPlaying:(NSString *)userId {
    return self.userPlayInfo[userId] != nil;
}

- (void)startRoomPK:(NSString *)roomId userId:(NSString *)userId {
    NSDictionary *dic = @{@"strRoomId": roomId, @"userId": userId};
    [[TRTCCloud sharedInstance] connectOtherRoom:[dic mj_JSONString]];
}

- (void)updateMixingParams:(BOOL)shouldMix isRoomPK:(BOOL)isRoomPK {
    if (!self.userId) {
        return;
    }
    if (!shouldMix || self.userPlayInfo.count == 0) {
        [[TRTCCloud sharedInstance] setMixTranscodingConfig:nil];
        return;
    }
 
    NSMutableArray *users = [[NSMutableArray alloc] initWithCapacity:2];
    __block int index = 0;
    TRTCMixUser *me = [[TRTCMixUser alloc] init];
    me.userId = self.userId;
    me.zOrder = index;
    if (isRoomPK) {
        me.rect = CGRectMake(0, 0, self.mixConfig.videoWidth*0.5, self.mixConfig.videoHeight*0.5);
    } else {
        me.rect = CGRectMake(0, 0, self.mixConfig.videoWidth, self.mixConfig.videoHeight);
    }
    
    me.roomID = self.roomId;
    [users addObject:me];
    [self.userPlayInfo enumerateKeysAndObjectsUsingBlock:^(NSString * _Nonnull key, PlayInfo * _Nonnull obj, BOOL * _Nonnull stop) {
        index += 1;
        TRTCMixUser *user = [[TRTCMixUser alloc] init];
        user.userId = key;
        user.zOrder = index;
        user.rect = [self mixRect:obj.videoView.frame isRoomPK:isRoomPK index:index];
        user.roomID = obj.roomId;
        [users addObject:user];
    }];
    self.mixConfig.mixUsers = users;
    [[TRTCCloud sharedInstance] setMixTranscodingConfig:self.mixConfig];
}


- (CGRect)mixRect:(CGRect)userFrame isRoomPK:(BOOL)isRoomPK index:(int)index {
    if (isRoomPK) {
        CGSize size = CGSizeMake(self.mixConfig.videoWidth*0.5, self.mixConfig.videoHeight*0.5);
        return CGRectMake(size.width*(index%2), size.height*(index/2), size.width, size.height);
    } else {
        CGSize mainScreenSize =  UIScreen.mainScreen.bounds.size;
        CGFloat scaleWidth = self.mixConfig.videoWidth/mainScreenSize.width;
        CGFloat scaleHeight = self.mixConfig.videoHeight/mainScreenSize.height;
        return CGRectMake(userFrame.origin.x*scaleWidth,
                          userFrame.origin.y*scaleHeight,
                          userFrame.size.width*scaleWidth,
                          userFrame.size.height*scaleHeight);
    }
}

- (NSString *)cdnUrlForUser:(NSString *)userId roomId:(NSString *)roomId {
    if (self.sdkAppId == 0) {
        return @"";
    }
    return [NSString stringWithFormat:@"%d_%@_%@_main.flv", self.sdkAppId, self.roomId, self.userId];
}

#pragma mark - private method
- (void)startCDNPlay:(TXLivePlayer *)cdnPlayer streamId:(NSString *)streamId view:(UIView *)view {
    if (!self.urlDomain || !streamId) {
        return;
    }
    NSString *streamUrl = nil;
    if ([self.urlDomain hasSuffix:@"/"]) {
        streamUrl = [NSString stringWithFormat:@"%@%@", self.urlDomain, streamId];
    } else {
        streamUrl = [NSString stringWithFormat:@"%@/%@.flv", self.urlDomain, streamId];
    }
    [cdnPlayer setupVideoWidget:view.bounds containView:view insertIndex:0];
    TRTCCloudCdnDelegate *trtcCDNDelegate = [[TRTCCloudCdnDelegate alloc] initWithStreamId:streamId];
    trtcCDNDelegate.action = self;
    cdnPlayer.delegate = trtcCDNDelegate;
    int result = [cdnPlayer startLivePlay:streamUrl type:PLAY_TYPE_LIVE_FLV];
    if (result != 0) {
        [self playCallBackWithUserId:streamId code:result message:liveRoomLocalize(@"Demo.TRTC.LiveRoom.playingfailed")];
    }
}

- (void)stopCdnPlay:(TXLivePlayer *)cdnPlayer {
    cdnPlayer.delegate = nil;
    [cdnPlayer stopPlay];
    [cdnPlayer removeVideoWidget];
}

- (void)setFilter:(UIImage *)image {
    [[[TRTCCloud sharedInstance] getBeautyManager] setFilter:image];
}

- (void)setFilterConcentration:(float)concentration {
    [[[TRTCCloud sharedInstance] getBeautyManager] setFilterStrength:concentration];
}

- (void)setGreenScreenFile:(NSURL *)fileUrl {
    NSString *filePath = [fileUrl path];
    [[[TRTCCloud sharedInstance] getBeautyManager] setGreenScreenFile: filePath];
}


@end
