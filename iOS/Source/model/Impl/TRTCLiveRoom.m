//
//  TRTCLiveRoom.m
//  TRTCVoiceRoomOCDemo
//
//  Created by abyyxwang on 2020/7/7.
//  Copyright © 2020 tencent. All rights reserved.
//

#import "TRTCLiveRoom.h"
#import "TRTCLiveRoomMemberManager.h"
#import "TRTCLiveRoomModelDef.h"
#import "TXLiveRoomCommonDef.h"
#import "TRTCLiveRoomIMAction.h"
#import "TRTCCloudAnction.h"
#import <MJExtension/MJExtension.h>
#import <ImSDK_Plus/ImSDK_Plus.h>
#import "LiveRoomLocalized.h"
#import "TUILogin.h"

static double trtcLiveCheckStatusTimeOut = 3;


@interface NSNumber (String)

- (BOOL)isEqualToString:(NSString *)string;

@end

@implementation NSNumber (String)

- (BOOL)isEqualToString:(NSString *)string {
    return NO;
}

@end

@interface TRTCLiveRoom () <TRTCLiveRoomMembermanagerDelegate, V2TIMAdvancedMsgListener, V2TIMGroupListener, V2TIMSignalingListener>

@property (nonatomic, strong) TRTCCloudAnction *trtcAction;
@property (nonatomic, strong) TRTCLiveRoomMemberManager *memberManager;
@property (nonatomic, strong) TRTCLiveRoomConfig *config;
@property (nonatomic, strong) TRTCLiveUserInfo *me;
@property (nonatomic, assign) BOOL mixingPKStream;
@property (nonatomic, assign) BOOL mixingLinkMicStream;
@property (nonatomic, strong) TRTCLiveRoomInfo *curRoomInfo;

@property (nonatomic, strong, readonly) TXBeautyManager *beautyManager;
@property (nonatomic, assign) TRTCLiveRoomLiveStatus status;

@property (nonatomic, strong, readonly) NSString *roomID;
@property (nonatomic, strong, readonly) NSString *ownerId;

@property (nonatomic, copy) Callback enterRoomCallback;
@property (nonatomic, copy) ResponseCallback requestJoinAnchorCallback;
@property (nonatomic, copy) ResponseCallback requestRoomPKCallback;

@property (nonatomic, strong) TRTCPKAnchorInfo *pkAnchorInfo;
@property (nonatomic, strong) NSMutableDictionary<NSString *, NSString *> *requestRoomPKDic;
@property (nonatomic, strong) TRTCJoinAnchorInfo *joinAnchorInfo;
@property (nonatomic, strong, nullable)NSString *requestJoinAnchorID;


@property (nonatomic, assign, readonly) BOOL isOwner;
@property (nonatomic, assign, readonly) BOOL isAnchor;
@property (nonatomic, assign, readonly) BOOL configCdn;
@property (nonatomic, assign, readonly) BOOL shouldPlayCdn;
@property (nonatomic, assign, readonly) BOOL shouldMixStream;

@property (nonatomic, strong) NSMutableDictionary<NSString *, TRTCJoinAnchorInfo *> *onJoinAnchorDic;
@property (nonatomic, strong) NSMutableDictionary<NSString *, NSString *> *requestJoinAnchorDic;
@property (nonatomic, strong) NSMutableDictionary<NSString *, NSString *> *responseRoomPKDic;

@property (nonatomic, assign) TRTCVideoResolution videoResolution;
@property (nonatomic, assign) int videoFPS;
@property (nonatomic, assign) int videoBitrate;

@end

@implementation TRTCLiveRoom

+ (instancetype)shareInstance {
    static TRTCLiveRoom *instance = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        instance = [[TRTCLiveRoom alloc] init];
    });
    return instance;
}

- (instancetype)init
{
    self = [super init];
    if (self) {
        self.memberManager.delegate = self;
        self.videoResolution = TRTCVideoResolution_1920_1080;
        self.videoFPS = 24;
        self.videoBitrate = 3500;
        
        // 默认混流开启
        self.mixingPKStream = true;
        self.mixingLinkMicStream = true;
    }
    return self;
}

- (void)dealloc {
    TRTCLog(@"dealloc TRTCLiveRoom");
}

#pragma mark - Getter
- (TRTCCloudAnction *)trtcAction {
    if (!_trtcAction) {
        _trtcAction = [[TRTCCloudAnction alloc] init];
    }
    return _trtcAction;
}

- (TRTCLiveRoomMemberManager *)memberManager {
    if (!_memberManager) {
        _memberManager = [[TRTCLiveRoomMemberManager alloc] init];
    }
    return _memberManager;
}

- (TRTCPKAnchorInfo *)pkAnchorInfo {
    if (!_pkAnchorInfo) {
        _pkAnchorInfo = [[TRTCPKAnchorInfo alloc] init];
    }
    return _pkAnchorInfo;
}

-(TRTCJoinAnchorInfo *)joinAnchorInfo {
    if (!_joinAnchorInfo) {
        _joinAnchorInfo = [[TRTCJoinAnchorInfo alloc] init];
    }
    return _joinAnchorInfo;
}

- (NSMutableDictionary<NSString *,NSString *> *)requestJoinAnchorDic {
    if (!_requestJoinAnchorDic) {
        _requestJoinAnchorDic = [NSMutableDictionary dictionaryWithCapacity:2];
    }
    return _requestJoinAnchorDic;
}

- (NSMutableDictionary<NSString *,NSString *> *)responseRoomPKDic {
    if (!_responseRoomPKDic) {
        _responseRoomPKDic = [NSMutableDictionary dictionaryWithCapacity:2];
    }
    return _responseRoomPKDic;
}

- (NSMutableDictionary<NSString *,NSString *> *)requestRoomPKDic {
    if (!_requestRoomPKDic) {
        _requestRoomPKDic = [NSMutableDictionary dictionaryWithCapacity:2];
    }
    return _requestRoomPKDic;
}

- (NSString *)roomID {
    return self.trtcAction.roomId;
}

- (NSString *)ownerId {
    return self.memberManager.ownerId;
}

- (TXBeautyManager *)beautyManager {
    return self.trtcAction.beautyManager;
}

- (void)setStatus:(TRTCLiveRoomLiveStatus)status {
    if (_status != status) {
        if (self.curRoomInfo) {
            self.curRoomInfo.roomStatus = status;
            if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onRoomInfoChange:)]) {
                [self.delegate trtcLiveRoom:self onRoomInfoChange:self.curRoomInfo];
            }
        } else {
            NSString *streameUrl = [NSString stringWithFormat:@"%@_stream", self.me.userId];
            TRTCLiveRoomInfo* roomInfo = [[TRTCLiveRoomInfo alloc] initWithRoomId:self.roomID
                                                                         roomName:@""
                                                                         coverUrl:@""
                                                                          ownerId:self.me.userId ?: @""
                                                                        ownerName:self.me.userName ?: @""
                                                                        streamUrl:streameUrl
                                                                      memberCount:self.memberManager.audience.count
                                                                       roomStatus:status];
            if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onRoomInfoChange:)]) {
                [self.delegate trtcLiveRoom:self onRoomInfoChange:roomInfo];
            }
        }
    }
    _status = status;
}


#pragma mark - public method
- (void)loginWithSdkAppID:(int)sdkAppID userID:(NSString *)userID userSig:(NSString *)userSig config:(TRTCLiveRoomConfig *)config callback:(Callback)callback {
    
    [self initIMListener];
    @weakify(self)
    [TUILogin login:sdkAppID userID:userID userSig:userSig succ:^{
        @strongify(self)
        if (!self) {
            return;
        }
        TRTCLiveUserInfo *user = [[TRTCLiveUserInfo alloc] init];
        user.userId = userID;
        self.me = user;
        self.config = config;
        [self.trtcAction setupWithUserId:userID urlDomain:config.cdnPlayDomain sdkAppId:sdkAppID userSig:userSig];
        if (callback) {
            callback(0, @"login success.");
        }
    } fail:^(int code, NSString *msg) {
        @strongify(self)
        if (!self) {
            return;
        }
        if (callback) {
            callback(code, msg);
        }
    }];
}

- (void)logout:(Callback)callback {
    @weakify(self)
    [TUILogin logout:^{
        @strongify(self)
        if (!self) {
            return;
        }
        self.me = nil;
        self.config = nil;
        [self.trtcAction reset];
        if (callback) {
            callback(0, @"success");
        }
    } fail:^(int code, NSString *msg) {
        @strongify(self)
        if (!self) {
            return;
        }
        self.me = nil;
        self.config = nil;
        [self.trtcAction reset];
        if (callback) {
            callback(code, (msg ?: @"logout error"));
        }
    }];
}

- (void)setSelfProfileWithName:(NSString *)name avatarURL:(NSString *)avatarURL callback:(Callback)callback {
    TRTCLiveUserInfo *me = [self checkUserLogIned:callback];
    if (!me) {
        return;
    }
    me.avatarURL = avatarURL;
    me.userName = name;
    @weakify(self)
    [TRTCLiveRoomIMAction setProfileWithName:name avatar:avatarURL callback:^(int code, NSString * _Nonnull message) {
        @strongify(self)
        if (!self) {
            return;
        }
        if (code == 0) {
            [self.memberManager updateProfile:me.userId name:name avatar:avatarURL];
        }
        if (callback) {
            callback(code, message);
        }
    }];
}

- (void)createRoomWithRoomID:(UInt32)roomID roomParam:(TRTCCreateRoomParam *)roomParam callback:(Callback)callback {
    [TRTCCloud sharedInstance].delegate = self;
    [self initIMListener];
    TRTCLiveUserInfo *me = [self checkUserLogIned:callback];
    if (!me) {
        return;
    }
    BOOL result = [self checkRoomUnjoined:callback];
    if (!result) {
        return;
    }
    NSString *roomIDStr = [NSString stringWithFormat:@"%d", roomID];
    self.curRoomInfo = [[TRTCLiveRoomInfo alloc] initWithRoomId:roomIDStr
                                                       roomName:roomParam.roomName
                                                       coverUrl:roomParam.coverUrl
                                                        ownerId:me.userId
                                                      ownerName:me.userName
                                                      streamUrl:[NSString stringWithFormat:@"%@_stream", me.userId]
                                                    memberCount:0
                                                     roomStatus:TRTCLiveRoomLiveStatusSingle];
    @weakify(self)
    [TRTCLiveRoomIMAction createRoomWithRoomID:roomIDStr
                                     roomParam:roomParam success:^(NSArray<TRTCLiveUserInfo *> * _Nonnull members, NSDictionary<NSString *,id> * _Nonnull customInfo, TRTCLiveRoomInfo * _Nullable roomInfo) {
        @strongify(self)
        if (!self) {
            return;
        }
        self.status = TRTCLiveRoomLiveStatusSingle;
        self.trtcAction.roomId = roomIDStr;
        [self.memberManager setmembers:members groupInfo:customInfo];
        [self.memberManager setOwner:me];
        if (callback) {
            callback(0, @"");
        }
    } error:callback];
}

- (void)destroyRoom:(Callback)callback {
    TRTCLiveUserInfo *user = [self checkUserLogIned:callback];
    if (!user) {
        return;
    }
    NSString *roomId = [self checkRoomJoined:callback];
    if (!roomId) {
        return;
    }
    if (![self checkIsOwner:callback]) {
        return;
    }
    __weak typeof(self) weakSelf = self;
    [TRTCLiveRoomIMAction destroyRoomWithRoomID:roomId callback:^(int code, NSString * _Nonnull message) {
        __strong typeof(weakSelf) strongSelf = weakSelf;
        if (!strongSelf) {
            return;
        }
        [strongSelf unInitIMListener];
        if (callback) {
            callback(code, message);
        }
    }];
    [self reset];
    [TRTCCloud destroySharedIntance];
}

- (void)enterRoomWithRoomID:(UInt32)roomID callback:(Callback)callback {
    [TRTCCloud sharedInstance].delegate = self;
    [self initIMListener];
    TRTCLiveUserInfo *me = [self checkUserLogIned:callback];
    if (!me) {
        return;
    }
    if (![self checkRoomUnjoined:callback]) {
        return;
    }
    NSString *roomIDStr = [NSString stringWithFormat:@"%d", roomID];
    if (self.shouldPlayCdn) {
        self.trtcAction.roomId = roomIDStr;
        [self imEnter:roomIDStr callback:callback];
    } else {
        [self trtcEnter:roomIDStr userId:me.userId callback:callback];
        @weakify(self)
        [self imEnter:roomIDStr callback:^(int code, NSString * _Nullable message) {
            @strongify(self)
            if (!self) {
                return;
            }
            if (code != 0 && [self canDelegateResponseMethod:@selector(trtcLiveRoom:onError:message:)]) {
                [self.delegate trtcLiveRoom:self onError:code message:message];
            }
        }];
    }
}

- (void)imEnter:(NSString *)roomID callback:(Callback)callback {
    @weakify(self)
    [TRTCLiveRoomIMAction enterRoomWithRoomID:roomID success:^(NSArray<TRTCLiveUserInfo *> * _Nonnull members, NSDictionary<NSString *,id> * _Nonnull customInfo, TRTCLiveRoomInfo * _Nullable roomInfo) {
        @strongify(self)
        if (!self) {
            return;
        }
        [self.memberManager setmembers:members groupInfo:customInfo];
        for (TRTCLiveUserInfo *info in members) {
            if ([info.userId isEqualToString:roomInfo.ownerId]) {
                [self.memberManager setOwner:info];
                break;
            }
        }
        self.curRoomInfo = roomInfo;
        self.status = roomInfo != nil ? roomInfo.roomStatus : TRTCLiveRoomLiveStatusSingle;
        if (callback) {
            callback(0, @"");
        }
        if (self.shouldPlayCdn) {
            [self notifyAvailableStreams];
        }
    } error:callback];
}

- (void)trtcEnter:(NSString *)roomID userId:(NSString *)userId callback:(Callback)callback {
    self.trtcAction.roomId = roomID;
    self.enterRoomCallback = callback;
    [self.trtcAction enterRoomWithRoomID:roomID userId:userId role:TRTCRoleAudience];
    NSString *uuid = [self.trtcAction.curroomUUID mutableCopy];
    @weakify(self)
    dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(10 * NSEC_PER_SEC)), dispatch_get_main_queue(), ^{
        @strongify(self)
        if (!self) {
            return;
        }
        if ([uuid isEqualToString:self.trtcAction.curroomUUID] && self.enterRoomCallback) {
            self.enterRoomCallback(-1, liveRoomLocalize(@"Demo.TRTC.LiveRoom.enterroomtimeout"));
            self.enterRoomCallback = nil;
        }
    });
}

- (void)exitRoom:(Callback)callback {
    if (![self checkUserLogIned:callback]) {
        return;
    }
    NSString *roomID = [self checkRoomJoined:callback];
    if (!roomID) {
        return;
    }
    if (self.isOwner) {
        if (callback) {
            callback(-1, liveRoomLocalize(@"Demo.TRTC.LiveRoom.onlyordinarymembercanexit"));
        }
        return;
    }
    [self reset];
    [TRTCLiveRoomIMAction exitRoomWithRoomID:roomID callback:callback];
    [TRTCCloud destroySharedIntance];
}

- (void)getRoomInfosWithRoomIDs:(NSArray<NSString *> *)roomIDs callback:(RoomInfoCallback)callback {
    [TRTCLiveRoomIMAction getRoomInfoWithRoomIds:roomIDs success:^(NSArray<TRTCLiveRoomInfo *> * _Nonnull roomInfos) {
        NSMutableArray *sortInfo = [[NSMutableArray alloc] initWithCapacity:2];
        NSMutableDictionary *resultMap = [[NSMutableDictionary alloc] initWithCapacity:2];
        for (TRTCLiveRoomInfo *room in roomInfos) {
            resultMap[room.roomId] = room;
        }
        for (NSString *roomId in roomIDs) {
            if ([resultMap.allKeys containsObject:roomId]) {
                TRTCLiveRoomInfo *roomInfo = resultMap[roomId];
                if (roomInfo) {
                    [sortInfo addObject:roomInfo];
                }
            }
        }
        if (callback) {
            callback(0, @"success", sortInfo);
        }
    } error:^(int code, NSString * _Nonnull message) {
        if (callback) {
            callback(code, message, @[]);
        }
    }];
}

- (void)getAnchorList:(UserListCallback)callback {
    if (callback) {
        callback(0, @"", self.memberManager.anchors.allValues);
    }
}

- (void)getAudienceList:(UserListCallback)callback {
    NSString *roomId = [self checkRoomJoined:nil];
    if (!roomId) {
        if (callback) {
            callback(-1, liveRoomLocalize(@"Demo.TRTC.LiveRoom.notenterroom"), self.memberManager.audience);
        }
        return;
    }
    @weakify(self)
    [TRTCLiveRoomIMAction getAllMembersWithRoomID:roomId success:^(NSArray<TRTCLiveUserInfo *> * _Nonnull members) {
        @strongify(self)
        if (!self) {
            return;
        }
        for (TRTCLiveUserInfo *user in members) {
            if (!self.memberManager.anchors[user.userId]) {
                [self.memberManager addAudience:user];
            }
        }
        if (callback) {
            callback(0, @"", self.memberManager.audience);
        }
    } error:^(int code, NSString * _Nonnull message) {
        @strongify(self)
        if (!self) {
            return;
        }
        if (callback) {
            callback(0, @"", self.memberManager.audience);
        }
    }];
}

- (void)startCameraPreviewWithFrontCamera:(BOOL)frontCamera view:(UIView *)view callback:(Callback)callback {
    if (![self checkUserLogIned:callback]) {
        return;
    }
    [self.trtcAction startLocalPreview:frontCamera view:view];
    callback(0, @"success");
}

- (void)stopCameraPreview {
    [self.trtcAction stopLocalPreview];
}

- (void)startPublishWithStreamID:(NSString *)streamID callback:(Callback)callback {
    TRTCLiveUserInfo *me = [self checkUserLogIned:callback];
    if (!me) {
        return;
    }
    NSString *roomID = [self checkRoomJoined:callback];
    if (!roomID) {
        return;
    }
    [self.trtcAction setupVideoParam:self.isOwner];
    [self.trtcAction startPublish:streamID];
    NSString *streamIDNonnull = ([streamID isEqualToString:@""] || !streamID) ? [self.trtcAction cdnUrlForUser:me.userId roomId:roomID] : streamID;
    if (self.isOwner) {
        [self.memberManager updateStream:me.userId streamId:streamIDNonnull];
        if (callback) {
            callback(0, @"");
        }
    } else if (self.ownerId) {
        [self.trtcAction switchRole:TRTCRoleAnchor];
    } else {
        NSAssert(NO, @"");
    }
}

- (void)stopPublish:(Callback)callback {
    TRTCLiveUserInfo *me = [self checkUserLogIned:callback];
    if (!me) {
        return;
    }
    NSString *roomID = [self checkRoomJoined:callback];
    if (!roomID) {
        return;
    }
    [self.trtcAction stopPublish];
    [self.memberManager updateStream:me.userId streamId:nil];
    if (self.isOwner) {
        [self.trtcAction exitRoom];
    } else {
        [self.memberManager updateStream:me.userId streamId:nil];
        [self stopCameraPreview];
        [self switchRoleOnLinkMic:NO];
    }
}

- (void)startPlayWithUserID:(NSString *)userID view:(UIView *)view callback:(Callback)callback {
    TRTCLiveUserInfo *me = [self checkUserLogIned:callback];
    if (!me) {
        return;
    }
    NSString *roomID = [self checkRoomJoined:callback];
    if (!roomID) {
        return;
    }
    TRTCLiveUserInfo *user = self.memberManager.pkAnchor;
    NSString *pkAnchorRoomId = self.pkAnchorInfo.roomId;
    if (user && pkAnchorRoomId) {
        [self.trtcAction startPlay:user.userId streamID:user.streamId view:view usesCDN:self.shouldPlayCdn roomId:pkAnchorRoomId callback:callback];
    } else if (self.memberManager.anchors[userID]) {
        TRTCLiveUserInfo *anchor = self.memberManager.anchors[userID];
        [self.trtcAction startPlay:anchor.userId streamID:anchor.streamId view:view usesCDN:self.shouldPlayCdn roomId:nil callback:callback];
    } else {
        if (callback) {
            callback(-1, liveRoomLocalize(@"Demo.TRTC.LiveRoom.notfoundanchor"));
        }
    }
}

- (void)stopPlayWithUserID:(NSString *)userID callback:(Callback)callback {
    if (!userID) {
        callback(-1, @"user id is nil");
        return;
    }
    [self.trtcAction stopPlay:userID usesCDN:self.shouldPlayCdn];
    if (callback) {
        callback(0, @"");
    }
}

- (void)requestJoinAnchor:(NSString *)reason timeout:(int)timeout responseCallback:(ResponseCallback)responseCallback {
    if (!responseCallback) {
        responseCallback = ^(BOOL reslut, NSString *msg) {};
    }
    TRTCLiveUserInfo *me = [self checkUserLogIned:nil];
    if (!me) {
        responseCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.notlogin"));
        return;
    }
    NSString *roomID = [self checkRoomJoined:nil];
    if (!roomID) {
        responseCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.notenterroom"));
        return;
    }
    if (self.isAnchor) {
        responseCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.ismicconnectednow"));
        return;
    }
    if (self.status == TRTCLiveRoomLiveStatusRoomPK || self.pkAnchorInfo.userId) {
        responseCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.anchorisinpk"));
        return;
    }
    if (self.joinAnchorInfo.userId) {
        responseCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.userwaitingresponseformicconnect"));
        return;
    }
    if (self.status == TRTCLiveRoomLiveStatusNone) {
        responseCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.smtwrongandretry"));
        return;
    }
    
    if (!self.ownerId) {
        return;
    }
    self.requestJoinAnchorCallback = responseCallback;
    self.joinAnchorInfo.userId = me.userId ?: @"";
    self.joinAnchorInfo.uuid = [[NSUUID UUID] UUIDString];
    __weak typeof(self) weakSelf = self;
    self.requestJoinAnchorID = [TRTCLiveRoomIMAction requestJoinAnchorWithUserID:self.ownerId timeout:timeout reason:reason callback:^(int code, NSString * _Nonnull message) {
        __strong __typeof(weakSelf) self = weakSelf;
        if (code != 0) {
            [self clearJoinState:YES userID:me.userId];
            if (responseCallback) {
                responseCallback(NO, message);
            }
        } else {
            NSLog(@"send link-mic msg success");
        }
    }];
}

- (void)cancelRequestJoinAnchor:(NSString *)reason responseCallback:(Callback)responseCallback {
    if (self.requestJoinAnchorID) {
        [TRTCLiveRoomIMAction cancelRequestJoinAnchorWithRequestID:self.requestJoinAnchorID reason:reason callback:responseCallback];
        self.requestJoinAnchorID = nil;
    }
}

- (void)cancelRequestRoomPKWithRoomID:(UInt32)roomID userID:(NSString *)userID responseCallback:(Callback)responseCallback {
    if ([self.requestRoomPKDic.allKeys containsObject:userID]) {
        NSString *requestID = [self.requestRoomPKDic objectForKey:userID];
        [TRTCLiveRoomIMAction cancelRequestRoomPKWithRequestID:requestID reason:@"" callback:responseCallback];
        [self.requestRoomPKDic removeObjectForKey:userID];
        [self.pkAnchorInfo reset];
        [self.memberManager removeAnchor:self.memberManager.pkAnchor.userId];
    }
}

- (void)responseJoinAnchor:(NSString *)userID agree:(BOOL)agree reason:(NSString *)reason {
    TRTCLiveUserInfo *me = [self checkUserLogIned:nil];
    if (!me && !self.isOwner) {
        return;
    }
    NSString *roomID = [self checkRoomJoined:nil];
    if (!roomID) {
        return;
    }
    if ([self.requestJoinAnchorDic.allKeys containsObject:userID]) {
        if (agree) {
            if ([TRTCCloud sharedInstance].delegate != self) {
                [TRTCCloud sharedInstance].delegate = self;
            }
            TRTCJoinAnchorInfo *joinAnchorInfo = [[TRTCJoinAnchorInfo alloc] init];
            joinAnchorInfo.uuid = [[NSUUID UUID] UUIDString];
            joinAnchorInfo.userId = userID;
            joinAnchorInfo.isResponsed = YES;
            [self.onJoinAnchorDic setObject:joinAnchorInfo forKey:userID];
            NSString* uuid = [joinAnchorInfo.uuid copy];
            @weakify(self)
            dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(trtcLiveCheckStatusTimeOut * NSEC_PER_SEC)), dispatch_get_main_queue(), ^{
                @strongify(self)
                if (!self) {
                    return;
                }
                TRTCJoinAnchorInfo *info = self.onJoinAnchorDic[userID];
                if (self.memberManager.anchors[userID] == nil && [uuid isEqualToString:info.uuid]) {
                    [self kickoutJoinAnchor:userID callback:nil];
                    [self clearJoinState:YES userID:userID];
                } else {
                    [self clearJoinState:NO userID:userID];
                }
            });
        } else {
            [self clearJoinState:NO userID:userID];
        }
    }
    NSString *requestID = [self.requestJoinAnchorDic objectForKey:userID];
    if (requestID) {
        [TRTCLiveRoomIMAction respondJoinAnchorWithRequestID:requestID agreed:agree reason:reason callback:nil];
        [self.requestJoinAnchorDic removeObjectForKey:userID];
    }
}

- (void)responseKickoutJoinAnchorWithRequestID:(NSString *)requestID {
    [TRTCLiveRoomIMAction respondKickoutJoinAnchor:requestID agree:YES message:@"agree kickOut"];
}

- (void)kickoutJoinAnchor:(NSString *)userID callback:(Callback)callback {
    TRTCLiveUserInfo *me = [self checkUserLogIned:callback];
    if (!me && !self.isOwner) {
        return;
    }
    NSString *roomID = [self checkRoomJoined:callback];
    if (!roomID) {
        return;
    }
    if (!self.memberManager.anchors[userID]) {
        if (callback) {
            callback(-1, liveRoomLocalize(@"Demo.TRTC.LiveRoom.usernotmicconnect"));
        }
        return;
    }
    __weak typeof(self) wealSelf = self;
    [TRTCLiveRoomIMAction kickoutJoinAnchorWithUserID:userID callback:^(int code, NSString * _Nonnull message) {
        __strong typeof(wealSelf) strongSelf = wealSelf;
        if (code == 0) {
            [strongSelf stopLinkMic:userID];
        }
    }];
    
}

- (void)requestRoomPKWithRoomID:(UInt32)roomID userID:(NSString *)userID timeout:(int)timeout responseCallback:(ResponseCallback)responseCallback {
    if (!responseCallback) {
        responseCallback = ^(BOOL reslut, NSString *msg) {};
    }
    TRTCLiveUserInfo *me = [self checkUserLogIned:nil];
    if (!me) {
        responseCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.notlogin"));
        return;
    }
    NSString *myRoomId = [self checkRoomJoined:nil];
    if (!myRoomId) {
        responseCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.notenterroom"));
        return;
    }
    NSString* streamId = [self checkIsPublishing:nil];
    if (!streamId) {
        responseCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.onlypushstreamcanoperate"));
        return;
    }
    if (self.status == TRTCLiveRoomLiveStatusLinkMic || self.joinAnchorInfo.userId) {
        responseCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.anchorisconnectingandunablepk"));
        return;
    }
    if (self.status == TRTCLiveRoomLiveStatusRoomPK) {
        responseCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.anchorisinpk"));
        return;
    }
    if (self.pkAnchorInfo.userId) {
        responseCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.useriswaitingforpkrep"));
        return;
    }
    if (self.status == TRTCLiveRoomLiveStatusNone) {
        responseCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.smtwrongandretry"));
        return;
    }
    NSString *roomIDStr = [NSString stringWithFormat:@"%u", (unsigned int)roomID];
    self.requestRoomPKCallback = responseCallback;
    self.pkAnchorInfo.userId = userID;
    self.pkAnchorInfo.roomId = roomIDStr;
    self.pkAnchorInfo.uuid = [[NSUUID UUID] UUIDString];
    NSString *requesetPKID = [TRTCLiveRoomIMAction requestRoomPKWithUserID:userID timeout:timeout fromRoomID:myRoomId fromStreamID:streamId callback:^(int code, NSString * _Nonnull message) {
        if (code != 0) {
            responseCallback(NO, message);
        }
    }];
    [self.requestRoomPKDic setObject:requesetPKID forKey:userID];
}

- (void)RoomPKWithRoomID:(UInt32)roomID userID:(NSString *)userID responseCallback:(ResponseCallback)responseCallback {
    
}

- (void)responseRoomPKWithUserID:(NSString *)userID agree:(BOOL)agree reason:(NSString *)reason {
    TRTCLiveUserInfo *me = [self checkUserLogIned:nil];
    if (!me) {
        return;
    }
    NSString *roomID = [self checkRoomJoined:nil];
    if (!roomID) {
        return;
    }
    if (![self checkIsOwner:nil]) {
        return;
    }
    NSString *streamId = [self checkIsPublishing:nil];
    if (!streamId) {
        return;
    }
    if ([TRTCCloud sharedInstance].delegate != self) {
        [TRTCCloud sharedInstance].delegate = self;
    }
    if ([self.pkAnchorInfo.userId isEqualToString:userID]) {
        self.pkAnchorInfo.isResponsed = YES;
        if (agree) {
            NSString *uuid = self.pkAnchorInfo.uuid;
            @weakify(self)
            dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(trtcLiveCheckStatusTimeOut * NSEC_PER_SEC)), dispatch_get_main_queue(), ^{
                @strongify(self)
                if (!self) {
                    return;
                }
                if (self.status != TRTCLiveRoomLiveStatusRoomPK && [uuid isEqualToString:self.pkAnchorInfo.uuid]) {
                    [self quitRoomPK:nil];
                    [self clearPKState];
                }
            });
        } else {
            [self clearPKState];
        }
    }
    NSString *inviteID = [self.responseRoomPKDic objectForKey:userID];
    if (inviteID) {
        [TRTCLiveRoomIMAction responseRoomPKWithRequestID:inviteID agreed:agree reason:reason streamID:streamId callback:nil];
    }
}

- (void)responseQuitRoomPK:(NSString *)requestID {
    [TRTCLiveRoomIMAction respondQuitRoomPK:requestID agree:YES message:liveRoomLocalize(@"Demo.TRTC.LiveRoom.endPK")];
}

- (void)quitRoomPK:(Callback)callback {
    TRTCLiveUserInfo *me = [self checkUserLogIned:callback];
    if (!me) {
        return;
    }
    NSString *roomID = [self checkRoomJoined:callback];
    if (!roomID) {
        return;
    }
    if (self.status == TRTCLiveRoomLiveStatusRoomPK && self.memberManager.pkAnchor) {
        [self.pkAnchorInfo reset];
        self.status = TRTCLiveRoomLiveStatusSingle;
        [[TRTCCloud sharedInstance] disconnectOtherRoom];
        [self.memberManager removeAnchor:self.memberManager.pkAnchor.userId];
        [TRTCLiveRoomIMAction quitRoomPKWithUserID:self.memberManager.pkAnchor.userId callback:callback];
    } else {
        if (callback) {
            callback(-1, liveRoomLocalize(@"Demo.TRTC.LiveRoom.isnotpkstate"));
        }
    }
}

#pragma mark - 音频设置
- (void)switchCamera {
    [[TRTCCloud sharedInstance] switchCamera];
}

- (void)setMirror:(BOOL)isMirror {
    [[TRTCCloud sharedInstance] setVideoEncoderMirror:isMirror];
}

- (void)muteLocalAudio:(BOOL)isMuted {
    [[TRTCCloud sharedInstance] muteLocalAudio:isMuted];
}

- (void)muteRemoteAudioWithUserID:(NSString *)userID isMuted:(BOOL)isMuted {
    [[TRTCCloud sharedInstance] muteRemoteAudio:userID mute:isMuted];
}

- (void)muteAllRemoteAudio:(BOOL)isMuted {
    [[TRTCCloud sharedInstance] muteAllRemoteAudio:isMuted];
}

- (void)setAudioQuality:(NSInteger)quality {
    if (quality == 3) {
        [[TRTCCloud sharedInstance] setAudioQuality:TRTCAudioQualityMusic];
    } else if (quality == 2) {
        [[TRTCCloud sharedInstance] setAudioQuality:TRTCAudioQualityDefault];
    } else {
        [[TRTCCloud sharedInstance] setAudioQuality:TRTCAudioQualitySpeech];
    }
}

- (void)setVideoResolution:(TRTCVideoResolution)resolution {
    _videoResolution = resolution;
    [self setVideoEncoderParamInternal];
}

- (void)setVideoFps:(int)fps {
    _videoFPS = fps;
    [self setVideoEncoderParamInternal];
}

- (void)setVideoBitrate:(int)bitrate {
    _videoBitrate = bitrate;
    [self setVideoEncoderParamInternal];
}

- (void)setLocalViewMirror:(TRTCLocalVideoMirrorType)type{
    [[TRTCCloud sharedInstance] setLocalViewMirror:type];
}


- (void)showVideoDebugLog:(BOOL)isShow {
    [[TRTCCloud sharedInstance]setDebugViewMargin:self.ownerId margin:UIEdgeInsetsMake(100, 10, 30, 10)];
    [[TRTCCloud sharedInstance] showDebugView:isShow ? 2 : 0];
}

#pragma mark - Send msg
- (void)sendRoomTextMsg:(NSString *)message callback:(Callback)callback {
    TRTCLiveUserInfo *me = [self checkUserLogIned:callback];
    if (!me) {
        return;
    }
    NSString *roomID = [self checkRoomJoined:callback];
    if (!roomID) {
        return;
    }
    [TRTCLiveRoomIMAction sendRoomTextMsgWithRoomID:roomID message:message callback:callback];
}

- (void)sendRoomCustomMsgWithCommand:(NSString *)cmd message:(NSString *)message callback:(Callback)callback {
    TRTCLiveUserInfo *me = [self checkUserLogIned:callback];
    if (!me) {
        return;
    }
    NSString *roomID = [self checkRoomJoined:callback];
    if (!roomID) {
        return;
    }
    [TRTCLiveRoomIMAction sendRoomCustomMsgWithRoomID:roomID command:cmd message:message callback:callback];
}

- (TXBeautyManager *)getBeautyManager {
    return self.beautyManager;
}

- (TXAudioEffectManager *)getAudioEffectManager {
    return [[TRTCCloud sharedInstance] getAudioEffectManager];
}

#pragma mark - private method
- (BOOL)canDelegateResponseMethod:(SEL)method {
    return self.delegate && [self.delegate respondsToSelector:method];
}

#pragma mark - TRTCCloudDelegate
- (void)onError:(TXLiteAVError)errCode errMsg:(NSString *)errMsg extInfo:(NSDictionary *)extInfo {
    if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onError:message:)]) {
        [self.delegate trtcLiveRoom:self onError:errCode message:errMsg];
    }
}

- (void)onWarning:(TXLiteAVWarning)warningCode warningMsg:(NSString *)warningMsg extInfo:(NSDictionary *)extInfo {
    if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onWarning:message:)]) {
        [self.delegate trtcLiveRoom:self onWarning:warningCode message:warningMsg];
    }
}

- (void)onEnterRoom:(NSInteger)result {
    [self logApi:@"onEnterRoom", nil];
    TRTCLog(@"on enter trtc room. result:%ld", (long)result);
    if (result > 0) {
        if (self.enterRoomCallback) {
            self.enterRoomCallback(0, @"enter trtc room success.");
        }
    } else {
        NSString *errorMsg = (result == ERR_TRTC_USER_SIG_CHECK_FAILED ? @"userSig invalid, please login again.":@"enter trtc room fail.");
        if (self.enterRoomCallback) {
            self.enterRoomCallback((int)result, errorMsg);
        }
    }
    self.enterRoomCallback = nil;
}

- (void)onRemoteUserEnterRoom:(NSString *)userId {
    if (self.shouldPlayCdn) {
        return;
    }
    if ([self.joinAnchorInfo.userId isEqualToString:userId]) {
        [self clearJoinState:NO userID:nil];
    }
    if ([self.trtcAction isUserPlaying:userId]) {
        [self.trtcAction startTRTCPlay:userId];
        return;
    }
    if (self.isOwner) {
        if ([self.memberManager.pkAnchor.userId isEqualToString:userId]) {
            self.status = TRTCLiveRoomLiveStatusRoomPK;
            [self.memberManager confirmPKAnchor:userId];
        } else if (!self.memberManager.anchors[userId]) {
            self.status = TRTCLiveRoomLiveStatusLinkMic;
            [self addTempAnchor:userId];
        }
        if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onAnchorEnter:)]) {
            [self.delegate trtcLiveRoom:self onAnchorEnter:userId];
        }
        [self.trtcAction updateMixingParams:self.shouldMixStream isRoomPK:self.status == TRTCLiveRoomLiveStatusRoomPK];
    } else {
        if (!self.memberManager.anchors[userId]) {
            [self addTempAnchor:userId];
        }
        if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onAnchorEnter:)]) {
            [self.delegate trtcLiveRoom:self onAnchorEnter:userId];
        }
    }
}

- (void)onRemoteUserLeaveRoom:(NSString *)userId reason:(NSInteger)reason {
    [self logApi:@"onremoteUserLeaveRoom", userId, nil];
    if (self.shouldPlayCdn) {
        return;
    }
    if (self.isOwner) {
        if (self.memberManager.anchors[userId] && self.memberManager.anchors.count <= 2) {
            if (self.pkAnchorInfo.userId && self.pkAnchorInfo.roomId) {
                if ([self canDelegateResponseMethod:@selector(trtcLiveRoomOnQuitRoomPK:)]) {
                    [self.delegate trtcLiveRoomOnQuitRoomPK:self];
                }
            }
            [self clearPKState];
            [self clearJoinState:YES userID:userId];
            self.status = TRTCLiveRoomLiveStatusSingle;
        }
        [self.memberManager removeAnchor:userId];
    }
    
    if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onAnchorExit:)]) {
        [self.delegate trtcLiveRoom:self onAnchorExit:userId];
    }
    if (self.isOwner) {
        [self.trtcAction updateMixingParams:self.shouldMixStream isRoomPK:self.status == TRTCLiveRoomLiveStatusRoomPK];
    }
}

- (void)onFirstVideoFrame:(NSString *)userId streamType:(TRTCVideoStreamType)streamType width:(int)width height:(int)height {
    [self.trtcAction onFirstVideoFrame:userId];
}

#pragma mark - TRTCLiveRoomMembermanagerDelegate
- (void)memberManager:(TRTCLiveRoomMemberManager *)manager onUserEnter:(TRTCLiveUserInfo *)user isAnchor:(BOOL)isAnchor {
    if (isAnchor) {
        if (self.configCdn && [self canDelegateResponseMethod:@selector(trtcLiveRoom:onAnchorEnter:)]) {
            [self.delegate trtcLiveRoom:self onAnchorEnter:user.userId];
        }
    } else {
        if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onAudienceEnter:)]) {
            [self.delegate trtcLiveRoom:self onAudienceEnter:user];
        }
    }
}

- (void)memberManager:(TRTCLiveRoomMemberManager *)manager onUserLeave:(TRTCLiveUserInfo *)user isAnchor:(BOOL)isAnchor {
    if (isAnchor) {
        if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onAnchorExit:)]) {
            [self.delegate trtcLiveRoom:self onAnchorExit:user.userId];
        }
    } else {
        if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onAudienceExit:)]) {
            [self.delegate trtcLiveRoom:self onAudienceExit:user];
        }
    }
}

- (void)memberManager:(TRTCLiveRoomMemberManager *)manager onChangeStreamId:(NSString *)streamID userId:(NSString *)userId {
    if (self.shouldPlayCdn) {
        return;
    }
    if (self.shouldMixStream && ![self.ownerId isEqualToString:userId]) {
        return;
    }
    if (streamID && ![streamID isEqualToString:@""]) {
        if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onAnchorEnter:)]) {
            [self.delegate trtcLiveRoom:self onAnchorEnter:userId];
        }
    } else {
        if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onAnchorExit:)]) {
            [self.delegate trtcLiveRoom:self onAnchorExit:userId];
        }
    }
}

- (void)memberManager:(TRTCLiveRoomMemberManager *)manager onChangeAnchorList:(NSArray<NSDictionary<NSString *,id> *> *)anchorList {
    if (!self.isOwner) {
        return;
    }
    NSString *roomID = [self checkRoomJoined:nil];
    if (!roomID) {
        return;
    }
    NSDictionary *data = @{
        @"type": @(self.status),
        @"list": anchorList,
    };
    [TRTCLiveRoomIMAction updateGroupInfoWithRoomID:roomID groupInfo:data callback:^(int code, NSString * _Nonnull message) {
        if (code != 0) {
            NSLog(@"TUILiveKit# update IM %@ error：%d,%@", roomID, code, message);
        }
    }];
}

#pragma mark - V2TIMSignalingListener
- (void)onReceiveNewInvitation:(NSString *)inviteID inviter:(NSString *)inviter groupID:(NSString *)groupID inviteeList:(NSArray<NSString *> *)inviteeList data:(NSString *)data {
    NSDictionary *dic = [self checkInviteData:data];
    if (!dic) {
        return;
    }
    NSString *cmd = [[dic objectForKey:@"data"] objectForKey:@"cmd"];
    if ([cmd isEqualToString:Signal_RequestJoinAnchor]) {
        [self.requestJoinAnchorDic setObject:inviteID forKey:inviter];
        TRTCLiveUserInfo *audience = nil;
        for (TRTCLiveUserInfo *obj in self.memberManager.audience) {
            NSLog(@"==== userid: %@", obj.userId);
            if ([inviter isEqualToString:obj.userId]) {
                audience = obj;
                break;
            }
        }
        if (audience) {
            [self handleJoinAnchorRequestFromUser:audience reason:dic[@"reason"] ?: @""];
        } else {
            NSLog(@"get info error");
        }
    } else if([cmd isEqualToString:Signal_KickoutJoinAnchor]) {
        if ([self canDelegateResponseMethod:@selector(trtcLiveRoomOnKickoutJoinAnchor:)]) {
            [self.delegate trtcLiveRoomOnKickoutJoinAnchor:self];
        }
        [self switchRoleOnLinkMic:NO];
        [self responseKickoutJoinAnchorWithRequestID:inviteID];
    } else if ([cmd isEqualToString:Signal_RequestRoomPK]) {
        [self.responseRoomPKDic setObject:inviteID forKey:inviter];
        NSString *roomId = dic[@"data"][@"roomId"];
        NSString *streamId =dic[@"data"][@"streamId"];
        if (roomId) {
            @weakify(self);
            [[V2TIMManager sharedInstance] getUsersInfo:@[inviter] succ:^(NSArray<V2TIMUserFullInfo *> *infoList) {
                @strongify(self);
                V2TIMUserFullInfo *info = infoList.firstObject;
                TRTCLiveUserInfo *userInfo = [[TRTCLiveUserInfo alloc] init];
                userInfo.userName = info.nickName;
                userInfo.userId = info.userID;
                userInfo.avatarURL = info.faceURL;
                [self handleRoomPKRequestFromUser:userInfo roomId:roomId streamId:streamId];
            } fail:^(int code, NSString *desc) {
                @strongify(self);
                if (self.delegate && [self.delegate respondsToSelector:@selector(onError:errMsg:extInfo:)]) {
                    [self.delegate trtcLiveRoom:self onError:code message:desc];
                }
            }];
        }
    } else if([cmd isEqualToString:Signal_QuitRoomPK]) {
        self.status = TRTCLiveRoomLiveStatusSingle;
        TRTCLiveUserInfo *pkAnchor = self.memberManager.pkAnchor;
        if (pkAnchor) {
            [self.memberManager removeAnchor:pkAnchor.userId];
        }
        if (self.pkAnchorInfo.userId && self.pkAnchorInfo.roomId) {
            if ([self canDelegateResponseMethod:@selector(trtcLiveRoomOnQuitRoomPK:)]) {
                [self.delegate trtcLiveRoomOnQuitRoomPK:self];
            }
        }
        [self clearPKState];
        [self responseQuitRoomPK:inviteID];
    }
}

- (void)onInviteeAccepted:(NSString *)inviteID invitee:(NSString *)invitee data:(NSString *)data {
    NSDictionary *dic = [self checkInviteData:data];
    if (!dic) {
        return;
    }
    NSString *cmd = [[dic objectForKey:@"data"]objectForKey:@"cmd"];
    if ([cmd isEqualToString:Signal_RequestJoinAnchor]) {
        if (![self.requestJoinAnchorID isEqualToString:inviteID]) {
            return;
        }
        self.requestJoinAnchorID = nil;
        [self switchRoleOnLinkMic:YES];
        NSString *uuid = self.joinAnchorInfo.uuid;
        @weakify(self)
        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(trtcLiveCheckStatusTimeOut * NSEC_PER_SEC)), dispatch_get_main_queue(), ^{
            @strongify(self)
            if (!self) {
                return;
            }
            if (self.memberManager.anchors[invitee] == nil && [uuid isEqualToString:self.joinAnchorInfo.uuid]) {
                [self kickoutJoinAnchor:invitee callback:nil];
                [self clearJoinState];
            } else {
                [self clearJoinState:NO userID:nil];
            }
        });
        NSString *reason = dic[@"reason"] ?: @"";
        if (self.requestJoinAnchorCallback) {
            self.requestJoinAnchorCallback(YES, reason);
            self.requestJoinAnchorCallback = nil;
        }
        
    } else if ([cmd isEqualToString:Signal_RequestRoomPK]) {
        if (![self.requestRoomPKDic.allValues containsObject:inviteID]) {
            return;
        }
        if (self.status == TRTCLiveRoomLiveStatusRoomPK) {
            return;
        }
        [self.requestRoomPKDic removeObjectForKey:invitee];
        NSString *streamId = dic[@"data"][@"stream_id"];
        if (self.requestRoomPKCallback) {
            [[V2TIMManager sharedInstance] getUsersInfo:@[invitee] succ:^(NSArray<V2TIMUserFullInfo *> *infoList) {
                V2TIMUserFullInfo *info = infoList.firstObject;
                TRTCLiveUserInfo *userInfo = [[TRTCLiveUserInfo alloc] init];
                userInfo.userName = info.nickName;
                userInfo.userId = info.userID;
                userInfo.avatarURL = info.faceURL;
                self.status = TRTCLiveRoomLiveStatusRoomPK;
                [self startRoomPKWithUser:userInfo streamId:streamId];
            } fail:^(int code, NSString *desc) {
                
            }];
            NSString *uuid = self.pkAnchorInfo.uuid;
            @weakify(self)
            dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)((trtcLiveCheckStatusTimeOut + 2) * NSEC_PER_SEC)), dispatch_get_main_queue(), ^{
                @strongify(self)
                if (!self) {
                    return;
                }
                if (self.status != TRTCLiveRoomLiveStatusRoomPK && [uuid isEqualToString:self.pkAnchorInfo.uuid]) {
                    [self quitRoomPK:nil];
                    [self clearPKState];
                }
            });
            NSString *reason = dic[@"reason"] ?: @"";
            if (self.requestRoomPKCallback) {
                self.requestRoomPKCallback(YES, reason);
                self.requestRoomPKCallback = nil;
            }
        }
    }
}

- (void)onInviteeRejected:(NSString *)inviteID invitee:(NSString *)invitee data:(NSString *)data {
    NSDictionary *dic = [self checkInviteData:data];
    if (!dic) {
        return;
    }
    NSString *cmd = [[dic objectForKey:@"data"]objectForKey:@"cmd"];
    if ([cmd isEqualToString:Signal_RequestJoinAnchor]) {
        if (![self.requestJoinAnchorID isEqualToString:inviteID]) {
            return;
        }
        self.requestJoinAnchorID = nil;
        [self clearJoinState];
        NSString *reason = dic[@"reason"] ?: @"";
        if (self.requestJoinAnchorCallback) {
            self.requestJoinAnchorCallback(NO, reason);
            self.requestJoinAnchorCallback = nil;
        }
    } else if ([cmd isEqualToString:Signal_RequestRoomPK]){
        if (![self.requestRoomPKDic.allValues containsObject:inviteID]) {
            return;
        }
        [self.requestRoomPKDic removeObjectForKey:invitee];
        [self clearPKState];
        if (self.requestRoomPKCallback) {
            NSString *reason = dic[@"reason"] ?: @"";
            if (self.requestRoomPKCallback) {
                self.requestRoomPKCallback(NO, reason);
                self.requestRoomPKCallback = nil;
            }
        }
    }
}

- (void)onInvitationCancelled:(NSString *)inviteID inviter:(NSString *)inviter data:(NSString *)data {
    NSDictionary *dic = [self checkInviteData:data];
    if (!dic) {
        return;
    }
    NSString *cmd = [[dic objectForKey:@"data"]objectForKey:@"cmd"];
    if ([cmd isEqualToString:Signal_RequestJoinAnchor]) {
        if (![self.requestJoinAnchorDic.allValues containsObject:inviteID]) {
            return;
        }
        [self.requestJoinAnchorDic removeObjectForKey:inviter];
        if (self.delegate && [self.delegate respondsToSelector:@selector(trtcLiveRoom:onCancelJoinAnchor:reason:)]) {
            TRTCLiveUserInfo *audience = nil;
            for (TRTCLiveUserInfo *obj in self.memberManager.audience) {
                NSLog(@"==== userid: %@", obj.userId);
                if ([inviter isEqualToString:obj.userId]) {
                    audience = obj;
                    break;
                }
            }
            if (audience) {
                [self.delegate trtcLiveRoom:self onCancelJoinAnchor:audience reason:@"cancel link-mic request"];
            }
            
        }
    } else if ([cmd isEqualToString:Signal_RequestRoomPK]){
        if (![self.responseRoomPKDic.allValues containsObject:inviteID]) {
            return;
        }
        [self.responseRoomPKDic removeObjectForKey:inviter];
        [self clearPKState];
        @weakify(self);
        [[V2TIMManager sharedInstance] getUsersInfo:@[inviter] succ:^(NSArray<V2TIMUserFullInfo *> *infoList) {
            @strongify(self);
            V2TIMUserFullInfo *info = infoList.firstObject;
            TRTCLiveUserInfo *userInfo = [[TRTCLiveUserInfo alloc] init];
            userInfo.userName = info.nickName;
            userInfo.userId = info.userID;
            userInfo.avatarURL = info.faceURL;
            if (self.delegate && [self.delegate respondsToSelector:@selector(trtcLiveRoom:onCancelRoomPK:)]) {
                [self.delegate trtcLiveRoom:self onCancelRoomPK:userInfo];
            }
        } fail:^(int code, NSString *desc) {
            @strongify(self);
            if (self.delegate && [self.delegate respondsToSelector:@selector(onError:errMsg:extInfo:)]) {
                [self.delegate trtcLiveRoom:self onError:code message:desc];
            }
        }];
    }
}

- (void)onInvitationTimeout:(NSString *)inviteID inviteeList:(NSArray<NSString *> *)inviteeList {
    if ([self.requestJoinAnchorID isEqualToString:inviteID]) {
        if (self.requestJoinAnchorCallback) {
            self.requestJoinAnchorCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.anchornotresponsethereq"));
            self.requestJoinAnchorCallback = nil;
            [self clearJoinState];
        }
    } else if ([self.requestJoinAnchorDic.allValues containsObject:inviteID]) {
        [self.requestJoinAnchorDic enumerateKeysAndObjectsUsingBlock:^(NSString * _Nonnull key, NSString * _Nonnull obj, BOOL * _Nonnull stop) {
            if ([obj isEqualToString:inviteID]) {
                if (self.delegate && [self.delegate respondsToSelector:@selector(trtcLiveRoom:audienceRequestJoinAnchorTimeout:)]) {
                    [self.delegate trtcLiveRoom:self audienceRequestJoinAnchorTimeout:key];
                }
                *stop = YES;
                [self.requestJoinAnchorDic removeObjectForKey:key];
                [self clearJoinState:YES userID:key];
            }
        }];
    } else if ([self.requestRoomPKDic.allValues containsObject:inviteID]) {
        if (self.requestRoomPKCallback) {
            self.requestRoomPKCallback(NO, liveRoomLocalize(@"Demo.TRTC.LiveRoom.anchornotresponsepkbetweenroom"));
            self.requestRoomPKCallback = nil;
            [self clearPKState];
        }
    } else if ([self.responseRoomPKDic.allValues containsObject:inviteID]) {
        if (self.delegate && [self.delegate respondsToSelector:@selector(trtcLiveRoom:anchorRequestRoomPKTimeout:)]) {
            [self.responseRoomPKDic enumerateKeysAndObjectsUsingBlock:^(NSString * _Nonnull key, NSString * _Nonnull obj, BOOL * _Nonnull stop) {
                if ([obj isEqualToString:inviteID]) {
                    [self.delegate trtcLiveRoom:self anchorRequestRoomPKTimeout:key];
                    *stop = YES;
                    [self.responseRoomPKDic removeObjectForKey:key];
                }
            }];
        }
        [self clearPKState];
    }
}

- (NSDictionary * _Nullable)checkInviteData:(NSString *)data {
    if (!data) {
        return nil;
    }
    NSData *jsonData = [data dataUsingEncoding:NSUTF8StringEncoding];
    NSError *error;
    NSDictionary *dic = [NSJSONSerialization JSONObjectWithData:jsonData options:NSJSONReadingMutableContainers error:&error];
    if (error) {
        NSLog(@"decode info error:%@", error);
        return nil;
    }
    
    NSString *businessID = [dic objectForKey:Signal_Business_ID];
    if (![businessID isEqualToString:Signal_Business_Live]) {
        NSLog(@"Non-LiveRoom signaling messages, do not respond");
        return nil;
    }
    NSString *versionString = [dic objectForKey:@"version"];
    if (![versionString isEqualToString:gTrtcLiveRoomProtocolVersion]) {
        NSLog(@"Message version numbers do not match");
    }
    return dic;
}

#pragma mark - V2TIMAdvancedMsgListener
- (void)onRecvNewMessage:(V2TIMMessage *)msg {
    if (msg.elemType == V2TIM_ELEM_TYPE_CUSTOM) {
        V2TIMCustomElem *elem = msg.customElem;
        NSData *data = elem.data;
        NSError *error;
        NSDictionary *json = [NSJSONSerialization JSONObjectWithData:data options:NSJSONReadingAllowFragments error:&error];
        if (error) {
            return;
        }
        NSNumber* action = json[@"action"] ?: @(0);
        id version = json[@"version"] ?: @"";
        BOOL isString = [version isKindOfClass:[NSString class]];
        if (isString && ![version isEqualToString:gTrtcLiveRoomProtocolVersion]) {
            
        }
        [self handleActionMessage:[action intValue] elem:elem message:msg json:json];
    } else if (msg.elemType == V2TIM_ELEM_TYPE_TEXT) {
        if (msg.textElem) {
            [self handleActionMessage:TRTCLiveRoomIMActionTypeRoomTextMsg elem:msg.textElem message:msg json:@{}];
        }
    }
}

- (TRTCLiveUserInfo *)liveUserInfoWithMessage:(V2TIMMessage *)message {
    TRTCLiveUserInfo *liveUser = [[TRTCLiveUserInfo alloc] init];
    liveUser.userId = message.sender ?: @"";
    liveUser.userName = message.nickName ?: @"";
    liveUser.avatarURL = message.faceURL ?: @"";
    return liveUser;
}

- (void)handleActionMessage:(TRTCLiveRoomIMActionType)action elem:(V2TIMElem *)elem message:(V2TIMMessage *)message json:(NSDictionary<NSString *, id> *)json {
    NSDate* sendTime = message.timestamp;
    if (sendTime && sendTime.timeIntervalSinceNow < -10) {
        return;
    }
    NSString *userID = message.sender;
    if (!userID) {
        return;
    }
    TRTCLiveUserInfo *liveUser = [self liveUserInfoWithMessage:message];
    if (!self.memberManager.anchors[liveUser.userId]) {
        if (action != TRTCLiveRoomIMActionTypeRespondRoomPK &&
            action != TRTCLiveRoomIMActionTypeRequestRoomPK &&
            action != TRTCLiveRoomIMActionTypeQuitRoomPK) {
            [self.memberManager addAudience:liveUser];
        }
    }
    
    switch (action) {
        case TRTCLiveRoomIMActionTypeNotifyJoinAnchorStream:
        {
            NSString *streamId = json[@"stream_id"];
            if (streamId) {
                [self startLinkMic:userID streamId:streamId];
            }
        }
            break;
        case TRTCLiveRoomIMActionTypeRoomTextMsg:
        {
            V2TIMTextElem *textElem = (V2TIMTextElem *)elem;
            NSString *text = textElem.text;
            if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onRecvRoomTextMsg:fromUser:)]) {
                [self.delegate trtcLiveRoom:self onRecvRoomTextMsg:text fromUser:liveUser];
            }
        }
            break;
        case TRTCLiveRoomIMActionTypeRoomCustomMsg:
        {
            NSString *command = json[@"command"];
            NSString *message = json[@"message"];
            if (command && message) {
                if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onRecvRoomCustomMsgWithCommand:message:fromUser:)]) {
                    [self.delegate trtcLiveRoom:self onRecvRoomCustomMsgWithCommand:command message:message fromUser:liveUser];
                }
            }
        }
            break;
        case TRTCLiveRoomIMActionTypeUpdateGroupInfo:
        {
            [self.memberManager updateAnchorsWithGroupinfo:json];
            NSNumber *roomStatus = json[@"type"];
            if (roomStatus) {
                self.status = [roomStatus intValue];
            }
        }
            break;
        case TRTCLiveRoomIMActionTypeUnknown:
            NSLog(@"%@", liveRoomLocalize(@"Demo.TRTC.LiveRoom.receiveothermessage"));
            break;
        default:
            TRTCLog(@"!!!!!!!! unknow message type");
            break;
    }
}

#pragma mark - V2TIMGroupListener
- (void)onMemberInvited:(NSString *)groupID opUser:(V2TIMGroupMemberInfo *)opUser memberList:(NSArray<V2TIMGroupMemberInfo *> *)memberList {
    for (V2TIMGroupMemberInfo *member in memberList) {
        TRTCLiveUserInfo *user = [[TRTCLiveUserInfo alloc] init];
        user.userId = member.userID;
        user.userName = member.nickName ?: @"";
        user.avatarURL = member.faceURL ?: @"";
        [self.memberManager addAudience:user];
    }
}

- (void)onMemberLeave:(NSString *)groupID member:(V2TIMGroupMemberInfo *)member {
    [self.memberManager removeMember:member.userID];
}

- (void)onMemberEnter:(NSString *)groupID memberList:(NSArray<V2TIMGroupMemberInfo *> *)memberList {
    for (V2TIMGroupMemberInfo *member in memberList) {
        TRTCLiveUserInfo *user = [[TRTCLiveUserInfo alloc] init];
        user.userId = member.userID;
        user.userName = member.nickName ?: @"";
        user.avatarURL = member.faceURL ?: @"";
        [self.memberManager addAudience:user];
    }
}

- (void)onGroupInfoChanged:(NSString *)groupID changeInfoList:(NSArray<V2TIMGroupChangeInfo *> *)changeInfoList {
    __block V2TIMGroupChangeInfo *info = nil;
    [changeInfoList enumerateObjectsUsingBlock:^(V2TIMGroupChangeInfo * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
        if (obj.type == V2TIM_GROUP_INFO_CHANGE_TYPE_INTRODUCTION) {
            info = obj;
            *stop = YES;
        }
    }];
    if (info) {
        NSDictionary *customInfo = [info.value mj_JSONObject];
        NSNumber *roomStatus = customInfo[@"type"];
        self.status = [roomStatus intValue];
    }
}

- (void)onGroupDismissed:(NSString *)groupID opUser:(V2TIMGroupMemberInfo *)opUser {
    [self unInitIMListener];
    [self handleRoomDismissed:YES];
}

- (void)onRevokeAdministrator:(NSString *)groupID opUser:(V2TIMGroupMemberInfo *)opUser memberList:(NSArray<V2TIMGroupMemberInfo *> *)memberList {
    [self handleRoomDismissed:YES];
}


#pragma mark - Actions
- (void)notifyAvailableStreams {
    if (self.shouldMixStream) {
        if (self.ownerId && [self canDelegateResponseMethod:@selector(trtcLiveRoom:onAnchorEnter:)]) {
            [self.delegate trtcLiveRoom:self onAnchorEnter:self.ownerId];
        }
    } else {
        [self.memberManager.anchors.allKeys enumerateObjectsUsingBlock:^(NSString * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
            if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onAnchorEnter:)]) {
                [self.delegate trtcLiveRoom:self onAnchorEnter:obj];
            }
        }];
    }
}

- (void)handleRoomDismissed:(BOOL)isOwnerDeleted {
    NSString *roomID = [self checkRoomJoined:nil];
    if (!roomID) {
        return;
    }
    if (self.isOwner && !isOwnerDeleted) {
        [self destroyRoom:nil];
    } else {
        [self exitRoom:nil];
    }
    if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onRoomDestroy:)]) {
        [self.delegate trtcLiveRoom:self onRoomDestroy:roomID];
    }
}

- (void)handleJoinAnchorRequestFromUser:(TRTCLiveUserInfo *)user reason:(NSString *)reason {
    if (self.status == TRTCLiveRoomLiveStatusRoomPK || self.pkAnchorInfo.userId != nil) {
        [self responseJoinAnchor:user.userId agree:NO reason:liveRoomLocalize(@"Demo.TRTC.LiveRoom.anchorispkbetweenroom")];
        return;
    }
    if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onRequestJoinAnchor:reason:)]) {
        [self.delegate trtcLiveRoom:self onRequestJoinAnchor:user reason:reason];
    }
}

- (void)startLinkMic:(NSString *)userId streamId:(NSString *)streamId {
    self.status = TRTCLiveRoomLiveStatusLinkMic;
    [self.memberManager switchMember:userId toAnchor:YES streamId:streamId];
}

- (void)stopLinkMic:(NSString *)userId {
    self.status = self.memberManager.anchors.count <= 2 ? TRTCLiveRoomLiveStatusSingle : TRTCLiveRoomLiveStatusLinkMic;
    [self.memberManager switchMember:userId toAnchor:NO streamId:nil];
    [self.onJoinAnchorDic removeObjectForKey:userId];
}

- (void)switchRoleOnLinkMic:(BOOL)isLinkMic {
    [self switchRoleOnLinkMic:isLinkMic needDelayPlay:NO];
}

- (void)switchRoleOnLinkMic:(BOOL)isLinkMic needDelayPlay:(BOOL)needDelayPlay {
    TRTCLiveUserInfo *me = [self checkUserLogIned:nil];
    if (!me) {
        return;
    }
    [self.memberManager switchMember:me.userId toAnchor:isLinkMic streamId:nil];
    if (self.configCdn) {
        if (needDelayPlay) {
            dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(1 * NSEC_PER_SEC)), dispatch_get_main_queue(), ^{
                [self.trtcAction togglePlay:!isLinkMic];
            });
        } else {
            [self.trtcAction togglePlay:!isLinkMic];
        }
    }
    if (!isLinkMic) {
        [self.trtcAction switchRole:TRTCRoleAudience];
    }
}

- (void)handleRoomPKRequestFromUser:(TRTCLiveUserInfo *)user roomId:(NSString *)roomId streamId:(NSString *)streamId {
    if (self.status == TRTCLiveRoomLiveStatusLinkMic || self.onJoinAnchorDic.count > 0) {
        [self responseRoomPKWithUserID:user.userId agree:NO reason:liveRoomLocalize(@"Demo.TRTC.LiveRoom.anchorismicconnecting")];
        return;
    }
    if ((self.pkAnchorInfo.userId != nil && ![self.pkAnchorInfo.roomId isEqualToString:roomId]) || self.status == TRTCLiveRoomLiveStatusRoomPK) {
        [self responseRoomPKWithUserID:user.userId agree:NO reason:liveRoomLocalize(@"Demo.TRTC.LiveRoom.anchorispking")];
        return;
    }
    if ([self.pkAnchorInfo.userId isEqualToString:user.userId]) {
        return;
    }
    self.pkAnchorInfo.userId = user.userId;
    self.pkAnchorInfo.roomId = roomId;
    self.pkAnchorInfo.uuid = [[NSUUID UUID] UUIDString];

    [self prepareRoomPKWithUser:user streamId:streamId];
    if ([self canDelegateResponseMethod:@selector(trtcLiveRoom:onRequestRoomPK:)]) {
        [self.delegate trtcLiveRoom:self onRequestRoomPK:user];
    }
}

- (void)unInitIMListener {
    [[V2TIMManager sharedInstance] removeGroupListener:self];
    [[V2TIMManager sharedInstance] removeSignalingListener:self];
    [[V2TIMManager sharedInstance] removeAdvancedMsgListener:self];
}

- (void)initIMListener {
    [[V2TIMManager sharedInstance] addAdvancedMsgListener:self];
    [[V2TIMManager sharedInstance] addSignalingListener:self];
    [[V2TIMManager sharedInstance] addGroupListener:self];
}

- (void)clearPKState {
    [self.pkAnchorInfo reset];
    [self.memberManager removePKAnchor];
}

- (void)clearJoinState {
    [self clearJoinState:YES userID:nil];
}

- (void)clearJoinState:(BOOL)shouldRemove userID:(NSString *)userID {
    if (shouldRemove && self.joinAnchorInfo.userId) {
        [self.memberManager removeAnchor:self.joinAnchorInfo.userId];
    }
    [self.joinAnchorInfo reset];
    if (userID) {
        [self.onJoinAnchorDic removeObjectForKey:userID];
    }
}

- (void)addTempAnchor:(NSString *)userId {
    TRTCLiveUserInfo *user = [[TRTCLiveUserInfo alloc] init];
    user.userId = userId;
    [self.memberManager addAnchor:user];
}

- (void)prepareRoomPKWithUser:(TRTCLiveUserInfo *)user streamId:(NSString *)streamId {
    user.streamId = streamId;
    [self.memberManager prepaerPKAnchor:user];
}

- (void)startRoomPKWithUser:(TRTCLiveUserInfo *)user streamId:(NSString *)streamId {
    NSString *roomId = self.pkAnchorInfo.roomId;
    if (!roomId) {
        return;
    }
    [self prepareRoomPKWithUser:user streamId:streamId];
    [self.trtcAction startRoomPK:roomId userId:user.userId];
}

#pragma mark - Beauty
- (void)setFilter:(UIImage *)image {
    [self.trtcAction setFilter:image];
}

- (void)setFilterConcentration:(float)concentration {
    [self.trtcAction setFilterConcentration:concentration];
}

- (void)setGreenScreenFile:(NSURL *)fileUrl {
    [self.trtcAction setGreenScreenFile:fileUrl];
}

#pragma mark - Utils
- (void)logApi:(NSString *)api, ... NS_REQUIRES_NIL_TERMINATION {
    
}

- (TRTCLiveUserInfo *)checkUserLogIned:(Callback)callback {
    if (!self.me) {
        if (callback) {
            callback(-1, liveRoomLocalize(@"Demo.TRTC.LiveRoom.notlogin"));
        }
        return nil;
    }
    return self.me;
}

- (NSString *)checkRoomJoined:(Callback)callback {
    if ([self.roomID length] == 0) {
        if (callback) {
            callback(-1, liveRoomLocalize(@"Demo.TRTC.LiveRoom.hasnotenterroom"));
        }
        return nil;
    }
    return self.roomID;
}

- (BOOL)checkRoomUnjoined:(Callback)callback {
    if ([self.roomID length] > 0) {
        if (callback) {
            callback(-1, liveRoomLocalize(@"Demo.TRTC.LiveRoom.isinroomnow"));
        }
        return NO;
    }
    return YES;
}

- (BOOL)checkIsOwner:(Callback)callback {
    if (!self.isOwner) {
        if (callback) {
            callback(-1, liveRoomLocalize(@"Demo.TRTC.LiveRoom.onlyanchorcanoperation"));
        }
        return NO;
    }
    return YES;
}

- (NSString *)checkIsPublishing:(Callback)callback {
    if (!self.me.streamId) {
        if (callback) {
             callback(-1, liveRoomLocalize(@"Demo.TRTC.LiveRoom.onlypushstreamcanoperate"));
        }
        return nil;
    }
    return self.me.streamId;
}

- (void)reset {
    self.enterRoomCallback = nil;
    [self.trtcAction exitRoom];
    [self.trtcAction stopAllPlay:self.shouldPlayCdn];
    self.trtcAction.roomId = nil;
    self.status = TRTCLiveRoomLiveStatusNone;
    [self clearPKState];
    [self clearJoinState];
    [self.onJoinAnchorDic removeAllObjects];
    [self.memberManager clearMembers];
    self.curRoomInfo = nil;
}

#pragma mark - private readOnly property
- (BOOL)isOwner {
    if (self.me.userId) {
        return [self.me.userId isEqualToString:self.memberManager.ownerId];
    }
    return NO;
}

- (BOOL)isAnchor {
    if (self.me.userId) {
        return self.memberManager.anchors[self.me.userId] != nil;
    }
    return false;
}

- (BOOL)configCdn {
    return self.config.useCDNFirst;
}

- (BOOL)shouldPlayCdn {
    return self.configCdn && !self.isAnchor;
}

- (BOOL)shouldMixStream {
    switch (self.status) {
        case TRTCLiveRoomLiveStatusNone:
            return NO;
        case TRTCLiveRoomLiveStatusSingle:
            return NO;
        case TRTCLiveRoomLiveStatusLinkMic:
            return self.mixingLinkMicStream;
        case TRTCLiveRoomLiveStatusRoomPK:
            return self.mixingPKStream;
        default:
            break;
    }
    return NO;
}

- (void)setVideoEncoderParamInternal {
    TRTCVideoEncParam *param = [[TRTCVideoEncParam alloc] init];
    param.videoResolution = _videoResolution;
    param.videoBitrate = _videoBitrate;
    param.videoFps = _videoFPS;
    param.resMode = TRTCVideoResolutionModePortrait;
    param.enableAdjustRes = YES;
    [[TRTCCloud sharedInstance] setVideoEncoderParam:param];
}

@end
