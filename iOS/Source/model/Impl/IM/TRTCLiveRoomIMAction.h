//
//  TRTCLiveRoomIMAction.h
//  TRTCVoiceRoomOCDemo
//
//  Created by abyyxwang on 2020/7/8.
//  Copyright © 2020 tencent. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <ImSDK_Plus/ImSDK_Plus.h>

NS_ASSUME_NONNULL_BEGIN

static NSString *trtcLiveRoomProtocolVersion = @"1.0.0";

typedef NS_ENUM(NSUInteger, TRTCLiveRoomIMActionType) {
    TRTCLiveRoomIMActionTypeUnknown = 0,
    TRTCLiveRoomIMActionTypeRequestJoinAnchor = 100,
    TRTCLiveRoomIMActionTypeRespondJoinAnchor,
    TRTCLiveRoomIMActionTypeKickoutJoinAnchor,
    TRTCLiveRoomIMActionTypeRespondKickoutJoinAnchor,
    TRTCLiveRoomIMActionTypeCancelRequestJoinAnchor,
    TRTCLiveRoomIMActionTypeNotifyJoinAnchorStream,
    
    TRTCLiveRoomIMActionTypeRequestRoomPK = 200,
    TRTCLiveRoomIMActionTypeRespondRoomPK,
    TRTCLiveRoomIMActionTypeQuitRoomPK,
    TRTCLiveRoomIMActionTypeRespondQuitRoomPK,
    TRTCLiveRoomIMActionTypeCancelRequestRoomPK,
    
    TRTCLiveRoomIMActionTypeRoomTextMsg = 300,
    TRTCLiveRoomIMActionTypeRoomCustomMsg,
    
    TRTCLiveRoomIMActionTypeUpdateGroupInfo = 400,
};

extern NSString * const Signal_RequestJoinAnchor;
extern NSString * const Signal_RequestRoomPK;
extern NSString * const Signal_KickoutJoinAnchor;
extern NSString * const Signal_QuitRoomPK;
extern NSString * const Signal_RespondQuitRoomPK;

@class TRTCLiveUserInfo;
@class TRTCLiveRoomInfo;
@class TRTCCreateRoomParam;
typedef void(^LRIMCallback)(int code, NSString* message);
typedef void(^LREnterRoomCallback)(NSArray<TRTCLiveUserInfo *> *members, NSDictionary<NSString *, id> * customInfo, TRTCLiveRoomInfo * _Nullable roomInfo);
typedef void(^LRMemberCallback)(NSArray<TRTCLiveUserInfo *> *members);
typedef void(^LRRoomInfosCallback)(NSArray<TRTCLiveRoomInfo *> *roomInfos);

@interface TRTCLiveRoomIMAction : NSObject

+ (BOOL)setupSDKWithSDKAppID:(int)sdkAppId userSig:(NSString *)userSig messageLister:(id<V2TIMAdvancedMsgListener, V2TIMGroupListener>)listener;

+ (void)releaseSdk;

+ (void)loginWithUserID:(NSString *)userID userSig:(NSString *)userSig callback:(LRIMCallback _Nullable)callback;

+ (void)logout:(LRIMCallback _Nullable)callback;

+ (void)setProfileWithName:(NSString *)name avatar:(NSString *)avatar callback:(LRIMCallback  _Nullable)callback;

+ (void)createRoomWithRoomID:(NSString *)roomID roomParam:(TRTCCreateRoomParam *)roomParam success:(LREnterRoomCallback _Nullable)success error:(LRIMCallback _Nullable)errorCallback;

+ (void)destroyRoomWithRoomID:(NSString *)roomID callback:(LRIMCallback _Nullable)callback;

+ (void)enterRoomWithRoomID:(NSString *)roomID success:(LREnterRoomCallback _Nullable)success error:(LRIMCallback _Nullable)errorCallback;

+ (void)exitRoomWithRoomID:(NSString *)roomID callback:(LRIMCallback _Nullable)callback;

+ (void)getRoomInfoWithRoomIds:(NSArray<NSString *> *)roomIds success:(LRRoomInfosCallback _Nullable)success error:(LRIMCallback _Nullable)error;

+ (void)getAllMembersWithRoomID:(NSString *)roomID success:(LRMemberCallback _Nullable)success error:(LRIMCallback _Nullable)error;

#pragma mark - Action Message
+ (void)notifyStreamToAnchorWithUserId:(NSString *)userID streamID:(NSString *)streamID callback:(LRIMCallback _Nullable)callback;

+ (void)sendRoomTextMsgWithRoomID:(NSString *)roomID message:(NSString *)message callback:(LRIMCallback _Nullable)callback;

+ (void)sendRoomCustomMsgWithRoomID:(NSString *)roomID command:(NSString *)command message:(NSString *)message callback:(LRIMCallback _Nullable)callback;

+ (void)updateGroupInfoWithRoomID:(NSString *)roomID groupInfo:(NSDictionary<NSString *, id> *)groupInfo callback:(LRIMCallback _Nullable)callback;


#pragma mark - 信令通道
/**
 *  和主播连麦增减timeout参数
 */
+ (NSString * _Nullable)requestJoinAnchorWithUserID:(NSString *)userID timeout:(int)timeout reason:(NSString *)reason callback:(LRIMCallback _Nullable)callback;
/**
 *  取消连麦
 */
+ (void)cancelRequestJoinAnchorWithRequestID:(NSString *)requestID reason:(NSString *)reason callback:(LRIMCallback)callback;
/**
 *  和主播连麦处理结果
 */
+ (void)respondJoinAnchorWithRequestID:(NSString *)requestID agreed:(BOOL)agreed reason:(NSString *)reason callback:(LRIMCallback _Nullable)callback;
/**
 * 被主播踢出房间
 */
+ (NSString *)kickoutJoinAnchorWithUserID:(NSString *)userID callback:(LRIMCallback _Nullable)callback;
/**
 *  踢出连麦观众回调
 */
+ (void)respondKickoutJoinAnchor:(NSString *)inviteID agree:(BOOL)agree message:(NSString *)message;
/**
 *  主播向其他主播请求PK
 */
+ (NSString *)requestRoomPKWithUserID:(NSString *)userID timeout:(int)timeout fromRoomID:(NSString *)fromRoomID fromStreamID:(NSString *)fromStreamID callback:(LRIMCallback _Nullable)callback;
/**
 *  主播取消PK请求
 */
+ (void)cancelRequestRoomPKWithRequestID:(NSString *)requestID reason:(NSString *)reason callback:(LRIMCallback)callback;
/**
 *  PK请求结果回调
 */
+ (void)responseRoomPKWithRequestID:(NSString *)requestID agreed:(BOOL)agreed reason:(NSString * _Nullable)reason streamID:(NSString *)streamID callback:(LRIMCallback _Nullable)callback;
/**
 *  退出PK请求
 */
+ (NSString *)quitRoomPKWithUserID:(NSString *)userID callback:(LRIMCallback _Nullable)callback;

/**
 *  退出PK请求回调
 */
+ (void)respondQuitRoomPK:(NSString *)inviteID agree:(BOOL)agree message:(NSString *)message;


@end

NS_ASSUME_NONNULL_END
