//
//  TRTCLiveRoomIMAction.m
//  TRTCVoiceRoomOCDemo
//
//  Created by abyyxwang on 2020/7/8.
//  Copyright © 2020 tencent. All rights reserved.
//

#import "TRTCLiveRoomIMAction.h"
#import "TXLiveRoomCommonDef.h"
#import <ImSDK_Plus/ImSDK_Plus.h>
#import "TRTCLiveRoomDef.h"
#import <MJExtension/MJExtension.h>
#import "LiveRoomLocalized.h"

typedef NS_ENUM(NSUInteger, ConvType) {
    ConvTypeUser,
    ConvTypeGroup,
};

NSString * const Signal_RequestJoinAnchor = @"requestJoinAnchor";
NSString * const Signal_RequestRoomPK = @"requestRoomPK";
NSString * const Signal_KickoutJoinAnchor = @"kickoutJoinAnchor";
NSString * const Signal_QuitRoomPK = @"quitRoomPK";
NSString * const Signal_CancelJoinAnchor = @"cancelJoinAnchor";

@interface ConversationParams : NSObject

@property (nonatomic, assign) ConvType type;
@property (nonatomic, copy) NSString *userID;
@property (nonatomic, copy) NSString *gourpID;
@property (nonatomic, copy, nullable) NSString *text;
@property (nonatomic, assign) V2TIMMessagePriority priority;

@end

@implementation ConversationParams

@end


@implementation TRTCLiveRoomIMAction

+ (BOOL)setupSDKWithSDKAppID:(int)sdkAppId userSig:(NSString *)userSig messageLister:(id<V2TIMAdvancedMsgListener,V2TIMGroupListener>)listener {
    BOOL reslut = [[V2TIMManager sharedInstance] initSDK:sdkAppId config:nil];
    if (reslut) {
        [[V2TIMManager sharedInstance] removeAdvancedMsgListener:listener];
        [[V2TIMManager sharedInstance] addAdvancedMsgListener:listener];
        [[V2TIMManager sharedInstance] addGroupListener:listener];
    }
    return reslut;
}

+ (void)releaseSdk {
    [[V2TIMManager sharedInstance] unInitSDK];
}

+ (void)loginWithUserID:(NSString *)userID userSig:(NSString *)userSig callback:(LRIMCallback)callback {
    if (!userID || [userID isEqualToString:@""]) {
        if (callback) {
            callback(-1, @"user id error.");
        }
        return;
    }
    if ([[[V2TIMManager sharedInstance] getLoginUser] isEqualToString:userID]) {
        callback(0, @"alerady login");
        return;
    }
    [[V2TIMManager sharedInstance] login:userID userSig:userSig succ:^{
        if (callback) {
            callback(0, @"success");
        }
    } fail:^(int code, NSString *desc) {
        if (callback) {
            callback(code, desc ?: @"login failed.");
        }
    }];
}

+ (void)logout:(LRIMCallback)callback {
    [[V2TIMManager sharedInstance] logout:^{
        if (callback) {
            callback(0, @"success");
        }
    } fail:^(int code, NSString *desc) {
        if (callback) {
            callback(code, desc ?: @"login failed.");
        }
    }];
}

+ (void)setProfileWithName:(NSString *)name avatar:(NSString *)avatar callback:(LRIMCallback)callback {
    V2TIMUserFullInfo* info = [[V2TIMUserFullInfo alloc] init];
    info.nickName = name;
    info.faceURL = avatar;
    [[V2TIMManager sharedInstance] setSelfInfo:info succ:^{
        if (callback) {
            callback(0, @"success");
        }
    } fail:^(int code, NSString *desc) {
        if (callback) {
            callback(code, desc ?: @"set profile failed.");
        }
    }];
}

+ (void)createRoomWithRoomID:(NSString *)roomID roomParam:(TRTCCreateRoomParam *)roomParam
 success:(LREnterRoomCallback)success error:(LRIMCallback)errorCallback {
    [[V2TIMManager sharedInstance] createGroup:@"AVChatRoom" groupID:roomID groupName:roomParam.roomName succ:^(NSString *groupID) {
        if (success) {
            success(@[], @{}, nil);
        }
        V2TIMGroupInfo *info = [[V2TIMGroupInfo alloc] init];
        info.groupID = roomID;
        info.faceURL = roomParam.coverUrl;
        info.groupName = roomParam.roomName;
        [[V2TIMManager sharedInstance] setGroupInfo:info succ:nil fail:nil];
    } fail:^(int code, NSString *desc) {
        if (code == ERR_SVR_GROUP_GROUPID_IN_USED_FOR_SUPER) {
            [[V2TIMManager sharedInstance] joinGroup:roomID msg:nil succ:^{
                [TRTCLiveRoomIMAction getAllMembersWithRoomID:roomID success:^(NSArray<TRTCLiveUserInfo *> * _Nonnull members) {
                    if (success) {
                        success(members, @{}, nil);
                    }
                } error:errorCallback];
                V2TIMGroupInfo *info = [[V2TIMGroupInfo alloc] init];
                info.groupID = roomID;
                info.faceURL = roomParam.coverUrl;
                info.groupName = roomParam.roomName;
                [[V2TIMManager sharedInstance] setGroupInfo:info succ:nil fail:nil];
            } fail:^(int code, NSString *desc) {
                if (errorCallback) {
                    errorCallback(code, desc ?: @"joinGroup failed.");
                }
            }];
        } else {
            if (errorCallback) {
                errorCallback(code, desc ?: @"joinGroup failed.");
            }
        }
    }];
}

+ (void)destroyRoomWithRoomID:(NSString *)roomID callback:(LRIMCallback)callback {
    [[V2TIMManager sharedInstance] dismissGroup:roomID succ:^{
        if (callback) {
            callback(0, @"success");
        }
    } fail:^(int code, NSString *desc) {
        if (callback) {
            callback(code, desc ?: @"");
        }
    }];
}

+ (void)enterRoomWithRoomID:(NSString *)roomID success:(LREnterRoomCallback)success error:(LRIMCallback)errorCallback {
    [[V2TIMManager sharedInstance] joinGroup:roomID msg:@"" succ:^{
        [TRTCLiveRoomIMAction getAllMembersWithRoomID:roomID success:^(NSArray<TRTCLiveUserInfo *> * _Nonnull members) {
            [[V2TIMManager sharedInstance] getGroupsInfo:@[roomID] succ:^(NSArray<V2TIMGroupInfoResult *> *groupResultList) {
                if (!groupResultList || groupResultList.count == 0) {
                    TRTCLog(@"failed to query group info");
                    return;
                }
                V2TIMGroupInfoResult *infoResult = groupResultList[0];
                V2TIMGroupInfo* info = infoResult.info;
                NSDictionary *customInfo = [info.introduction mj_JSONObject];
                if (info && customInfo) {
                    TRTCLiveRoomInfo* roomInfo = nil;
                    NSArray *userDic = customInfo[@"list"];
                    NSDictionary *owner = userDic.count > 0 ? userDic[0] : nil;
                    NSNumber *type = customInfo[@"type"] ?: @(1);
                    if (owner) {
                        roomInfo = [[TRTCLiveRoomInfo alloc] initWithRoomId:roomID
                                                                   roomName:@""
                                                                   coverUrl:@""
                                                                    ownerId:owner[@"userId"] ?: @""
                                                                  ownerName:owner[@"name"] ?: @""
                                                                   streamUrl:owner[@"streamId"]
                                                                memberCount:0
                                                                 roomStatus:[type intValue]];
                    }
                    [TRTCLiveRoomIMAction getRoomInfoWithRoomIds:@[roomID] success:^(NSArray<TRTCLiveRoomInfo *> * _Nonnull roomInfos) {
                        if (roomInfos.count > 0) {
                            roomInfo.roomName = roomInfos[0].roomName ?: @"";
                            roomInfo.coverUrl = roomInfos[0].coverUrl ?: @"";
                            if (success) {
                                success(members, customInfo, roomInfo);
                            }
                        }
                    } error:^(int code, NSString * _Nonnull message) {
                        if (success) {
                            success(members, customInfo, roomInfo);
                        }
                    }];
                }
            } fail:errorCallback];
        } error:errorCallback];
    } fail:errorCallback];
}

+ (void)exitRoomWithRoomID:(NSString *)roomID callback:(LRIMCallback)callback {
    [[V2TIMManager sharedInstance] quitGroup:roomID succ:^{
        if (callback) {
            callback(0, @"");
        }
    } fail:^(int code, NSString *desc) {
        if (callback) {
            callback(code, desc ?: @"quit group failed.");
        }
    }];
}

+ (void)getRoomInfoWithRoomIds:(NSArray<NSString *> *)roomIds success:(LRRoomInfosCallback)success error:(LRIMCallback)error {
    [[V2TIMManager sharedInstance] getGroupsInfo:roomIds succ:^(NSArray<V2TIMGroupInfoResult *> *groupResultList) {
        if (!groupResultList) {
            error(-1, liveRoomLocalize(@"Demo.TRTC.LiveRoom.cannotgetroominfo"));
            return;
        }
        NSMutableArray<TRTCLiveRoomInfo *> *roomInfos = [[NSMutableArray alloc] initWithCapacity:2];
        [groupResultList enumerateObjectsUsingBlock:^(V2TIMGroupInfoResult * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
            V2TIMGroupInfo* info = obj.info;
            NSDictionary *customInfo = [info.introduction mj_JSONObject];
            if (info && customInfo) {
                TRTCLiveRoomInfo* roomInfo = nil;
                NSArray *userDic = customInfo[@"list"];
                NSDictionary *owner = userDic.count > 0 ? userDic[0] : nil;
                NSNumber *type = customInfo[@"type"] ?: @(1);
                if (owner) {
                    roomInfo = [[TRTCLiveRoomInfo alloc] initWithRoomId:info.groupID
                                                               roomName:info.groupName
                                                               coverUrl:info.faceURL
                                                                ownerId:info.owner
                                                              ownerName:(owner[@"name"] ?: @"")
                                                              streamUrl:(owner[@"streamId"] ?: @"")
                                                            memberCount:info.memberCount
                                                             roomStatus:[type intValue]];
                }
                if (roomInfo) {
                    [roomInfos addObject:roomInfo];
                }
            }
        }];
        if (success) {
            success(roomInfos);
        }
    } fail:^(int code, NSString *desc) {
        if (error) {
            error(code, desc ?: liveRoomLocalize(@"Demo.TRTC.LiveRoom.getroominfofailed"));
        }
    }];
}

+ (void)getAllMembersWithRoomID:(NSString *)roomID success:(LRMemberCallback)success error:(LRIMCallback)error {
    [[V2TIMManager sharedInstance] getGroupMemberList:roomID filter:V2TIM_GROUP_MEMBER_FILTER_ALL
     nextSeq:0 succ:^(uint64_t nextSeq, NSArray<V2TIMGroupMemberFullInfo *> *memberList) {
        if (success) {
            NSMutableArray *infos = [[NSMutableArray alloc] initWithCapacity:2];
            if (memberList) {
                [memberList enumerateObjectsUsingBlock:^(V2TIMGroupMemberFullInfo * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {
                    TRTCLiveUserInfo* user = [[TRTCLiveUserInfo alloc] initWithProfile:obj];
                    [infos addObject:user];
                }];
            }
            success(infos);
        }
    } fail:^(int code, NSString *desc) {
        if (error) {
            error(code, desc ?: @"");
        }
    }];
}

#pragma mark - Action Message
+ (void)notifyStreamToAnchorWithUserId:(NSString *)userID streamID:(NSString *)streamID callback:(LRIMCallback)callback {
    NSDictionary *data = @{
        @"action": @(TRTCLiveRoomIMActionTypeNotifyJoinAnchorStream),
        @"stream_id": streamID,
        @"version": gTrtcLiveRoomProtocolVersion,
    };
    ConversationParams *params = [[ConversationParams alloc] init];
    params.type = ConvTypeUser;
    params.userID = userID;
    [TRTCLiveRoomIMAction sendMessage:data convType:params callback:callback];
}

+ (void)sendRoomTextMsgWithRoomID:(NSString *)roomID message:(NSString *)message callback:(LRIMCallback)callback {
    NSDictionary *data = @{
        @"action": @(TRTCLiveRoomIMActionTypeRoomTextMsg),
        @"version": gTrtcLiveRoomProtocolVersion,
    };
    ConversationParams *params = [[ConversationParams alloc] init];
    params.type = ConvTypeGroup;
    params.gourpID = roomID;
    params.text = message;
    params.priority = V2TIM_PRIORITY_LOW;
    [TRTCLiveRoomIMAction sendMessage:data convType:params callback:callback];
}

+ (void)sendRoomCustomMsgWithRoomID:(NSString *)roomID command:(NSString *)command message:(NSString *)message callback:(LRIMCallback)callback {
    NSDictionary *data = @{
        @"action": @(TRTCLiveRoomIMActionTypeRoomCustomMsg),
        @"command": command,
        @"message": message,
        @"version": gTrtcLiveRoomProtocolVersion,
    };
    ConversationParams *params = [[ConversationParams alloc] init];
    params.type = ConvTypeGroup;
    params.gourpID = roomID;
    params.text = nil;
    params.priority = V2TIM_PRIORITY_LOW;
    [TRTCLiveRoomIMAction sendMessage:data convType:params callback:callback];
}

+ (void)updateGroupInfoWithRoomID:(NSString *)roomID groupInfo:(NSDictionary<NSString *,id> *)groupInfo callback:(LRIMCallback)callback {
    NSMutableDictionary *data = [groupInfo mutableCopy];
    data[@"version"] = gTrtcLiveRoomProtocolVersion;
    NSString *groupInfoString = [data mj_JSONString];
    TRTCLog(@" updateGroupInfo:%@, size:%lu", groupInfoString, (unsigned long)groupInfoString.length);
    V2TIMGroupInfo *info = [[V2TIMGroupInfo alloc] init];
    info.groupID = roomID;
    info.introduction = groupInfoString;
    [[V2TIMManager sharedInstance] setGroupInfo:info succ:^{
        data[@"action"] = @(TRTCLiveRoomIMActionTypeUpdateGroupInfo);
        ConversationParams *params = [[ConversationParams alloc] init];
           params.type = ConvTypeGroup;
           params.gourpID = roomID;
           params.text = @"";
           params.priority = V2TIM_PRIORITY_LOW;
        [TRTCLiveRoomIMAction sendMessage:data convType:params callback:callback];
    } fail:^(int code, NSString *desc) {
        if (callback) {
            callback(code, desc ?: @"update group info failed.");
        }
    }];
}

#pragma mark - Private method
+ (void)sendMessage:(NSDictionary<NSString *, id> *)data convType:(ConversationParams *)params callback:(LRIMCallback _Nullable)callback {
    NSError *error;
    NSData* jsonData = [NSJSONSerialization dataWithJSONObject:data options:NSJSONWritingFragmentsAllowed error:&error];
    if (error) {
        if (callback) {
             callback(-1, @"JSON data serialization failed.");
        }
        return;
    }
    V2TIMMessage *message = nil;
    if (params.text && params.type == ConvTypeGroup) {
        message = [[V2TIMManager sharedInstance] createTextMessage:params.text];
    } else {
        message = [[V2TIMManager sharedInstance] createCustomMessage:jsonData];
        params.priority = V2TIM_PRIORITY_NORMAL;
    }
    switch (params.type) {
        case ConvTypeGroup:
        {
            [[V2TIMManager sharedInstance] sendMessage:message
                                              receiver:nil
                                               groupID:params.gourpID
                                              priority:params.priority
                                        onlineUserOnly:NO
                                       offlinePushInfo:nil
                                              progress:nil
                                                  succ:^{
                if (callback) {
                    callback(0, @"send successfully!");
                }
            } fail:^(int code, NSString *desc) {
                if (callback) {
                    callback(0, desc ?: @"send message error.");
                }
            }];
        }
            break;
        case ConvTypeUser:
        {
            [[V2TIMManager sharedInstance] sendMessage:message
                                              receiver:params.userID
                                               groupID:nil
                                              priority:params.priority
                                        onlineUserOnly:NO
                                       offlinePushInfo:nil
                                              progress:nil
                                                  succ:^{
                if (callback) {
                    callback(0, @"send successfully!");
                }
            } fail:^(int code, NSString *desc) {
                if (callback) {
                    callback(0, desc ?: @"send message error.");
                }
            }];
        }
            break;
        default:
            break;
    }
}

#pragma mark - Signaling Channel
+ (NSDictionary *)covertCmdMessage:(NSDictionary *)data {
    return @{
        @"version" : @(1),
        @"data" : data,
        Signal_Business_ID : Signal_Business_Live,
        Signal_Platform :Signal_Platform_OS
    };
}

+ (NSString * _Nullable)requestJoinAnchorWithUserID:(NSString *)userID timeout:(int)timeout
 reason:(NSString *)reason callback:(LRIMCallback _Nullable)callback {
    NSDictionary *data = @{
        @"cmd" : Signal_RequestJoinAnchor,
        @"message" : reason ? : @"",
    };
    NSError *error;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:[self covertCmdMessage:data] options:NSJSONWritingPrettyPrinted error:&error];
    if (!jsonData) {
        callback(-1, @"json error");
        return nil;
    } else {
        NSString *content = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        return [TRTCLiveRoomIMAction sendInvitationWithUserId:userID timeout:timeout content:content callback:callback];
    }
}

+ (void)cancelRequestJoinAnchorWithRequestID:(NSString *)requestID reason:(NSString *)reason callback:(LRIMCallback)callback {
    NSDictionary *data = @{
        @"cmd": Signal_RequestJoinAnchor,
        @"message": reason ?: @""
    };
    
    NSError *error;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:[self covertCmdMessage:data] options:NSJSONWritingPrettyPrinted error:&error];
    if (!jsonData) {
        callback(-1, @"json error");
        return;
    } else {
        NSString *content = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        [TRTCLiveRoomIMAction cancelInvitation:requestID data:content callback:callback];
    }
}

+ (void)respondJoinAnchorWithRequestID:(NSString *)requestID agreed:(BOOL)agreed reason:(NSString *)reason callback:(LRIMCallback _Nullable)callback {
    NSDictionary *data = @{
        @"cmd": Signal_RequestJoinAnchor,
        @"message": reason ?: @""
    };
    NSError *error;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:[self covertCmdMessage:data] options:NSJSONWritingPrettyPrinted error:&error];
    if (!jsonData) {
        callback(-1, @"json error");
    } else {
        NSString *content = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        [self respondSignallingMessage:requestID agree:agreed content:content callback:callback];
    }
}

+ (NSString *)kickoutJoinAnchorWithUserID:(NSString *)userID callback:(LRIMCallback _Nullable)callback {
    NSDictionary *data = @{
        @"cmd": Signal_KickoutJoinAnchor,
    };
    NSError *error;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:[self covertCmdMessage:data] options:NSJSONWritingPrettyPrinted error:&error];
    if (!jsonData) {
        callback(-1, @"json error");
        return nil;
    } else {
        NSString *content = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        return [TRTCLiveRoomIMAction sendInvitationWithUserId:userID timeout:0 content:content callback:callback];
    }
}

+ (void)respondKickoutJoinAnchor:(NSString *)inviteID agree:(BOOL)agree message:(NSString *)message {
    NSDictionary *data = @{
        @"cmd": Signal_KickoutJoinAnchor,
    };
    NSError *error;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:[self covertCmdMessage:data] options:NSJSONWritingPrettyPrinted error:&error];
    if (!jsonData) {
        return;
    } else {
        NSString *content = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        [TRTCLiveRoomIMAction respondSignallingMessage:inviteID agree:YES content:content callback:nil];
    }
}

+ (NSString *)requestRoomPKWithUserID:(NSString *)userID timeout:(int)timeout
 fromRoomID:(NSString *)fromRoomID fromStreamID:(NSString *)fromStreamID callback:(LRIMCallback
 _Nullable)callback {
    NSDictionary *data = @{
        @"cmd":Signal_RequestRoomPK,
        @"roomId": fromRoomID,
        @"streamId": fromStreamID
    };
    NSError *error;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:[self covertCmdMessage:data] options:NSJSONWritingPrettyPrinted error:&error];
    if (!jsonData) {
        callback(-1, @"json error");
        return nil;
    } else {
        NSString *content = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        return [TRTCLiveRoomIMAction sendInvitationWithUserId:userID timeout:timeout content:content callback:callback];
    }
}

+ (void)cancelRequestRoomPKWithRequestID:(NSString *)requestID reason:(NSString *)reason callback:(LRIMCallback)callback {
    NSDictionary *data = @{
        @"cmd": Signal_RequestRoomPK,
    };
    NSError *error;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:[self covertCmdMessage:data] options:NSJSONWritingPrettyPrinted error:&error];
    if (!jsonData) {
        callback(-1, @"json error");
        return;
    } else {
        NSString *content = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        return [TRTCLiveRoomIMAction cancelInvitation:requestID data:content callback:callback];
    }
}

+ (void)responseRoomPKWithRequestID:(NSString *)requestID agreed:(BOOL)agreed reason:(NSString *
 _Nullable)reason streamID:(NSString *)streamID callback:(LRIMCallback _Nullable)callback {
    NSDictionary *data = @{
        @"cmd": Signal_RequestRoomPK,
        @"stream_id":streamID,
    };
    NSError *error;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:[self covertCmdMessage:data] options:NSJSONWritingPrettyPrinted error:&error];
    if (!jsonData) {
        callback(-1, @"json error");
    } else {
        NSString *content = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        [self respondSignallingMessage:requestID agree:agreed content:content callback:callback];
    }
}

+ (NSString *)quitRoomPKWithUserID:(NSString *)userID callback:(LRIMCallback _Nullable)callback {
    NSDictionary *data = @{
        @"cmd":Signal_QuitRoomPK,
    };
    NSError *error;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:[self covertCmdMessage:data] options:NSJSONWritingPrettyPrinted error:&error];
    if (!jsonData) {
        callback(-1, @"json error");
        return nil;
    } else {
        NSString *content = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        return [TRTCLiveRoomIMAction sendInvitationWithUserId:userID timeout:0 content:content callback:callback];
    }
}

+ (void)respondQuitRoomPK:(NSString *)inviteID agree:(BOOL)agree message:(NSString *)message {
    NSDictionary *data = @{
        @"action": Signal_QuitRoomPK,
    };
    NSError *error;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:[self covertCmdMessage:data] options:NSJSONWritingPrettyPrinted error:&error];
    if (!jsonData) {
        return;
    } else {
        NSString *content = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
        [TRTCLiveRoomIMAction respondSignallingMessage:inviteID agree:YES content:content callback:nil];

    }
}

#pragma mark - Signaling Message Sending
+ (NSString *)sendInvitationWithUserId:(NSString *)userId timeout:(int)timeout content:(NSString *)content callback:(LRIMCallback _Nullable)callback {
    return [[V2TIMManager sharedInstance] invite:userId data:content onlineUserOnly:YES offlinePushInfo:nil timeout:timeout succ:^{
        if (callback) {
            callback(0, @"send successfully");
        }
    } fail:^(int code, NSString *desc) {
        if (callback) {
            callback(-1, desc ?: @"send message error.");
        }
    }];
}

+ (void)acceptInvitation:(NSString *)identifier data:(NSString *)data callback:(LRIMCallback _Nullable)callback {
   [[V2TIMManager sharedInstance] accept:identifier data:data succ:^{
       if (callback) {
           callback(0, @"send successfully");
       }
   } fail:^(int code, NSString *desc) {
       if (callback) {
           callback(-1, desc ?: @"accept invation error.");
       }
   }];;
}

+ (void)rejectInvitaiton:(NSString *)identifier data:(NSString *)data callback:(LRIMCallback _Nullable)callback {
   [[V2TIMManager sharedInstance] reject:identifier data:data succ:^{
       if (callback) {
           callback(0, @"send successfully");
       }
   } fail:^(int code, NSString *desc) {
       if (callback) {
           callback(-1, desc ?: @"reject invitation error");
       }
   }];
}

+ (void)cancelInvitation:(NSString *)identifier data:(NSString *)data callback:(LRIMCallback _Nullable)callback {
   [[V2TIMManager sharedInstance] cancel:identifier data:data succ:^{
       if (callback) {
           callback(0, @"send successfully");
       }
   } fail:^(int code, NSString *desc) {
       if (callback) {
           callback(-1, desc ?: @"reject invitation error");
       }
   }];
}

+ (void)respondSignallingMessage:(NSString *)inviteID agree:(BOOL)agree content:(NSString *)content callback:(LRIMCallback _Nullable)callback {
    if (agree) {
        [TRTCLiveRoomIMAction acceptInvitation:inviteID data:content callback:callback];
    } else {
        [TRTCLiveRoomIMAction rejectInvitaiton:inviteID data:content callback:callback];
    }
}

@end
