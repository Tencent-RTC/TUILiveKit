//
//  TRTCLiveRoomDef.m
//  TRTCVoiceRoomOCDemo
//
//  Created by abyyxwang on 2020/7/7.
//  Copyright © 2020 tencent. All rights reserved.
//

#import "TRTCLiveRoomDef.h"

NSString * const Signal_Business_ID = @"businessID";
NSString * const Signal_Business_Live = @"Live";
NSString * const Signal_Platform = @"platform";
NSString * const Signal_Platform_OS = @"iOS";

@implementation TRTCCreateRoomParam

- (instancetype)initWithRoomName:(NSString *)roomName coverUrl:(NSString *)coverUrl{
    self = [super init];
    if (self) {
        self.roomName = roomName;
        self.coverUrl = coverUrl;
    }
    return self;
}

@end

@implementation TRTCLiveRoomConfig

- (instancetype)initWithUseCDNFirst:(BOOL)userCDNFirst cdnPlayDomain:(NSString *)cdnPlayDomain {
    self = [super init];
    if (self) {
        self.useCDNFirst = userCDNFirst;
        self.cdnPlayDomain = cdnPlayDomain;
    }
    return self;
}

@end

@implementation TRTCLiveRoomInfo

- (instancetype)init {
    if (self = [super init]) {
        self.roomId = @"";
        self.roomName = @"";
        self.coverUrl = @"";
        self.ownerId = @"";
        self.ownerName = @"";
        self.streamUrl = @"";
        self.memberCount = 0;
        self.roomStatus = TRTCLiveRoomLiveStatusNone;
    }
    return self;
}

- (instancetype)initWithRoomId:(NSString *)roomId
                      roomName:(NSString *)roomName
                      coverUrl:(NSString *)coverUrl
                       ownerId:(NSString *)ownerId
                     ownerName:(NSString *)ownerName
                     streamUrl:(NSString *)streamUrl
                   memberCount:(NSInteger)memberCount
                    roomStatus:(TRTCLiveRoomLiveStatus)roomStatus{
    self = [super init];
    if (self) {
        self.roomId = roomId;
        self.roomName = roomName;
        self.coverUrl = coverUrl;
        self.ownerId = ownerId;
        self.ownerName = ownerName;
        self.streamUrl = streamUrl;
        self.memberCount = memberCount;
        self.roomStatus = roomStatus;
    }
    return self;
}

@end

@implementation TRTCLiveUserInfo

- (instancetype)initWithProfile:(V2TIMGroupMemberFullInfo *)profile {
    self = [super init];
    if (self) {
        self.userId = profile.userID;
        self.userName = profile.nickName ?: @"";
        self.avatarURL = profile.faceURL ?: @"";
        self.isOwner = profile.role == V2TIM_GROUP_MEMBER_ROLE_SUPER;
    }
    return self;
}

-(NSString *)description {
    return [NSString stringWithFormat:@"ID: %@, Name: %@", self.userId, self.userName];
}

@end
