//
//  TRTCLiveRoomDelegate.h
//  TRTCVoiceRoomOCDemo
//
//  Created by abyyxwang on 2020/7/8.
//  Copyright © 2020 Tencent. All rights reserved.
//

#ifndef TRTCLiveRoomDelegate_h
#define TRTCLiveRoomDelegate_h

#import <Foundation/Foundation.h>
#import "TRTCLiveRoomDef.h"
NS_ASSUME_NONNULL_BEGIN

@class TRTCLiveRoom;
@protocol TRTCLiveRoomDelegate <NSObject>

@optional
/// Log callback
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
          onDebugLog:(NSString *)log
NS_SWIFT_NAME(trtcLiveRoom(_:onDebugLog:));

/// Error callback
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
             onError:(NSInteger)code
             message:(NSString  *)message
NS_SWIFT_NAME(trtcLiveRoom(_:onError:message:));
/// Error callback
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
           onWarning:(NSInteger)code
             message:(NSString *)message
NS_SWIFT_NAME(trtcLiveRoom(_:onWarning:message:));

/// Callback for room termination
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
       onRoomDestroy:(NSString *)roomID
NS_SWIFT_NAME(trtcLiveRoom(_:onRoomDestroy:));

/// Callback for live room information change
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
    onRoomInfoChange:(TRTCLiveRoomInfo *)info
NS_SWIFT_NAME(trtcLiveRoom(_:onRoomInfoChange:));

/// Callback for anchor's room entry
/// @note: Anchors include the main room anchor, co-anchoring audience members, and anchors using cross-room communication
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
       onAnchorEnter:(NSString *)userID
NS_SWIFT_NAME(trtcLiveRoom(_:onAnchorEnter:));

/// Callback for anchor's exit
/// @note: Anchors include the main room anchor, co-anchoring audience members, and anchors using cross-room communication
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
        onAnchorExit:(NSString *)userID
NS_SWIFT_NAME(trtcLiveRoom(_:onAnchorExit:));

/// Callback for audience member's room entry
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
     onAudienceEnter:(TRTCLiveUserInfo *)user
NS_SWIFT_NAME(trtcLiveRoom(_:onAudienceEnter:));

/// Callback for audience member's exit
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
      onAudienceExit:(TRTCLiveUserInfo *)user
NS_SWIFT_NAME(trtcLiveRoom(_:onAudienceExit:));

/// The anchor received a co-anchoring request from an audience member.
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
 onRequestJoinAnchor:(TRTCLiveUserInfo *)user
             reason:(NSString * _Nullable)reason
NS_SWIFT_NAME(trtcLiveRoom(_:onRequestJoinAnchor:reason:));

/// An audience member canceled a co-anchoring request.
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
  onCancelJoinAnchor:(TRTCLiveUserInfo *)user
              reason:(NSString *)reason
NS_SWIFT_NAME(trtcLiveRoom(_:onCancelJoinAnchor:reason:));

/// An audience member’s request to speak timed out
/// @param userID Timed out user ID
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
audienceRequestJoinAnchorTimeout:(NSString *)userID
NS_SWIFT_NAME(trtcLiveRoom(_:audienceRequestJoinAnchorTimeout:));

/// An audience member received the mic-off notification from the anchor
- (void)trtcLiveRoomOnKickoutJoinAnchor:(TRTCLiveRoom *)liveRoom
NS_SWIFT_NAME(trtcLiveRoomOnKickoutJoinAnchor(_:));

/// The anchor received a cross-room communication request from another anchor
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
     onRequestRoomPK:(TRTCLiveUserInfo *)user
NS_SWIFT_NAME(trtcLiveRoom(_:onRequestRoomPK:));

/// The request for cross-room communication from another anchor timed out
/// @param userID ID of timed out anchor
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
anchorRequestRoomPKTimeout:(NSString *)userID
NS_SWIFT_NAME(trtcLiveRoom(_:anchorRequestJoinAnchorTimeout:));

/// The anchor received a request to cancel cross-room communication from another anchor
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
     onCancelRoomPK:(TRTCLiveUserInfo *)user
NS_SWIFT_NAME(trtcLiveRoom(_:onCancelRoomPK:));

/// The anchor received a notification that the peer anchor ended cross-room communication
- (void)trtcLiveRoomOnQuitRoomPK:(TRTCLiveRoom *)liveRoom
NS_SWIFT_NAME(trtcLiveRoomOnQuitRoomPK(_:));

/// Room members received a group text chat message
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
   onRecvRoomTextMsg:(NSString *)message
            fromUser:(TRTCLiveUserInfo *)user
NS_SWIFT_NAME(trtcLiveRoom(_:onRecvRoomTextMsg:fromUser:));

/// Room members received a custom group message
- (void)trtcLiveRoom:(TRTCLiveRoom *)trtcLiveRoom
onRecvRoomCustomMsgWithCommand:(NSString *)command
             message:(NSString *)message
            fromUser:(TRTCLiveUserInfo *)user
NS_SWIFT_NAME(trtcLiveRoom(_:onRecvRoomCustomMsg:message:fromUser:));

@end

NS_ASSUME_NONNULL_END

#endif /* TRTCLiveRoomDelegate_h */
