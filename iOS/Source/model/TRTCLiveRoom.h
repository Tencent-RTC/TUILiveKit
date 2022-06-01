//
//  TRTCLiveRoom.h
//  TRTCVoiceRoomOCDemo
//
//  Created by abyyxwang on 2020/7/7.
//  Copyright © 2020 Tencent. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "TRTCLiveRoomDelegate.h"
#import "TRTCLiveRoomDef.h"
#import "TUILiveRoomKit.h"

NS_ASSUME_NONNULL_BEGIN

@class TXAudioEffectManager;
@class TXBeautyManager;
@interface TRTCLiveRoom : NSObject <TRTCCloudDelegate>

@property(nonatomic, weak)id<TRTCLiveRoomDelegate> delegate;

/// Initializes the `TRTCLiveRoom` singleton
+ (instancetype)shareInstance;

- (instancetype)init NS_UNAVAILABLE;
+ (instancetype)new NS_UNAVAILABLE;

#pragma mark - login and logout APIs

//////////////////////////////////////////////////////////
//
//                  Login and logout APIs
//
//////////////////////////////////////////////////////////

/// Log in to the component.
///
/// @param sdkAppID You can view `SDKAppID` in **[Application Management](https://console.cloud.tencent.com/trtc/app)** > **Application Info** of the TRTC console.
/// @param userID ID of the current user, which is a string that can contain only letters (a-z and A-Z), digits (0-9), hyphens (-), and underscores (\_)
/// @param userSig Tencent Cloud's proprietary security protection signature. For more information on how to get it, see [UserSig](https://cloud.tencent.com/document/product/647/17275).
/// @param config Global configuration information. Initialize it during login as it cannot be modified after login. `useCDNFirst`: Specifies the way audience watch live streams. Valid values: YES: Audience members watch live streams over CDNs, which is cost-efficient but has high latency; NO: Audience members watch live streams in the low latency mode, the cost of which is between that of CDN live streaming and co-anchoring, but the latency is within one second.
/// @param callback Callback for login. The return code is `0` if login is successful.
///
/// @note:  We recommend setting the validity period of `userSig` to 7 days to avoid cases where message sending/receiving and co-anchoring fail due to expired `userSig`.
- (void)loginWithSdkAppID:(int)sdkAppID
                  userID:(NSString *)userID
                 userSig:(NSString *)userSig
                  config:(TRTCLiveRoomConfig *)config
                callback:(Callback _Nullable)callback
NS_SWIFT_NAME(login(sdkAppID:userID:userSig:config:callback:));

/// Log out
/// @param callback  Callback for logout. The code is `0` if logout is successful.
- (void)logout:(Callback _Nullable)callback
NS_SWIFT_NAME(logout(_:));

/// Set user profiles. The information will be stored in Tencent Cloud IM.
///
/// @param name Username
/// @param avatarURL Profile picture URL
/// @param callback Callback for setting user profiles. The code is `0` if the operation is successful.
- (void)setSelfProfileWithName:(NSString *)name
                     avatarURL:(NSString * _Nullable)avatarURL
                      callback:(Callback _Nullable)callback
NS_SWIFT_NAME(setSelfProfile(name:avatarURL:callback:));

#pragma mark - room management APIs
//////////////////////////////////////////////////////////
//
//                  Room management
//
//////////////////////////////////////////////////////////

/// Create a room (called by anchor). If the room does not exist, the system will create the room automatically.
/// The process of creating a room and starting live streaming as an anchor is as follows:
/// 1. A user calls `startCameraPreview()` to enable camera preview and set beauty filters.
/// 2. The user calls `createRoom()` to create a room, the result of which is returned via a callback.
/// 3. The user calls `starPublish()` to push streams.
///
/// @param roomID The ID of the room. You need to assign and manage the IDs in a centralized manner. Multiple `roomID` values can be aggregated into a live room list. Currently, Tencent Cloud does not provide list management services. You need to manage your own room list.
/// @param roomParam Room information, such as room name and cover information. If both the room list and room information are managed by yourself, you can ignore this parameter.
/// @param callback Callback for room entry. The code is `0` if room entry is successful.
///
/// @note This API is called by an anchor to start live streaming. An anchor can create a room he or she created before.
- (void)createRoomWithRoomID:(UInt32)roomID
                   roomParam:(TRTCCreateRoomParam *)roomParam
                    callback:(Callback _Nullable)callback
NS_SWIFT_NAME(createRoom(roomID:roomParam:callback:));

/// Terminate a room (called by anchor)
/// After creating a room, an anchor can call this API to terminate it.
/// @param callback Callback for room termination. The code is `0` if the operation is successful.
///
/// @note After creating a room, an anchor can call this API to terminate it
- (void)destroyRoom:(Callback _Nullable)callback
NS_SWIFT_NAME(destroyRoom(callback:));

/// Enter a room (called by audience)
/// The process of entering a room and starting playback as an audience member is as follows:
/// 1. A user gets the latest room list from your server. The list may contain the `roomID` and other information of multiple rooms.
/// 2. The user selects a room and calls `enterRoom()` to enter the room.
/// 3. If the room list managed by your server contains the `userId` of the anchor of each room, audience members can call `startPlay(userId)` to play back the anchor's video after entering the room (`enterRoom()`).
/// If the room list contains `roomid` only, an audience member will receive the `onAnchorEnter(userId)` callback from `TRTCLiveRoomDelegate` upon room entry (`enterRoom()`).
/// The audience member can then call `startPlay(userId)` with the `userId` obtained from the callback to play back the anchor's video.
///
/// @param roomID ROOM ID
/// @param callback Callback for room entry. The code is `0` if room entry is successful.
///
/// @note
///   This API is called by a user to enter a room.
///   An anchor cannot use this API to enter a room he or she created, but must use createRoom instead.
- (void)enterRoomWithRoomID:(UInt32)roomID
                   callback:(Callback _Nullable)callback
NS_SWIFT_NAME(enterRoom(roomID:callback:));

/// Leave a room (called by audience)
/// @param callback Callback for room exit. The code is `0` if the operation is successful.
///
/// @note
///   This API is called by an audience member to leave a room.
///   An anchor cannot use this API to leave a room.

- (void)exitRoom:(Callback _Nullable)callback
NS_SWIFT_NAME(exitRoom(callback:));

/// Get room details
/// The information is provided by anchors via the `roomInfo` parameter when they call `createRoom()`. You don’t need this API if both the room list and room information are managed by yourself.
/// @param roomIDs The list of room IDs.
/// @param callback Callback of room details.
- (void)getRoomInfosWithRoomIDs:(NSArray<NSString *> *)roomIDs
                       callback:(RoomInfoCallback _Nullable)callback
NS_SWIFT_NAME(getRoomInfos(roomIDs:callback:));

/// Get the anchors and co-anchoring users in a room. This API takes effect only if it is called after `enterRoom()`.
/// @param callback Callback of user details.
- (void)getAnchorList:(UserListCallback _Nullable)callback
NS_SWIFT_NAME(getAnchorList(callback:));

/// Get the information of all audience members in a room. This API takes effect only if it is called after `enterRoom()`.
/// @param callback Callback of user details.
- (void)getAudienceList:(UserListCallback _Nullable)callback
NS_SWIFT_NAME(getAudienceList(callback:));

#pragma mark - stream pull/push APIs
//////////////////////////////////////////////////////////
//
//                  Stream pull/push APIs
//
//////////////////////////////////////////////////////////

/// Enable local video preview
///
/// @param frontCamera `YES`: Front camera; `NO`: Rear camera
/// @param view The control that loads video images.
/// @param callback Callback for the operation.
- (void)startCameraPreviewWithFrontCamera:(BOOL)frontCamera
                                     view:(UIView *)view
                                 callback:(Callback _Nullable)callback
NS_SWIFT_NAME(startCameraPreview(frontCamera:view:callback:));

/// Stop local video capturing and preview
- (void)stopCameraPreview;

/// Start live streaming (push streams). This API can be called in the following scenarios:
/// 1. An anchor starts live streaming.
/// 2. An audience member user starts co-anchoring.
///
/// @param streamID The `streamID` used to bind live streaming CDNs. You need to set it to the `streamID` of the anchor if you want audience to play the anchor’s stream via CDNs.
/// @param callback Callback for the operation
- (void)startPublishWithStreamID:(NSString *)streamID
                        callback:(Callback _Nullable)callback
NS_SWIFT_NAME(startPublish(streamID:callback:));

/// Stop live streaming (pushing streams). This API can be called in the following scenarios:
/// 1. An anchor ends live streaming.
/// 2. An audience member ends co-anchoring.
/// @param callback Callback for the operation.
- (void)stopPublish:(Callback _Nullable)callback
NS_SWIFT_NAME(stopPublish(callback:));

/// Play a remote video. This API can be called in common playback and co-anchoring scenarios.
/// Common playback scenario
/// 1. If the room list managed by your server contains the `userId` of the anchor of each room, audience members can call `startPlay(userId)` to play back the anchor's video after entering the room (`enterRoom()`).
/// 2. If the room list contains `roomid` only, audience members will receive the `onAnchorEnter(userId)` callback from `TRTCLiveRoomDelegate` upon room entry (`enterRoom()`).
/// The audience member can then call `startPlay(userId)` with the `userId` obtained from the callback to play back the anchor's video.
/// Co-anchoring scenario
/// After co-anchoring is initiated, the anchor will receive the `onAnchorEnter(userId)` callback from `TRTCLiveRoomDelegate` and can call `startPlay(userId)` with the `userId` returned by the callback to play back the co-anchoring video.
///
/// @param userID The ID of the user whose video is to be played.
/// @param view The control that loads video images.
/// @param callback Callback for the operation.
- (void)startPlayWithUserID:(NSString *)userID
                       view:(UIView *)view
                   callback:(Callback _Nullable)callback
NS_SWIFT_NAME(startPlay(userID:view:callback:));

/// Stop rendering a remote video
///
/// @param userID The ID of the remote user.
/// @param callback Callback for the operation.
///
/// @note Call this API after receiving the `onAnchorExit` callback.
- (void)stopPlayWithUserID:(NSString *)userID
                  callback:(Callback _Nullable)callback
NS_SWIFT_NAME(stopPlay(userID:callback:));

#pragma mark - co-anchoring APIs
//////////////////////////////////////////////////////////
//
//                  Co-anchoring APIs
//
//////////////////////////////////////////////////////////

/**
    * Sends a co-anchoring request (called by audience)
    *
    * The process for co-anchoring between the anchor and an audience member is as follows:
    * 1. An **audience member** calls `requestJoinAnchor()` to send a co-anchoring request to the anchor.
    * 2. The **anchor** receives the `onRequestJoinAnchor` callback notification from `TRTCLiveRoomDelegate`.
    * 3. The **anchor** calls `responseJoinAnchor()` to accept or reject the co-anchoring request.
    * 4. The **audience member** receives the `responseCallback` callback, which indicates whether the request is accepted.
    * 5. If the request is accepted, the **audience member** calls `startCameraPreview()` to enable local camera preview.
    * 6. The **audience member** calls `startPublish()` to push streams.
    * 7. Once the audience member starts co-anchoring, the **anchor** will receive the `onAnchorEnter` callback from `TRTCLiveRoomDelegate`.
    * 8. The **anchor** calls `startPlay()` to play back the co-anchoring user's video.
    * 9. If there are other audience members co-anchoring with the anchor in the room, the new co-anchoring audience member will receive the `onAnchorEnter()` callback and can call `startPlay` to play other co-anchoring users' videos.
* */
   
/// Send a co-anchoring request
///
/// @param reason Reason for co-anchoring
/// @param timeout Timeout period
/// @param responseCallback Callback of the response
///
/// @note After an audience member sends a co-anchoring request, the anchor will receive the `onRequestJoinAnchor` callback.
- (void)requestJoinAnchor:(NSString *)reason
                  timeout:(int)timeout
         responseCallback:(ResponseCallback _Nullable)responseCallback
NS_SWIFT_NAME(requestJoinAnchor(reason:timeout:responseCallback:));

/// Cancels a co-anchoring request (called by audience)
///
/// @param reason Reason for co-anchoring
/// @param responseCallback Callback of the response
/// @note After an audience member cancels the co-anchoring request, the anchor will receive the `onCancelRequestJoinAnchor` callback.
- (void)cancelRequestJoinAnchor:(NSString *)reason
         responseCallback:(Callback _Nullable)responseCallback
NS_SWIFT_NAME(cancelJoinAnchor(reason:responseCallback:));

/// Respond to a co-anchoring request
///
/// @param userID User ID.
/// @param agree `YES`: Accept; `NO`: Reject
/// @param reason Reason for accepting/rejecting the request
/// @note After the anchor responds to the request, the audience will receive the `responseCallback` passed in to `requestJoinAnchor`.
- (void)responseJoinAnchor:(NSString *)userID
                     agree:(BOOL)agree
                    reason:(NSString *)reason
NS_SWIFT_NAME(responseJoinAnchor(userID:agree:reason:));

/// Remove a user from co-anchoring
///
/// @param userID ID of the user to remove from co-anchoring
/// @param callback Callback for the operation
/// @note After the anchor calls this API to remove a user from co-anchoring, the user will receive the `trtcLiveRoomOnKickoutJoinAnchor()` callback.
- (void)kickoutJoinAnchor:(NSString *)userID
                 callback:(Callback _Nullable)callback
NS_SWIFT_NAME(kickoutJoinAnchor(userID:callback:));

#pragma mark - anchor cross-room communication APIs

//////////////////////////////////////////////////////////
//
//                  Anchor cross-room communication APIs
//
//////////////////////////////////////////////////////////

/**
    * Requests cross-room communication
    *
    * Two anchors in different rooms can communicate with each other. The process is as follows:
    * 1. **Anchor A** calls `requestRoomPK()` to send a co-anchoring request to anchor B.
    * 2. **Anchor B** receives the `onRequestRoomPK` callback from `TRTCLiveRoomDelegate`.
    * 3. **Anchor B** calls `responseRoomPK()` to respond to the communication request.
    * 4. After accepting the request, **anchor B** waits for the `onAnchorEnter` callback from `TRTCLiveRoomDelegate` and calls `startPlay()` to play back anchor A's video.
    * 5. **Anchor A** receives the `responseCallback` callback, which indicates whether the request is accepted.
    * 6. If the request is accepted, **anchor A** waits for the `onAnchorEnter` callback from `TRTCLiveRoomDelegate` and calls `startPlay()` to play back anchor B's video.
    */

/// Request cross-room communication
///
/// @param roomID Room ID of the anchor to invite
/// @param userID User ID of the anchor to invite
/// @param timeout Timeout period
/// @param responseCallback Callback of the response
/// @note After a cross-room communication request is sent, the invited anchor will receive the `onRequestRoomPK` callback.
- (void)requestRoomPKWithRoomID:(UInt32)roomID
                         userID:(NSString *)userID
                        timeout:(int)timeout
               responseCallback:(ResponseCallback _Nullable)responseCallback
NS_SWIFT_NAME(requestRoomPK(roomID:userID:timeout:responseCallback:));
/// Respond to a cross-room communication request
/// Respond to a communication request from another anchor
///
/// @param userID User ID of the request sending anchor
/// @param agree `YES`: Accept; `NO`: Reject
/// @param reason Reason for accepting/rejecting the request
///
/// @note After the anchor responds to the request, the anchor sending the request will receive the `responseCallback` passed in to `requestRoomPK`.
- (void)responseRoomPKWithUserID:(NSString *)userID
                           agree:(BOOL)agree
                          reason:(NSString *)reason
NS_SWIFT_NAME(responseRoomPK(userID:agree:reason:));

/// Cancels the request for cross-room communication
///
/// @param roomID Room ID of the anchor to invite
/// @param userID User ID of the anchor to invite
/// @param responseCallback Callback of the response
///
/// @note After a cross-room communication request is sent, the invited anchor will receive the `onCancelRequestRoomPK` callback.
- (void)cancelRequestRoomPKWithRoomID:(UInt32)roomID
                        userID:(NSString *)userID
              responseCallback:(Callback _Nullable)responseCallback
NS_SWIFT_NAME(cancelRoomPK(roomID:userID:responseCallback:));

/// End cross-room communication
/// @param callback Callback for ending cross-room communication
///
/// @note If either anchor ends cross-room communication, the other anchor will receive the `trtcLiveRoomOnQuitRoomPK` callback.
- (void)quitRoomPK:(Callback _Nullable)callback
NS_SWIFT_NAME(quitRoomPK(callback:));

#pragma mark - Audio/Video control APIs
//////////////////////////////////////////////////////////
//
//                  Audio/Video control APIs
//
//////////////////////////////////////////////////////////

/// Switch between the front and rear cameras
- (void)switchCamera;

/// Specify whether to mirror video
/// @param isMirror Enable/Disable mirroring
- (void)setMirror:(BOOL)isMirror
NS_SWIFT_NAME(setMirror(_:));

/// Mute or unmute the local user
/// @param isMuted `YES`: Mute; `NO`: Unmute
- (void)muteLocalAudio:(BOOL)isMuted
NS_SWIFT_NAME(muteLocalAudio(_:));

/// Mute or unmute a remote user
///
/// @param userID: ID of the remote user
/// @param isMuted `YES`: Mute; `NO`: Unmute
- (void)muteRemoteAudioWithUserID:(NSString *)userID isMuted:(BOOL)isMuted
NS_SWIFT_NAME(muteRemoteAudio(userID:isMuted:));

/// Mute or unmute all remote users
/// @param isMuted `YES`: Mute; `NO`: Unmute
- (void)muteAllRemoteAudio:(BOOL)isMuted
NS_SWIFT_NAME(muteAllRemoteAudio(_:));

/// Set audio quality. Valid values: `1` (low), `2` (average), `3` (high)
/// @param quality Audio quality
- (void)setAudioQuality:(NSInteger)quality
NS_SWIFT_NAME(setAudioiQuality(_:));

/// Sets the resolution
/// @param resolution Video resolution
- (void)setVideoResolution:(TRTCVideoResolution)resolution
NS_SWIFT_NAME(setVideo(resolution:));

/// Sets the frame rate
/// @param fps Frame rate in FPS
- (void)setVideoFps:(int)fps
NS_SWIFT_NAME(setVideo(fps:));

/// Sets the bitrate
/// @param bitrate Bitrate in Kbps
- (void)setVideoBitrate:(int)bitrate
NS_SWIFT_NAME(setVideo(bitrate:));

/// Sets the mirror mode for the local preview
/// @param type Mirror type for local video preview
- (void)setLocalViewMirror:(TRTCLocalVideoMirrorType)type
NS_SWIFT_NAME(setLocalViewMirror(type:));


#pragma mark - Gets the audio effect management object

/// Get the audio effect management object
- (TXAudioEffectManager *)getAudioEffectManager;

#pragma mark - Beauty filter APIs
//////////////////////////////////////////////////////////
//
//                  Beauty filter APIs
//
//////////////////////////////////////////////////////////

/* Get the beauty filter management object TXBeautyManager
*
* You can do the following using TXBeautyManager:
* - Set the beauty filter style and apply effects including skin brightening, rosy skin, eye enlarging, face slimming, chin slimming, chin lengthening/shortening, face shortening, nose narrowing, eye brightening, teeth whitening, eye bag removal, wrinkle removal, and smile line removal.
* - Adjust the hairline, eye spacing, eye corners, lip shape, nose wings, nose position, lip thickness, and face shape.
* - Apply animated effects such as face widgets (materials).
* - Add makeup effects.
* - Recognize gestures.
*/
- (TXBeautyManager *)getBeautyManager;

//MARK: - on-screen commenting APIs

//////////////////////////////////////////////////////////
//
//                  On-screen commenting APIs
//
//////////////////////////////////////////////////////////

/// Send a chat message that can be seen by all users in a room
///
/// @param message Chat message
/// @param callback Callback for message sending
- (void)sendRoomTextMsg:(NSString *)message callback:(Callback _Nullable)callback
NS_SWIFT_NAME(sendRoomTextMsg(message:callback:));

/// Send a custom message
///
/// @param command custom command word used to distinguish between different message types
/// @param message Chat message
/// @param callback Callback for message sending
- (void)sendRoomCustomMsgWithCommand:(NSString *)cmd message:(NSString *)message callback:(Callback _Nullable)callback
NS_SWIFT_NAME(sendRoomCustomMsg(cmd:message:callback:));

#pragma mark - debugging APIs
//MARK: - debugging APIs

//////////////////////////////////////////////////////////
//
//                  Debugging APIs
//
//////////////////////////////////////////////////////////

/// Specify whether to display debugging information on the UI
/// @param isShow Show/Hide debugging information
- (void)showVideoDebugLog:(BOOL)isShow
NS_SWIFT_NAME(showVideoDebugLog(_:));

@end

NS_ASSUME_NONNULL_END
