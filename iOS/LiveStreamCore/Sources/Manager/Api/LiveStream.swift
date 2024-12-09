//
//  LiveStream.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/22.
//

import Foundation
import RTCRoomEngine
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

typealias SentRequestResultClosure = ((_ request: TUIRequest) -> Void)

protocol LiveStream {
    func destroy()

    func addRoomEngineObserver(_ observer: TUIRoomObserver)

    func removeRoomEngineObserver(_ observer: TUIRoomObserver)

    func addLiveConnectionManagerObserver(_ observer: TUILiveConnectionObserver)

    func removeLiveConnectionManagerObserver(_ observer: TUILiveConnectionObserver)

    /****************************************** Room Business *******************************************/
    func createRoom(roomInfo: TUIRoomInfo) async throws

    func enterRoom(roomId: String) async throws -> TUIRoomInfo

    func exitRoom() async throws

    func destroyRoom() async throws

    /****************************************** Seat Business *******************************************/
    func takeSeat(seatIndex: Int,
                  timeout: TimeInterval,
                  requestCallback: SentRequestResultClosure?) async throws -> TakeSeatResult

    func takeUserOnSeatByAdmin(seatIndex: Int,
                               userId: String,
                               timeout: TimeInterval,
                               requestCallback: SentRequestResultClosure?) async throws -> TakeSeatResult

    func leaveSeat() async throws

    func lockSeat(seatIndex: Int, lockMode: TUISeatLockParams) async throws

    func getSeatList() async throws -> [TUISeatInfo]

    func getSeatApplicationList() async throws -> [TUIRequest]
    
    func responseRemoteRequest(requestId: String, agree: Bool) async throws

    func cancelRequest(requestId: String) async throws

    func kickUserOffSeatByAdmin(seatIndex: Int, userId: String) async throws

    /****************************************** User Business *******************************************/
    func getUserInfo(userId: String) async throws -> TUIUserInfo

    /****************************************** Media Business *******************************************/
    func openLocalMicrophone() async throws

    func closeLocalMicrophone()

    func openLocalCamera(isFront: Bool, quality: TUIVideoQuality) async throws

    func closeLocalCamera()

    func switchCamera(isFrontCamera: Bool)
    
    func updateVideoQualityEx(streamType: TUIVideoStreamType, params: TUIRoomVideoEncoderParams)

    func setLocalVideoView(_ view: UIView?)

    func setRemoteVideoView(userId: String, streamType: TUIVideoStreamType, videoView: UIView)

    func startPlayRemoteVideo(userId: String, streamType: TUIVideoStreamType, onLoading: @escaping TUIPlayOnLoadingBlock) async throws -> String
    
    func stopPlayRemoteVideo(userId: String, streamType: TUIVideoStreamType)

    func muteLocalAudio()

    func unMuteLocalAudio() async throws

    func enableGravitySensor(enable: Bool)

    func setVideoResolutionMode(_ resolutionMode: TUIResolutionMode)

    func setBeautyStyle(_ style: TXBeautyStyle)

    /****************************************** DATA REPORT *******************************************/
    func callExperimentalAPI(jsonStr: String)

    /****************************************** TRTC Cloud *******************************************/
    func getTRTCCloud() -> TRTCCloud

    /***************************************** Plugin - Connection ******************************************/
    func requestConnection(roomIdList: [String], timeout: Int, extensionInfo: String) async throws -> [String: TUIConnectionCode]

    func acceptConnection(roomId: String) async throws

    func rejectConnection(roomId: String) async throws

    func disconnect() async throws

    func cancelConnectionRequest(list: [String]) async throws
}
