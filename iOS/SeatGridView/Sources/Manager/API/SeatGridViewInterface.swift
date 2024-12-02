//
//  SeatGridViewInterface.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/17.
//

import Foundation
import RTCRoomEngine

typealias SentRequestResultClosure = ((TUIRequest) -> Void)
protocol SeatGridViewInterface {
    func addRoomEngineObserver(_ observer: TUIRoomObserver)
    func removeRoomEngineObserver(_ observer: TUIRoomObserver)
    
    func createRoom(roomInfo: TUIRoomInfo) async throws
    func destroyRoom() async throws
    func enterRoom(roomId: String) async throws -> TUIRoomInfo
    func exitRoom() async throws
    func getLiveInfo(roomId: String) async throws -> TUILiveInfo
    func setLiveInfo(liveInfo: TUILiveInfo, modifyFlag: TUILiveModifyFlag) async throws
    func updateRoomSeatByModeByAdmin(seatMode: TUISeatMode) async throws
   
    func takeSeat(seatIndex: Int,timeout: TimeInterval, requestCallback: SentRequestResultClosure) async throws -> SGTakeSeatResultWithId
    func takeUserOnSeatByAdmin(seatIndex: Int,
                               userId: String,
                               timeout: TimeInterval,
                               requestCallback: SentRequestResultClosure) async throws -> SGTakeSeatResultWithId
    func leaveSeat() async throws
    func moveSeat(index: Int) async throws
    func lockSeat(index: Int, lockMode: TUISeatLockParams) async throws
    func getSeatList() async throws -> [TUISeatInfo]
    func responseRemoteRequest(requestId: String, agree: Bool) async throws
    func cancelRequest(requestId: String)  async throws
    func kickUserOffSeatByAdmin(userId: String) async throws
    
    func getUserList() async throws -> [TUIUserInfo]
    func getUserInfo(userId: String) async throws -> TUIUserInfo
    
    func openLocalMicrophone() async throws
    func closeLocalMicrophone()
    func muteLocalAudio()
    func unmuteLocalAudio() async throws
    func updateAudioQuality(quality: TUIAudioQuality)
    
    func getSelfInfo() -> TUILoginUserInfo
    
    func callExperimentalAPI(jsonString: String)
}
