//
//  RoomEngineObserver.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/11/6.
//

import RTCRoomEngine

class RoomEngineObserver: NSObject, TUIRoomObserver {
    private(set) weak var context: LiveStreamManager.Context?
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        super.init()
    }
    
    func onRoomDismissed(roomId: String, reason: TUIRoomDismissedReason) {
        LiveStreamLog.info("\(#file)","\(#line)","onRoomDismissed:[roomId:\(roomId),reason:\(reason)]")
        context?.roomManager.onRoomDismissed(roomId: roomId)
        Task {
            await context?.observers.notifyObservers { observer in
                observer.onRoomDismissed(roomId: roomId)
            }
        }
    }
    
    func onRoomUserCountChanged(roomId: String, userCount: Int) {
        LiveStreamLog.info("\(#file)","\(#line)","onRoomUserCountChanged:[roomId:\(roomId),userCount:\(userCount)]")
    }
    
    func onSeatListChanged(seatList: [TUISeatInfo], seated seatedList: [TUISeatInfo], left leftList: [TUISeatInfo]) {
        LiveStreamLog.info("\(#file)","\(#line)",
                             "onSeatListChanged:[seatList:\(seatList),seatedList:\(seatedList),leftList:\(leftList)]")
        context?.coGuestManager.onSeatListChanged(seatList: seatList, seated: seatedList, left: leftList)
        let seatUserList: [TUIUserInfo] = seatList.filter { !($0.userId?.isEmpty ?? false) }.map { LiveStreamConvert.convertToUserInfo(seatInfo: $0) }
        let seatedUserList: [TUIUserInfo] = seatedList.map { LiveStreamConvert.convertToUserInfo(seatInfo: $0)}
        let leftUserList: [TUIUserInfo] = leftList.map { LiveStreamConvert.convertToUserInfo(seatInfo: $0)}
        Task {
            await context?.observers.notifyObservers { observer in
                observer.onConnectedUsersUpdated(userList: seatUserList, joinList: seatedUserList, leaveList: leftUserList)
                
                let userInfos = leftList
                    .filter { !isSelfInfo(seatInfo: $0) }
                    .map { LiveStreamConvert.convertToUserInfo(seatInfo: $0) }
                userInfos.forEach { observer.onUserConnectionExited(userInfo: $0) }
            }
        }
    }
    
    func onRequestReceived(request: TUIRequest) {
        LiveStreamLog.info("\(#file)","\(#line)","onRequestReceived:[request:\(request)]")
        context?.coGuestManager.onRequestReceived(request: request)
        if request.requestAction == .takeSeat {
            Task {
                await context?.observers.notifyObservers { observer in
                    let inviterUser = LiveStreamConvert.convertToUserInfo(request: request)
                    observer.onUserConnectionRequest(inviterUser: inviterUser)
                }
            }
        }
    }
    
    func onRequestCancelled(request: TUIRequest, operateUser: TUIUserInfo) {
        LiveStreamLog.info("\(#file)","\(#line)","onRequestCancelled:[request:\(request),operateUser:\(operateUser)]")
        context?.coGuestManager.onRequestCancelled(request: request, operateUser: operateUser)
        if request.requestAction == .takeSeat {
            Task {
                await context?.observers.notifyObservers { observer in
                    observer.onUserConnectionCancelled(inviterUser: operateUser)
                }
            }
        }
    }
    
    func onRequestProcessed(request: TUIRequest, operateUser: TUIUserInfo) {
        LiveStreamLog.info("\(#file)","\(#line)","onRequestProcessed:[request:\(request),operateUser:\(operateUser)]")
        context?.coGuestManager.onRequestProcessed(request: request, operateUser: operateUser)
    }
    
    func onKickedOffSeat(seatIndex: Int, operateUser: TUIUserInfo) {
        LiveStreamLog.info("\(#file)","\(#line)","onKickedOffSeat:[seatIndex:\(seatIndex),operateUser:\(operateUser)]")
        Task {
            await context?.observers.notifyObservers { observer in
                observer.onUserConnectionTerminated()
            }
        }
    }
    
    func onUserAudioStateChanged(userId: String, hasAudio: Bool, reason: TUIChangeReason) {
        LiveStreamLog.info("\(#file)","\(#line)",
                           "onUserAudioStateChanged:[userId:\(userId),hasAudio:\(hasAudio),reason:\(reason)]")
        context?.userManager.onUserAudioStateChanged(userId: userId, hasAudio: hasAudio, reason: reason)
    }
    
    func onUserVideoStateChanged(userId: String, streamType: TUIVideoStreamType, hasVideo: Bool, reason: TUIChangeReason) {
        LiveStreamLog.info("\(#file)","\(#line)",
                           "onUserVideoStateChanged:[userId:\(userId),streamType:\(streamType),hasVideo:\(hasVideo),reason:\(reason)]")
        context?.userManager.onUserVideoStateChanged(userId: userId, hasVideo: hasVideo, reason: reason)
    }
    
    func onRemoteUserEnterRoom(roomId: String, userInfo: TUIUserInfo) {
        LiveStreamLog.info("\(#file)","\(#line)","onRemoteUserEnterRoom:[roomId:\(roomId),userInfo:\(userInfo)]")
    }
    
    func onRemoteUserLeaveRoom(roomId: String, userInfo: TUIUserInfo) {
        LiveStreamLog.info("\(#file)","\(#line)","onRemoteUserLeaveRoom:[roomId:\(roomId),userInfo:\(userInfo)]")
    }
    
    func onKickedOffLine(message: String) {
        LiveStreamLog.info("\(#file)","\(#line)","onKickedOffLine:[message:\(message)]")
    }
    
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
        LiveStreamLog.info("\(#file)","\(#line)","onKickedOutOfRoom:[roomId:\(roomId),reason:\(reason),message:\(message)]")
    }
    
    func onUserInfoChanged(userInfo: TUIUserInfo, modifyFlag: TUIUserInfoModifyFlag) {
        context?.userManager.onUserInfoChanged(userInfo: userInfo, modifyFlag: modifyFlag)
    }
}

// MARK: - Private
extension RoomEngineObserver {
    private func isSelfInfo(seatInfo: TUISeatInfo) -> Bool {
        guard let context = context else { return false }
        if context.userManager.userState.selfInfo.userId.isEmpty {
            return false
        }
        return seatInfo.userId == context.userManager.userState.selfInfo.userId
    }
}
