//
//  RoomEngineObserver.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/17.
//

import Foundation
import RTCRoomEngine

class SGRoomEngineObserver: NSObject, TUIRoomObserver {
    private(set) weak var context: SeatGridViewManager.Context?
    
    init(context: SeatGridViewManager.Context) {
        self.context = context
        super.init()
        TUIRoomEngine.sharedInstance().addObserver(self)
    }
    
    func onRoomDismissed(roomId: String, reason: TUIRoomDismissedReason) {
        guard let context = self.context else { return }
        VRLog.info("\(#file)","\(#line)","onRoomDismissed:[roomId:\(roomId),reason:\(reason)]")
        Task {
            await context.observers.notifyObservers(callback: { observer in
                observer.onRoomDismissed(roomId: roomId)
            })
        }
        
    }
    
    func onKickedOutOfRoom(roomId: String, reason: TUIKickedOutOfRoomReason, message: String) {
        guard let context = self.context else { return }
        VRLog.info("\(#file)","\(#line)","onKickedOutOfRoom:[roomId:\(roomId),reason:\(reason)]")
        Task {
            await context.observers.notifyObservers { observer in
                observer.onKickedOutOfRoom(roomId: roomId, reason: reason, message: message)
            }
        }
    }
    
    func onRequestReceived(request: TUIRequest) {
        guard let context = self.context else { return }
        VRLog.info("\(#file)","\(#line)","onRequestReceived:[request:\(request)]")
        context.seatManager.onRequestReceived(request: request)
        guard let requestType = SGRequestType.init(action: request.requestAction) else { return }
        Task {
            let userInfo = TUIUserInfo()
            userInfo.userId = request.userId
            userInfo.userName = request.userName
            userInfo.avatarUrl = request.avatarUrl
            await context.observers.notifyObservers(callback: { observer in
                observer.onSeatRequestReceived(type: requestType, userInfo: userInfo)
            })
        }
    }
    
    func onRequestCancelled(request: TUIRequest, operateUser: TUIUserInfo) {
        guard let context = self.context else { return }
        VRLog.info("\(#file)","\(#line)","onRequestCancelled:[request:\(request),operateUser:\(operateUser)]")
        context.seatManager.onRequestCancelled(request: request, operateUser: operateUser)
        guard let requestType = SGRequestType.init(action: request.requestAction) else { return }
        Task {
            let userInfo = TUIUserInfo()
            userInfo.userId = request.userId
            userInfo.userName = request.userName
            userInfo.avatarUrl = request.avatarUrl
            await context.observers.notifyObservers(callback: { observer in
                observer.onSeatRequestCancelled(type: requestType, userInfo: userInfo)
            })
        }
    }
    
    func onKickedOffSeat(seatIndex: Int, operateUser: TUIUserInfo) {
        guard let context = self.context else { return }
        VRLog.info("\(#file)","\(#line)","onKickedOffSeat:[seatIndex:\(seatIndex),operateUser:\(operateUser)]")
        Task {
            await context.observers.notifyObservers { observer in
                observer.onKickedOffSeat(userInfo: operateUser)
            }
        }
    }
    
    
    func onSeatListChanged(seatList: [TUISeatInfo], seated seatedList: [TUISeatInfo], left leftList: [TUISeatInfo]) {
        VRLog.info("\(#file)","\(#line)",
                             "onSeatListChanged:[seatList:\(seatList),seatedList:\(seatedList),leftList:\(leftList)]")
        context?.viewManager.onSeatCountChanged(seatCount: seatList.count)
        context?.seatManager.onSeatListChanged(seatList: seatList, seated: seatedList, left: leftList)
    }
    
    func onUserAudioStateChanged(userId: String, hasAudio: Bool, reason: TUIChangeReason) {
        VRLog.info("\(#file)","\(#line)",
                   "onUserAudioStateChanged:[userId:\(userId),hasAudio:\(hasAudio),reason:\(reason)]")
        guard let context = self.context else { return }
        Task {
            var userInfo = TUIUserInfo()
            userInfo.userId = userId
            userInfo = try await context.service.getUserInfo(userId: userId)
            await context.observers.notifyObservers(callback: { observer in
                observer.onUserAudioStateChanged(userInfo: userInfo, hasAudio: hasAudio, reason: reason)
            })
        }
        context.userManager.onUserAudioStateChanged(userId: userId, hasAudio: hasAudio)
    }
    
    func onUserVoiceVolumeChanged(volumeMap: [String : NSNumber]) {
        guard let context = self.context else { return }
        let map = volumeMap.mapValues { $0.intValue }
        context.userManager.onUserVoiceVolumeChanged(volumeMap: map)
    }
    
    deinit {
        TUIRoomEngine.sharedInstance().removeObserver(self)
        debugPrint("deinit:\(self)")
    }
}


extension SGRequestType {
    init?(action: TUIRequestAction) {
        switch action {
            case .takeSeat:
                self.init(rawValue: SGRequestType.applyToTakeSeat.rawValue)
            case .remoteUserOnSeat:
                self.init(rawValue: SGRequestType.inviteToTakeSeat.rawValue)
            default:
                return nil
        }
    }
}
