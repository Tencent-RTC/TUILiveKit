//
//  LiveConnectionObserver.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/11/11.
//

import RTCRoomEngine

class LiveConnectionObserver: NSObject, TUILiveConnectionObserver {
    private(set) weak var context: LiveStreamManager.Context?
    
    init(context: LiveStreamManager.Context) {
        self.context = context
        super.init()
    }
    
    func onConnectionUserListChanged(connectedList: [TUIConnectionUser], joinedList: [TUIConnectionUser], leavedList: [TUIConnectionUser]) {
        LiveStreamLog.info("\(#file)","\(#line)",
                           "onConnectionUserListChanged:[connectedList:\(connectedList),joinedList:\(joinedList),leavedList:\(leavedList)]")
        context?.coHostManager.onConnectionUserListChanged(connectedList: connectedList, joinedList: joinedList, leavedList: leavedList)
        Task {
            await context?.observers.notifyObservers { observer in
                let connectedList = connectedList.filter { user in
                    if let context = context {
                        return !user.userId.hasSuffix(context.roomManager.mixStreamIdSuffix)
                    }
                    return true
                }
                observer.onConnectedRoomsUpdated(hostUserList: connectedList)
            }
        }
        Task {
            for user in leavedList {
                await context?.observers.notifyObservers { observer in
                    observer.onCrossRoomConnectionExited(hostUser: user)
                }
            }
        }
    }
    
    func onConnectionRequestReceived(inviter: TUIConnectionUser, inviteeList: [TUIConnectionUser], extensionInfo: String) {
        LiveStreamLog.info("\(#file)","\(#line)",
                           "onConnectionRequestReceived:[inviter:\(inviter),inviteeList:\(inviteeList),extensionInfo:\(extensionInfo)]")
        context?.coHostManager.onConnectionRequestReceived(inviter: inviter)
        Task {
            await context?.observers.notifyObservers { observer in
                observer.onCrossRoomConnectionRequest(hostUser: inviter)
            }
        }
    }
    
    func onConnectionRequestCancelled(inviter: TUIConnectionUser) {
        LiveStreamLog.info("\(#file)","\(#line)","onConnectionRequestCancelled:[inviter:\(inviter)]")
        context?.coHostManager.onConnectionRequestCancelled(inviter: inviter)
        Task {
            await context?.observers.notifyObservers { observer in
                observer.onCrossRoomConnectionCancelled(hostUser: inviter)
            }
        }
    }
    
    func onConnectionRequestAccept(invitee: TUIConnectionUser) {
        LiveStreamLog.info("\(#file)","\(#line)","onConnectionRequestAccept:[invitee:\(invitee)]")
        context?.coHostManager.onConnectionRequestAccept(invitee: invitee)
        Task {
            await context?.observers.notifyObservers { observer in
                observer.onCrossRoomConnectionAccepted(hostUser: invitee)
            }
        }
    }
    
    func onConnectionRequestReject(invitee: TUIConnectionUser) {
        LiveStreamLog.info("\(#file)","\(#line)","onConnectionRequestReject:[invitee:\(invitee)]")
        context?.coHostManager.onConnectionRequestReject(invitee: invitee)
        Task {
            await context?.observers.notifyObservers { observer in
                observer.onCrossRoomConnectionRejected(hostUser: invitee)
            }
        }
    }
    
    func onConnectionRequestTimeout(inviter: TUIConnectionUser, invitee: TUIConnectionUser) {
        LiveStreamLog.info("\(#file)","\(#line)","onConnectionRequestTimeout:[inviter:\(inviter),invitee:\(invitee)]")
        context?.coHostManager.onConnectionRequestTimeout(inviter: inviter, invitee: invitee)
        Task {
            await context?.observers.notifyObservers { observer in
                observer.onCrossRoomConnectionTimeout(inviter: inviter, invitee: invitee)
            }
        }
    }
    
    
}
