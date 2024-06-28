//
//  BottomMenuViewDataHelper.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/20.
//

import Foundation
import RTCRoomEngine


class BottomMenuViewDataHelper {
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func generateBottomMenuData(store: LiveStore, routerStore: RouterStore, viewStore: VoiceRoomViewStore) -> [ButtonMenuInfo] {
        if store.selectCurrent(UserSelectors.isOwner) {
            return ownerBottomMenu(store: store, routerStore: routerStore)
        } else {
            return memberBottomMenu(store: store, routerStore: routerStore, viewStore: viewStore)
        }
    }
    
    func generateOperateSeatMenuData(store: LiveStore, routerStore: RouterStore, seat: SeatInfo) -> [ActionItem] {
        if store.selectCurrent(UserSelectors.isOwner) {
            return generateRoomOwnerOperateSeatMenuData(store: store, routerStore: routerStore, seat: seat)
        } else {
            return generateNormalUserOperateSeatMenuData(store: store, routerStore: routerStore, seat: seat)
        }
    }
}

extension BottomMenuViewDataHelper {
    private func generateRoomOwnerOperateSeatMenuData(store: LiveStore,routerStore: RouterStore, seat: SeatInfo) -> [ActionItem] {
        var menus: [ActionItem] = []
        if seat.userId.isEmpty {
            let lockSeat = ActionItem(title: seat.isLocked ? String.unLockSeat : String.lockSeat)
            lockSeat.actionClosure = { _ in
                let lockSeat = TUISeatLockParams()
                lockSeat.lockAudio = seat.isAudioLocked
                lockSeat.lockVideo = seat.isVideoLocked
                lockSeat.lockSeat = !seat.isLocked
                store.dispatch(action: SeatActions.lockSeat(payload: (seat.index, lockSeat)))
                routerStore.router(action: .dismiss)
            }
            menus.append(lockSeat)
            return menus
        }
        
        let isSelf = seat.userId == store.selectCurrent(UserSelectors.currentUserId)
        if isSelf {
            let audioStreamMutedSelector = Selector<OperationState, Bool>(keyPath: \OperationState.mediaState.isMicrophoneMuted)
            let isLocalAudioMuted = store.selectCurrent(audioStreamMutedSelector)
            let title = isLocalAudioMuted ? String.unmuteMicrophone : String.muteMicrophone
            let lockAudio = ActionItem(title: title)
            lockAudio.actionClosure = { _ in
                store.dispatch(action: MediaActions.operateMicrophoneMute(payload: !isLocalAudioMuted))
                routerStore.router(action: .dismiss)
            }
            menus.append(lockAudio)
            
        } else {
            let title = seat.isAudioLocked ? String.unmuteAudio : String.muteAudio
            let lockAudio = ActionItem(title: title)
            lockAudio.actionClosure = { _ in
                let lockSeat = TUISeatLockParams()
                lockSeat.lockAudio = !seat.isAudioLocked
                lockSeat.lockVideo = seat.isVideoLocked
                lockSeat.lockSeat = seat.isLocked
                store.dispatch(action: SeatActions.lockSeat(payload: (seat.index, lockSeat)))
                routerStore.router(action: .dismiss)
            }
            menus.append(lockAudio)
            
            let kickSeat = ActionItem(title: .kickSeat)
            kickSeat.actionClosure = {  _ in
                store.dispatch(action: SeatActions.kickSeat(payload: seat))
                routerStore.router(action: .dismiss)
            }
            menus.append(kickSeat)
        }
        return menus
    }
    
    
    private func generateNormalUserOperateSeatMenuData(store: LiveStore, routerStore: RouterStore, seat: SeatInfo) -> [ActionItem] {
        var menus: [ActionItem] = []
        let isOnSeat = store.selectCurrent(UserSelectors.isOnSeat)
        if seat.userId.isEmpty && !isOnSeat && !seat.isLocked {
            let takeSeat = ActionItem(title: .applySeat)
            takeSeat.actionClosure = { _ in
                store.dispatch(action: SeatActions.takeSeat(payload: seat.index))
                routerStore.router(action: .dismiss)
            }
            menus.append(takeSeat)
            return menus
        }
        
        let isSelf = seat.userId == store.selectCurrent(UserSelectors.currentUserId)
        if isSelf {
            let audioStreamMutedSelector = Selector<OperationState, Bool>(keyPath: \OperationState.mediaState.isMicrophoneMuted)
            let isLocalAudioMuted = store.selectCurrent(audioStreamMutedSelector)
            let title = isLocalAudioMuted ? String.unmuteMicrophone : String.muteMicrophone
            let lockAudio = ActionItem(title: title)
            lockAudio.actionClosure = { _ in
                store.dispatch(action: MediaActions.operateMicrophoneMute(payload: !isLocalAudioMuted))
                routerStore.router(action: .dismiss)
            }
            menus.append(lockAudio)
            
            let leaveSeat = ActionItem(title: .leaveSeat)
            leaveSeat.actionClosure = {  _ in
                store.dispatch(action: SeatActions.leaveSeat())
                routerStore.router(action: .dismiss)
            }
            menus.append(leaveSeat)
        }
        return menus
    }
}

// MARK: - Bottom menu data function
extension BottomMenuViewDataHelper {
    private func ownerBottomMenu(store: LiveStore, routerStore: RouterStore) -> [ButtonMenuInfo] {
        var menus: [ButtonMenuInfo] = []
        var linkMic = ButtonMenuInfo(normalIcon: "live_link_voice_room", normalTitle: "")
        linkMic.tapAction = { sender in
            routerStore.router(action: .present(.linkControl))
        }
        
        linkMic.bindStateClosure = { button, cancellableSet in
            store
                .select(SeatSelectors.getSeatApplicationCount)
                .receive(on: RunLoop.main)
                .map { count in
                    return count == 0
                }
                .assign(to: \MenuButton.redDot.isHidden, on: button)
                .store(in: &cancellableSet)
        }
        menus.append(linkMic)
        var audioEffect = ButtonMenuInfo(normalIcon: "live_music_icon")
        audioEffect.tapAction = { sender in
            routerStore.router(action: .present(.audioEffect))
        }
        menus.append(audioEffect)
        var musicList = ButtonMenuInfo(normalIcon: "live_music_list")
        musicList.tapAction = { sender in
            routerStore.router(action: .present(.musicList))
        }
        menus.append(musicList)
        return menus
    }
    
    private func memberBottomMenu(store: LiveStore, routerStore: RouterStore, viewStore: VoiceRoomViewStore) -> [ButtonMenuInfo] {
        var menus: [ButtonMenuInfo] = []
        var linkMic = ButtonMenuInfo(normalIcon: "live_connection_icon", selectIcon: "live_linking_icon")
        linkMic.tapAction = { sender in
            if sender.isSelected {
                let requestId = store.selectCurrent(SeatSelectors.getMySeatApplicationId)
                // cancel
                store.dispatch(action: SeatActions.cancelApplication(payload: requestId))
            } else {
                if store.selectCurrent(UserSelectors.isOnSeat) {
                    store.dispatch(action: SeatActions.leaveSeat())
                } else {
                    // request
                    store.dispatch(action: SeatActions.takeSeat(payload: nil))
                }
            }
        }
        linkMic.bindStateClosure = { button, cancellableSet in
            store
                .select(SeatSelectors.getMySeatApplicationId)
                .receive(on: RunLoop.main)
                .map { !$0.isEmpty }
                .assign(to: \MenuButton.isSelected, on: button)
                .store(in: &cancellableSet)
            store
                .select(UserSelectors.isOnSeat)
                .receive(on: RunLoop.main)
                .sink { isOnSeat in
                    let imageName = isOnSeat ? "live_linked_icon" : "live_connection_icon"
                    button.setImage(.liveBundleImage(imageName), for: .normal)
                }
                .store(in: &cancellableSet)
        }
        menus.append(linkMic)
        var gift = ButtonMenuInfo(normalIcon: "live_gift_icon", normalTitle: "")
        gift.tapAction = { sender in
            routerStore.router(action: .present(.giftView))
        }
        menus.append(gift)
        var like = ButtonMenuInfo(normalIcon: "live_like_icon")
        like.tapAction = { sender in
            viewStore.dispatch(action: VoiceRoomViewResponseActions.like())
        }
        menus.append(like)
        return menus
    }
}

fileprivate extension String {
    static let unLockSeat = localized("live.seat.unLockSeat")
    static let lockSeat = localized("live.seat.lockSeat")
    static let unmuteMicrophone = localized("live.seat.unmuteMicrophone")
    static let muteMicrophone = localized("live.seat.muteMicrophone")
    static let unmuteAudio = localized("live.seat.unmuteAudio")
    static let muteAudio = localized("live.seat.muteAudio")
    static let kickSeat = localized("live.seat.kickSeat")
    static let applySeat = localized("live.seat.applySeat")
    static let leaveSeat = localized("live.seat.leaveSeat")
}
