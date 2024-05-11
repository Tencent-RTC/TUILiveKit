//
//  MenuDataCreator.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/20.
//

import Foundation
import RTCRoomEngine

class MenuDataCreator {
    
    @WeakLazyInjected var store: VoiceRoomStoreProvider?
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func generateBottomMenuData() -> [ButtonMenuInfo] {
        guard let store = self.store else { return [] }
        if store.selectCurrent(UserSelectors.isOwner) {
            return ownerBottomMenu()
        } else {
            return memberBottomMenu()
        }
    }
    
    func generateOperateSeatMenuData(seat: SeatInfo) -> [ListMenuInfo] {
        guard let store = self.store else { return [] }
        if store.selectCurrent(UserSelectors.isOwner) {
            return generateRoomOwnerOperateSeatMenuData(seat: seat)
        } else {
            return generateNormalUserOperateSeatMenuData(seat: seat)
        }
    }
}

extension MenuDataCreator {
    private func generateRoomOwnerOperateSeatMenuData(seat: SeatInfo) -> [ListMenuInfo] {
        guard let store = self.store else { return [] }
        var menus: [ListMenuInfo] = []
        if seat.userId.isEmpty {
            var lockSeat = ListMenuInfo(title: seat.isLocked ? String.unLockSeat : String.lockSeat)
            lockSeat.tapAction = { _ in
                guard let store = self.store else { return }
                let lockSeat = TUISeatLockParams()
                lockSeat.lockAudio = seat.isAudioLocked
                lockSeat.lockVideo = seat.isVideoLocked
                lockSeat.lockSeat = !seat.isLocked
                store.dispatch(action: SeatActions.lockSeat(payload: (seat.index, lockSeat)))
                store.dispatch(action: NavigatorActions.navigatorTo(payload: .main))
            }
            menus.append(lockSeat)
            return menus
        }
        
        let isSelf = seat.userId == store.selectCurrent(UserSelectors.currentUserId)
        if isSelf {
            let audioStreamMutedSelector = Selector<OperationState, Bool>(keyPath: \OperationState.mediaState.isLocalAudioStreamMuted)
            let isLocalAudioMuted = store.selectCurrent(audioStreamMutedSelector)
            let title = isLocalAudioMuted ? String.unmuteMicrophone : String.muteMicrophone
            var lockAudio = ListMenuInfo(title: title)
            lockAudio.tapAction = { _ in
                guard let store = self.store else { return }
                store.dispatch(action: MediaActions.operateLocalAudioMute(payload: !isLocalAudioMuted))
                store.dispatch(action: NavigatorActions.navigatorTo(payload: .main))
            }
            menus.append(lockAudio)
            
        } else {
            let title = seat.isAudioLocked ? String.unmuteAudio : String.muteAudio
            var lockAudio = ListMenuInfo(title: title)
            lockAudio.tapAction = { _ in
                guard let store = self.store else { return }
                let lockSeat = TUISeatLockParams()
                lockSeat.lockAudio = !seat.isAudioLocked
                lockSeat.lockVideo = seat.isVideoLocked
                lockSeat.lockSeat = seat.isLocked
                store.dispatch(action: SeatActions.lockSeat(payload: (seat.index, lockSeat)))
                store.dispatch(action: NavigatorActions.navigatorTo(payload: .main))
            }
            menus.append(lockAudio)
            
            var kickSeat = ListMenuInfo(title: .kickSeat)
            kickSeat.tapAction = {  _ in
                guard let store = self.store else { return }
                store.dispatch(action: SeatActions.kickSeat(payload: seat))
                store.dispatch(action: NavigatorActions.navigatorTo(payload: .main))
            }
            menus.append(kickSeat)
        }
        return menus
    }
    
    
    private func generateNormalUserOperateSeatMenuData(seat: SeatInfo) -> [ListMenuInfo] {
        guard let store = self.store else { return [] }
        var menus: [ListMenuInfo] = []
        let isOnSeat = store.selectCurrent(UserSelectors.isOnSeat)
        if seat.userId.isEmpty && !isOnSeat && !seat.isLocked {
            var takeSeat = ListMenuInfo(title: .applySeat)
            takeSeat.tapAction = { _ in
                guard let store = self.store else { return }
                store.dispatch(action: SeatActions.takeSeat(payload: seat.index))
                store.dispatch(action: NavigatorActions.navigatorTo(payload: .main))
            }
            menus.append(takeSeat)
            return menus
        }
        
        let isSelf = seat.userId == store.selectCurrent(UserSelectors.currentUserId)
        if isSelf {
            let audioStreamMutedSelector = Selector<OperationState, Bool>(keyPath: \OperationState.mediaState.isLocalAudioStreamMuted)
            let isLocalAudioMuted = store.selectCurrent(audioStreamMutedSelector)
            let title = isLocalAudioMuted ? String.unmuteMicrophone : String.muteMicrophone
            var lockAudio = ListMenuInfo(title: title)
            lockAudio.tapAction = { _ in
                guard let store = self.store else { return }
                store.dispatch(action: MediaActions.operateLocalAudioMute(payload: !isLocalAudioMuted))
                store.dispatch(action: NavigatorActions.navigatorTo(payload: .main))
            }
            menus.append(lockAudio)
            
            var leaveSeat = ListMenuInfo(title: .leaveSeat)
            leaveSeat.tapAction = {  _ in
                guard let store = self.store else { return }
                store.dispatch(action: SeatActions.leaveSeat())
                store.dispatch(action: NavigatorActions.navigatorTo(payload: .main))
            }
            menus.append(leaveSeat)
        }
        return menus
    }
}

// MARK: - Bottom menu data function
extension MenuDataCreator {
    private func ownerBottomMenu() -> [ButtonMenuInfo] {
        var menus: [ButtonMenuInfo] = []
        var linkMic = ButtonMenuInfo(normalIcon: "live_link_icon", normalTitle: "")
        linkMic.tapAction = { sender in
            guard let store = self.store else { return }
            
            store.dispatch(action: NavigatorActions.navigatorTo(payload: .seatApplication))
        }
        
        linkMic.bindStateClosure = { button, cancellableSet in
            guard let store = self.store else { return }
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
            guard let store = self.store else { return }
            store.dispatch(action: NavigatorActions.navigatorTo(payload: .audioEffectPanel))
        }
        menus.append(audioEffect)
        var musicList = ButtonMenuInfo(normalIcon: "live_music_list")
        musicList.tapAction = { sender in
            guard let store = self.store else { return }
            store.dispatch(action: NavigatorActions.navigatorTo(payload: .musicListPanel))
        }
        menus.append(musicList)
        return menus
    }
    
    private func memberBottomMenu() -> [ButtonMenuInfo] {
        var menus: [ButtonMenuInfo] = []
        var linkMic = ButtonMenuInfo(normalIcon: "live_connection_icon", selectIcon: "live_linking_icon")
        linkMic.tapAction = { sender in
            guard let store = self.store else { return }
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
            guard let store = self.store else { return }
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
            guard let store = self.store else { return }
            store.dispatch(action: NavigatorActions.navigatorTo(payload: .giftList))
        }
        menus.append(gift)
        var like = ButtonMenuInfo(normalIcon: "live_like_icon")
        like.tapAction = { sender in
            guard let store = self.store else { return }
            store.dispatch(action: ViewActions.like())
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
