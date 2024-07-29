//
//  VoiceRoomRootMenuDataHelper.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/20.
//

import Foundation
import RTCRoomEngine

class VoiceRoomRootMenuDataHelper {
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
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
}

extension VoiceRoomRootMenuDataHelper {
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
        if !isSelf {
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
        }
        return menus
    }
    
    private func generateNormalUserOperateSeatMenuData(store: LiveStore, routerStore: RouterStore, seat: SeatInfo) -> [ActionItem] {
        var menus: [ActionItem] = []
        let isOnSeat = store.selectCurrent(UserSelectors.isOnSeat)
        if seat.userId.isEmpty && !isOnSeat && !seat.isLocked {
            let takeSeat = ActionItem(title: .takeSeat)
            takeSeat.actionClosure = { _ in
                store.dispatch(action: SeatActions.takeSeat(payload: seat.index))
                routerStore.router(action: .dismiss)
            }
            menus.append(takeSeat)
            return menus
        }
        return menus
    }
}

// MARK: - Bottom menu data function
extension VoiceRoomRootMenuDataHelper {
    private func ownerBottomMenu(store: LiveStore, routerStore: RouterStore) -> [ButtonMenuInfo] {
        var menus: [ButtonMenuInfo] = []
        
        var setting = ButtonMenuInfo(normalIcon: "live_anchor_setting_icon")
        setting.tapAction = { sender in
            let settingItems = self.generateOwnerSettingModel(routerStore: routerStore)
            routerStore.router(action: .present(.featureSetting(settingItems)))
        }
        menus.append(setting)
        
        var musicList = ButtonMenuInfo(normalIcon: "live_gift_icon")
        musicList.tapAction = { sender in
            routerStore.router(action: .present(.giftView))
        }
        menus.append(musicList)
        
        var linkMic = ButtonMenuInfo(normalIcon: "live_link_voice_room", normalTitle: "")
        linkMic.tapAction = { sender in
            routerStore.router(action: .present(.voiceLinkControl))
        }
        
        linkMic.bindStateClosure = { button, cancellableSet in
            store
                .select(SeatSelectors.getSeatApplicationCount)
                .receive(on: RunLoop.main)
                .sink(receiveValue: { applicationCount in
                    button.updateDotCount(count: applicationCount)
                })
                .store(in: &cancellableSet)
        }
        menus.append(linkMic)
        return menus
    }
    
    private func generateOwnerSettingModel(routerStore: RouterStore) -> FeatureClickPanelModel {
        let model = FeatureClickPanelModel()
        model.itemSize = CGSize(width: 56.scale375(), height: 76.scale375())
        model.itemDiff = 44.scale375()
        let designConfig = FeatureItemDesignConfig()
        designConfig.backgroundColor = .g3
        designConfig.cornerRadius = 10
        designConfig.titleFont = .customFont(ofSize: 12)
        designConfig.type = .imageAboveTitleBottom
        model.items.append(FeatureItem(title: .musicText,
                                       image: .liveBundleImage("live_setting_music_icon"),
                                       designConfig: designConfig,
                                       actionClosure: {
            routerStore.router(action: .present(.musicList))
        }))
        model.items.append(FeatureItem(title: .audioEffectsText,
                                       image: .liveBundleImage("live_setting_audio_effects"),
                                       designConfig: designConfig,
                                       actionClosure: {
            routerStore.router(action: .present(.audioEffect))
        }))
        return model
    }
    
    private func memberBottomMenu(store: LiveStore, routerStore: RouterStore, viewStore: VoiceRoomViewStore) -> [ButtonMenuInfo] {
        var menus: [ButtonMenuInfo] = []
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
        
        var linkMic = ButtonMenuInfo(normalIcon: "live_voice_room_link_icon", selectIcon: "live_voice_room_linking_icon")
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
                .sink { isEmpty in
                    button.isSelected = isEmpty
                    button.isSelected ? button.startRotate() : button.endRotate()
                }
                .store(in: &cancellableSet)
            store
                .select(UserSelectors.isOnSeat)
                .receive(on: RunLoop.main)
                .sink { isOnSeat in
                    let imageName = isOnSeat ? "live_linked_icon" : "live_voice_room_link_icon"
                    button.setImage(.liveBundleImage(imageName), for: .normal)
                }
                .store(in: &cancellableSet)
        }
        menus.append(linkMic)
        return menus
    }
}

fileprivate extension String {
    static let unLockSeat = localized("live.seat.unLockSeat")
    static let lockSeat = localized("live.seat.lockSeat")
    static let unmuteAudio = localized("live.seat.unmuteAudio")
    static let muteAudio = localized("live.seat.muteAudio")
    static let takeSeat = localized("live.seat.takeSeat")
    static let leaveSeat = localized("live.seat.leaveSeat")
    static let backgroundText: String = localized("live.anchor.setting.background")
    static let musicText: String = localized("live.category.music")
    static let audioEffectsText: String = localized("live.anchor.setting.audio.effects")
}
