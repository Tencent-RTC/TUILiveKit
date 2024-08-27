//
//  LiveRoomRootMenuDataCreator.swift
//  Alamofire
//
//  Created by aby on 2024/5/31.
//

import Foundation

class LiveRoomRootMenuDataCreator {
    func generateBottomMenuData(store: LiveStore, routerStore: RouterStore) -> [ButtonMenuInfo] {
        if store.selectCurrent(UserSelectors.isOwner) {
            return ownerBottomMenu(store: store, routerStore: routerStore)
        } else {
            return memberBottomMenu(store: store, routerStore: routerStore)
        }
    }
    
    func generateLinkTypeMenuData(store: LiveStore, routerStore: RouterStore) -> [LinkMicTypeCellData] {
        var data = [LinkMicTypeCellData]()
        data.append(LinkMicTypeCellData(image: .liveBundleImage("live_link_video"),
                                        text: .videoLinkRequestText,
                                        action: {
            self.applyToTakeSeat(store: store, needOpenCamera: true)
            routerStore.router(action: .dismiss())
        }))
        
        data.append(LinkMicTypeCellData(image: .liveBundleImage("live_link_audio"),
                                        text: .audioLinkRequestText,
                                        action: {
            self.applyToTakeSeat(store: store, needOpenCamera: false)
            routerStore.router(action: .dismiss())
        }))
        return data
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
}

extension LiveRoomRootMenuDataCreator {
    func ownerBottomMenu(store: LiveStore, routerStore: RouterStore) -> [ButtonMenuInfo] {
        var menus: [ButtonMenuInfo] = []
        var connection = ButtonMenuInfo(normalIcon: "live_connection_icon")
        connection.tapAction = { [weak self] sender in
            guard let self = self else { return }
            if store.selectCurrent(SeatSelectors.getSeatUserCount) > 1 {
                store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .connectDisableText)))
            } else {
                routerStore.router(action: .present(.connectionControl))
            }
        }
        connection.bindStateClosure = { button, cancellableSet in
            store
                .select(SeatSelectors.getSeatList)
                .receive(on: RunLoop.main)
                .sink { seatList in
                    let seatUserCount = seatList.filter({!$0.userId.isEmpty}).count
                    let imageName = seatUserCount > 1 ? "live_connection_disable_icon" : "live_connection_icon"
                    button.setImage(.liveBundleImage(imageName), for: .normal)
                }
                .store(in: &cancellableSet)
        }
        menus.append(connection)
        
        var linkMic = ButtonMenuInfo(normalIcon: "live_link_icon")
        linkMic.tapAction = { sender in
            if store.selectCurrent(ConnectionSelectors.isConnecting) {
                store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .linkMicDisableTExt)))
                return
            }
            routerStore.router(action: .present(.liveLinkControl))
        }
        linkMic.bindStateClosure = { button, cancellableSet in
            store
                .select(ConnectionSelectors.isConnecting)
                .receive(on: RunLoop.main)
                .sink { isConnecting in
                    let imageName = isConnecting ? "live_link_disable_icon" : "live_link_icon"
                    button.setImage(.liveBundleImage(imageName), for: .normal)
                }
                .store(in: &cancellableSet)
        }
        menus.append(linkMic)
        
        var setting = ButtonMenuInfo(normalIcon: "live_anchor_setting_icon")
        setting.tapAction = { sender in
            let settingModel = self.generateAnchorSettingModel(store: store,
                                                               routerStore: routerStore)
            routerStore.router(action: .present(.featureSetting(settingModel)))
        }
        menus.append(setting)
        
        var music = ButtonMenuInfo(normalIcon: "live_anchor_music_icon")
        music.tapAction = { sender in
            routerStore.router(action: .present(.musicList))
        }
        menus.append(music)
        return menus
    }
    
    func generateAnchorSettingModel(store: LiveStore, routerStore: RouterStore) -> FeatureClickPanelModel {
        let model = FeatureClickPanelModel()
        model.itemSize = CGSize(width: 56.scale375(), height: 76.scale375())
        model.itemDiff = 12.scale375()
        var designConfig = FeatureItemDesignConfig()
        designConfig.backgroundColor = .g3
        designConfig.cornerRadius = 10
        designConfig.titleFont = .customFont(ofSize: 12)
        designConfig.type = .imageAboveTitleBottom
        model.items.append(FeatureItem(normalTitle: .beautyText,
                                       normalImage: .liveBundleImage("live_video_setting_beauty"),
                                       designConfig: designConfig,
                                       actionClosure: { _ in
            routerStore.router(action: .present(.beauty))
        }))
        model.items.append(FeatureItem(normalTitle: .audioEffectsText,
                                       normalImage: .liveBundleImage("live_setting_audio_effects"),
                                       designConfig: designConfig,
                                       actionClosure: { _ in
            routerStore.router(action: .present(.audioEffect))
        }))
        model.items.append(FeatureItem(normalTitle: .flipText,
                                       normalImage: .liveBundleImage("live_video_setting_flip"),
                                       designConfig: designConfig,
                                       actionClosure: { _ in
            let isFrontCamera = store.selectCurrent(MediaSelectors.getFrontCameraState)
            store.dispatch(action: MediaActions.switchCamera(payload: isFrontCamera == true ? .rear : .front))
        }))
        model.items.append(FeatureItem(normalTitle: .mirrorText,
                                       normalImage: .liveBundleImage("live_video_setting_mirror"),
                                       designConfig: designConfig,
                                       actionClosure: { _ in
            let isMirror = store.selectCurrent(MediaSelectors.getMirrorState)
            store.dispatch(action: MediaActions.switchMirror(payload: isMirror == true ? false : true))
        }))
        model.items.append(FeatureItem(normalTitle: .videoParametersText,
                                       normalImage: .liveBundleImage("live_setting_video_parameters"),
                                       designConfig: designConfig,
                                       actionClosure: { _ in
            routerStore.router(action: .present(.videoSetting))
        }))
        return model
    }
    
    func memberBottomMenu(store: LiveStore, routerStore: RouterStore) -> [ButtonMenuInfo] {
        var menus: [ButtonMenuInfo] = []
        var gift = ButtonMenuInfo(normalIcon: "live_gift_icon", normalTitle: "")
        gift.tapAction = { sender in
            routerStore.router(action: .present(.giftView))
        }
        menus.append(gift)
        var linkMic = ButtonMenuInfo(normalIcon: "live_link_icon", selectIcon: "live_linking_icon")
        linkMic.tapAction = { sender in
            if store.selectCurrent(ConnectionSelectors.isConnecting) {
                store.dispatch(action: ViewActions.toastEvent(payload: ToastInfo(message: .linkMicDisableTExt)))
                return
            }
            if sender.isSelected {
                let requestId = store.selectCurrent(SeatSelectors.getMySeatApplicationId)
                store.dispatch(action: SeatActions.cancelApplication(payload: requestId))
            } else {
                if store.selectCurrent(UserSelectors.isOnSeat) {
                    store.dispatch(action: SeatActions.leaveSeat())
                } else {
                    routerStore.router(action: .present(.linkType))
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
                    let imageName = isOnSeat ? "live_linked_icon" : "live_link_icon"
                    button.setImage(.liveBundleImage(imageName), for: .normal)
                }
                .store(in: &cancellableSet)
            store
                .select(ConnectionSelectors.isConnecting)
                .receive(on: RunLoop.main)
                .sink { isConnecting in
                    let imageName = isConnecting ? "live_link_disable_icon" : "live_link_icon"
                    button.setImage(.liveBundleImage(imageName), for: .normal)
                }
                .store(in: &cancellableSet)

        }
        menus.append(linkMic)
        var like = ButtonMenuInfo(normalIcon: "live_like_icon")
        like.tapAction = { sender in
            store.dispatch(action: UserResponseActions.like())
        }
        menus.append(like)
        return menus
    }
}

extension LiveRoomRootMenuDataCreator {
    func applyToTakeSeat(store:LiveStore, needOpenCamera: Bool) {
        store.dispatch(action: ViewActions.updateAutoOpenCameraOnSeated(payload: needOpenCamera))
        store.dispatch(action: SeatActions.takeSeat(payload: nil))
    }
}

private extension String {
    static let videoLinkRequestText = localized("live.audience.linkType.videoLinkRequest")
    static var audioLinkRequestText =
    localized("live.audience.linkType.audioLinkRequest")
    static let beautyText = localized("live.anchor.setting.beauty")
    static let audioEffectsText = localized("live.anchor.setting.audio.effects")
    static let flipText = localized("live.anchor.setting.flip")
    static let mirrorText = localized("live.anchor.setting.mirror")
    static let videoParametersText = localized("live.anchor.setting.video.parameters")
    static let moreSettingText = localized("live.anchor.setting.more.setting")
    
    static let connectDisableText = localized("live.error.connectionDisable.linkMic")
    static let linkMicDisableTExt = localized("live.error.linkMicDisable.connecting")
}
