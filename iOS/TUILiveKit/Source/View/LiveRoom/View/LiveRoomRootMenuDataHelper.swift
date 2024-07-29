//
//  LiveRoomRootMenuDataHelper.swift
//  Alamofire
//
//  Created by aby on 2024/5/31.
//

import Foundation

class LiveRoomRootMenuDataHelper {
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
            routerStore.router(action: .dismiss)
        }))
        
        data.append(LinkMicTypeCellData(image: .liveBundleImage("live_link_audio"),
                                        text: .audioLinkRequestText,
                                        action: {
            self.applyToTakeSeat(store: store, needOpenCamera: false)
            routerStore.router(action: .dismiss)
        }))
        return data
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
}

extension LiveRoomRootMenuDataHelper {
    func ownerBottomMenu(store: LiveStore, routerStore: RouterStore) -> [ButtonMenuInfo] {
        var menus: [ButtonMenuInfo] = []
        var linkMic = ButtonMenuInfo(normalIcon: "live_connection_icon")
        linkMic.tapAction = { sender in
            routerStore.router(action: .present(.liveLinkControl))
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
        let designConfig = FeatureItemDesignConfig()
        designConfig.backgroundColor = .g3
        designConfig.cornerRadius = 10
        designConfig.titleFont = .customFont(ofSize: 12)
        designConfig.type = .imageAboveTitleBottom
        model.items.append(FeatureItem(title: .beautyText,
                                       image: .liveBundleImage("live_video_setting_beauty"),
                                       designConfig: designConfig,
                                       actionClosure: {
            routerStore.router(action: .present(.beauty))
        }))
        model.items.append(FeatureItem(title: .audioEffectsText,
                                       image: .liveBundleImage("live_setting_audio_effects"),
                                       designConfig: designConfig,
                                       actionClosure: {
            routerStore.router(action: .present(.audioEffect))
        }))
        model.items.append(FeatureItem(title: .flipText,
                                       image: .liveBundleImage("live_video_setting_flip"),
                                       designConfig: designConfig,
                                       actionClosure: {
            let isFrontCamera = store.selectCurrent(MediaSelectors.getFrontCameraState)
            store.dispatch(action: MediaActions.switchCamera(payload: isFrontCamera == true ? .rear : .front))
        }))
        model.items.append(FeatureItem(title: .mirrorText,
                                       image: .liveBundleImage("live_video_setting_mirror"),
                                       designConfig: designConfig,
                                       actionClosure: {
            let isMirror = store.selectCurrent(MediaSelectors.getMirrorState)
            store.dispatch(action: MediaActions.switchMirror(payload: isMirror == true ? false : true))
        }))
        model.items.append(FeatureItem(title: .videoParametersText,
                                       image: .liveBundleImage("live_setting_video_parameters"),
                                       designConfig: designConfig,
                                       actionClosure: {
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
        var linkMic = ButtonMenuInfo(normalIcon: "live_connection_icon", selectIcon: "live_linking_icon")
        linkMic.tapAction = { sender in
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
                    let imageName = isOnSeat ? "live_linked_icon" : "live_connection_icon"
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

extension LiveRoomRootMenuDataHelper {
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
}
