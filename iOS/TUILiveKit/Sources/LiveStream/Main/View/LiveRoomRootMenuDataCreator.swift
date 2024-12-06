//
//  LiveRoomRootMenuDataCreator.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/31.
//

import Foundation
import RTCRoomEngine
import TUICore
import RTCCommon
import LiveStreamCore

class LiveRoomRootMenuDataCreator {
    func generateBottomMenuData(manager: LiveStreamManager, routerManager: LSRouterManager, coreView: LiveCoreView, isOwner: Bool) -> [LSButtonMenuInfo] {
        if isOwner {
            return ownerBottomMenu(manager: manager, routerManager: routerManager, coreView: coreView)
        } else {
            return memberBottomMenu(manager: manager, routerManager: routerManager, coreView: coreView)
        }
    }
    
    func generateLinkTypeMenuData(manager: LiveStreamManager, routerManager: LSRouterManager, coreView: LiveCoreView) -> [LinkMicTypeCellData] {
        var data = [LinkMicTypeCellData]()
        let timeOutValue = 60
        data.append(LinkMicTypeCellData(image: .liveBundleImage("live_link_video"),
                                        text: .videoLinkRequestText,
                                        action: {
            manager.update(coGuestStatus: .applying)
            coreView.requestIntraRoomConnection(userId: "", timeOut: timeOutValue, openCamera: true) {
            } onError: { code, message in
                manager.update(coGuestStatus: .none)
                let error = InternalError(error: code, message: message)
                manager.toastSubject.send(error.localizedMessage)
            }
            routerManager.router(action: .dismiss())
        }))
        
        data.append(LinkMicTypeCellData(image: .liveBundleImage("live_link_audio"),
                                        text: .audioLinkRequestText,
                                        action: {
            manager.update(coGuestStatus: .applying)
            coreView.requestIntraRoomConnection(userId: "", timeOut: timeOutValue, openCamera: false) {
            } onError: { code, message in
                manager.update(coGuestStatus: .none)
                let error = InternalError(error: code, message: message)
                manager.toastSubject.send(error.localizedMessage)
            }
            routerManager.router(action: .dismiss())
        }))
        return data
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
}

extension LiveRoomRootMenuDataCreator {
    func ownerBottomMenu(manager: LiveStreamManager, routerManager: LSRouterManager, coreView: LiveCoreView) -> [LSButtonMenuInfo] {
        var menus: [LSButtonMenuInfo] = []
        var connection = LSButtonMenuInfo(normalIcon: "live_connection_icon")
        let selfUserId = manager.userState.selfInfo.userId
        connection.tapAction = { sender in
            if manager.coGuestState.connectedUserList.count > 1 {
                manager.toastSubject.send(.connectDisableText)
            } else if manager.battleManager.state.battleUsers.contains(where: {$0.userId == selfUserId}){
                manager.toastSubject.send(.connectDisableForBattleText)
            } else {
                routerManager.router(action: .present(.connectionControl))
            }
        }
        
        connection.bindStateClosure = { button, cancellableSet in
            func updateButton(_ button: LSMenuButton, battleUsers: [BattleUser], connectedUserList: [LSSeatInfo]) {
                let isBattle = battleUsers.contains(where: {$0.userId == selfUserId})
                let isCoGuestConnected = connectedUserList.count > 1
                let imageName = isBattle || isCoGuestConnected ? "live_connection_disable_icon" : "live_connection_icon"
                button.setImage(.liveBundleImage(imageName), for: .normal)
            }
            
            let connectedUserListPublisher = manager.subscribeCoGuestState(StateSelector(keyPath: \LSCoGuestState.connectedUserList))
            let battleUsersPublisher = manager.battleManager.subscribeState(StateSelector(keyPath: \LSBattleState.battleUsers))
            connectedUserListPublisher
                .removeDuplicates()
                .combineLatest(battleUsersPublisher.removeDuplicates())
                .receive(on: RunLoop.main)
                .sink { seatList, battleUsers in
                    updateButton(button, battleUsers: battleUsers, connectedUserList: seatList)
                }
                .store(in: &cancellableSet)
        }
        menus.append(connection)
        
        var battle = LSButtonMenuInfo(normalIcon: "live_battle_icon")
        battle.tapAction = { sender in
            let selfUserId = manager.userState.selfInfo.userId
            let isSelfInBattle = manager.battleState.battleUsers.contains(where: { $0.userId == selfUserId })
            if isSelfInBattle {
                self.confirmToExitBattle(manager: manager, routerManager: routerManager)
            } else {
                let requestUserIds = manager.coHostState.connectedUsers.map { $0.userId }
                manager.battleManager.requestBattle(userIds: requestUserIds, timeout: battleRequestTimeout)
            }
        }
        battle.bindStateClosure = { button, cancellableSet in
            let selfUserId = manager.userState.selfInfo.userId
            let battleUsersPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.battleUsers))
            let connectedUsersPublisher = manager.subscribeCoHostState(StateSelector(keyPath: \LSCoHostState.connectedUsers))
            let displayResultPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.isOnDisplayResult))
          
            battleUsersPublisher
                .removeDuplicates()
                .receive(on: RunLoop.main)
                .sink { battleUsers in
                    let isOnBattle = battleUsers.contains(where: {$0.userId == selfUserId})
                    let imageName = isOnBattle ? "live_battle_exit_icon" : "live_battle_icon"
                    button.setImage(.liveBundleImage(imageName), for: .normal)
                }
                .store(in: &cancellableSet)

            displayResultPublisher
                .removeDuplicates()
                .receive(on: RunLoop.main)
                .sink { display in
                    let imageName = display ?
                                        "live_battle_disable_icon" :
                                        "live_battle_icon"
                    button.setImage(.liveBundleImage(imageName), for: .normal)
                }
                .store(in: &cancellableSet)
            
            connectedUsersPublisher
                .removeDuplicates()
                .combineLatest(battleUsersPublisher)
                .receive(on: RunLoop.main)
                .sink { connectedUsers, battleUsers in
                    let isSelfInBattle = battleUsers.contains(where: { $0.userId == selfUserId })
                    guard !isSelfInBattle else { return }
                    let isSelfInConnection = connectedUsers.contains(where: { $0.userId == selfUserId })
                    
                    let imageName = isSelfInConnection ?
                                        "live_battle_icon" :
                                        "live_battle_disable_icon"
                    button.setImage(.liveBundleImage(imageName), for: .normal)
                }.store(in: &cancellableSet)
        }
        menus.append(battle)
        
        var linkMic = LSButtonMenuInfo(normalIcon: "live_link_icon")
        linkMic.tapAction = { sender in
            if manager.coHostManager.isCoHostConnecting() {
                manager.toastSubject.send(.linkMicDisableTExt)
                return
            }
            routerManager.router(action: .present(.liveLinkControl))
        }
        linkMic.bindStateClosure = { button, cancellableSet in
            let selector = StateSelector(keyPath: \LSCoHostState.connectedUsers)
            manager.coHostManager.subscribeCoHostState(selector)
                .map { !$0.isEmpty }
                .receive(on: RunLoop.main)
                .sink { isConnecting in
                    let imageName = isConnecting ? "live_link_disable_icon" : "live_link_icon"
                    button.setImage(.liveBundleImage(imageName), for: .normal)
                }
                .store(in: &cancellableSet)
        }
        menus.append(linkMic)
        
        var setting = LSButtonMenuInfo(normalIcon: "live_anchor_setting_icon")
        setting.tapAction = { sender in
            let settingModel = self.generateAnchorSettingModel(manager: manager, routerManager: routerManager)
            routerManager.router(action: .present(.featureSetting(settingModel)))
        }
        menus.append(setting)
        
        var music = LSButtonMenuInfo(normalIcon: "live_anchor_music_icon")
        music.tapAction = { sender in
            routerManager.router(action: .present(.musicList))
        }
        menus.append(music)
        return menus
    }
    
    private func confirmToExitBattle(manager: LiveStreamManager, routerManager: LSRouterManager) {
        var items: [ActionItem] = []
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .redColor)
        designConfig.backgroundColor = .white
        designConfig.lineColor = .g8
        let endBattleItem = ActionItem(title: .confirmEndBattleText, designConfig: designConfig, actionClosure: { _ in
            let alertInfo = LSAlertInfo(description: String.endBattleAlertText, imagePath: nil,
                                      cancelButtonInfo: (String.alertCancelText, .g3),
                                      defaultButtonInfo: (String.confirmEndBattleText, .redColor)) { _ in
                routerManager.router(action: .routeTo(.anchor))
            } defaultClosure: { alertPanel in
                manager.battleManager.exitBattle()
                routerManager.router(action: .routeTo(.anchor))
            }
            routerManager.router(action: .present(.alert(info: alertInfo)))
        })
        items.append(endBattleItem)
        routerManager.router(action: .present(.listMenu(ActionPanelData(items: items))))
    }
    
    func generateAnchorSettingModel(manager: LiveStreamManager,
                                    routerManager: LSRouterManager) -> LSFeatureClickPanelModel {
        let model = LSFeatureClickPanelModel()
        model.itemSize = CGSize(width: 56.scale375(), height: 76.scale375())
        model.itemDiff = 12.scale375()
        var designConfig = LSFeatureItemDesignConfig()
        designConfig.backgroundColor = .g3
        designConfig.cornerRadius = 10
        designConfig.titleFont = .customFont(ofSize: 12)
        designConfig.type = .imageAboveTitleBottom
        model.items.append(LSFeatureItem(normalTitle: .beautyText,
                                       normalImage: .liveBundleImage("live_video_setting_beauty"),
                                       designConfig: designConfig,
                                       actionClosure: { [weak routerManager] _ in
            guard let routerManager = routerManager else { return }
            routerManager.router(action: .dismiss(.panel, completion: { [weak routerManager] in
                guard let routerManager = routerManager else { return }
                routerManager.router(action: .present(.beauty))
            }))
            let isEffectBeauty = (TUICore.getService(TUICore_TEBeautyService) != nil)
            DataReporter.reportEventData(eventKey: isEffectBeauty ? Constants.DataReport.kDataReportPanelShowLiveRoomBeautyEffect :
                                            Constants.DataReport.kDataReportPanelShowLiveRoomBeauty)
        }))
        model.items.append(LSFeatureItem(normalTitle: .audioEffectsText,
                                       normalImage: .liveBundleImage("live_setting_audio_effects"),
                                       designConfig: designConfig,
                                       actionClosure: { _ in
            routerManager.router(action: .present(.audioEffect))
        }))
        model.items.append(LSFeatureItem(normalTitle: .flipText,
                                       normalImage: .liveBundleImage("live_video_setting_flip"),
                                       designConfig: designConfig,
                                       actionClosure: { _ in
            manager.switchCamera()
        }))
        model.items.append(LSFeatureItem(normalTitle: .mirrorText,
                                       normalImage: .liveBundleImage("live_video_setting_mirror"),
                                       designConfig: designConfig,
                                       actionClosure: { _ in
            manager.setCameraMirror()
        }))
        model.items.append(LSFeatureItem(normalTitle: .videoParametersText,
                                       normalImage: .liveBundleImage("live_setting_video_parameters"),
                                       designConfig: designConfig,
                                       actionClosure: { _ in
            routerManager.router(action: .present(.videoSetting))
        }))
        model.items.append(LSFeatureItem(normalTitle: .streamDashboardText,
                                       normalImage: .liveBundleImage("live_setting_stream_dashboard"),
                                       designConfig: designConfig,
                                       actionClosure: { _ in
            routerManager.router(action: .dismiss(.panel, completion: { [weak self] in
                guard let _ = self else { return }
                routerManager.router(action: .present(.streamDashboard))
            }))
        }))
        return model
    }
    
    func memberBottomMenu(manager: LiveStreamManager, routerManager: LSRouterManager, coreView: LiveCoreView) -> [LSButtonMenuInfo] {
        var menus: [LSButtonMenuInfo] = []
        var streamDashboard = LSButtonMenuInfo(normalIcon: "live_stream_dashboard_icon", normalTitle: "")
        streamDashboard.tapAction = { sender in
            routerManager.router(action: .present(.streamDashboard))
        }
        menus.append(streamDashboard)
        var gift = LSButtonMenuInfo(normalIcon: "live_gift_icon", normalTitle: "")
        gift.tapAction = { sender in
            routerManager.router(action: .present(.giftView))
        }
        menus.append(gift)
        var linkMic = LSButtonMenuInfo(normalIcon: "live_link_icon", selectIcon: "live_linking_icon")
        linkMic.tapAction = { sender in
            if manager.coHostManager.isCoHostConnecting() {
                manager.toastSubject.send(.linkMicDisableTExt)
                return
            }
            if sender.isSelected {
                coreView.cancelIntraRoomConnection(userId: "") {
                    manager.update(coGuestStatus: .none)
                } onError: { code, message in
                    let error = InternalError(error: code, message: message)
                    manager.toastSubject.send(error.localizedMessage)
                }
            } else {
                let isOnSeat = manager.coGuestState.connectedUserList.contains(where: { $0.userId == manager.userState.selfInfo.userId })
                if isOnSeat {
                    coreView.terminateIntraRoomConnection()
                } else {
                    let data = LiveRoomRootMenuDataCreator().generateLinkTypeMenuData(manager: manager, routerManager: routerManager, coreView: coreView)
                    routerManager.router(action: .present(.linkType(data)))
                }
            }
        }
        linkMic.bindStateClosure = { button, cancellableSet in
            manager.subscribeCoGuestState(StateSelector(keyPath: \LSCoGuestState.coGuestStatus))
                .removeDuplicates()
                .receive(on: RunLoop.main)
                .sink { coGuestStatus in
                    let imageName: String
                    switch coGuestStatus {
                    case .applying:
                        button.isSelected = true
                        imageName = "live_link_icon"
                    case .linking:
                        button.isSelected = false
                        imageName = "live_linked_icon"
                    case .none:
                        button.isSelected = false
                        imageName = "live_link_icon"
                    }
                    button.setImage(.liveBundleImage(imageName), for: .normal)
                }
                .store(in: &cancellableSet)
            
            manager.subscribeCoHostState(StateSelector(keyPath: \LSCoHostState.connectedUsers))
                .removeDuplicates()
                .receive(on: RunLoop.main)
                .sink { users in
                    let isConnecting = users.count > 0
                    let imageName = isConnecting ? "live_link_disable_icon" : "live_link_icon"
                    button.setImage(.liveBundleImage(imageName), for: .normal)
                }
                .store(in: &cancellableSet)
            
        }
        menus.append(linkMic)
        var like = LSButtonMenuInfo(normalIcon: "live_like_icon")
        like.tapAction = { sender in
            manager.likeSubject.send()
        }
        menus.append(like)
        return menus
    }
}

private extension String {
    static let videoLinkRequestText = localized("live.audience.linkType.videoLinkRequest")
    static var audioLinkRequestText = localized("live.audience.linkType.audioLinkRequest")
    static let beautyText = localized("live.anchor.setting.beauty")
    static let audioEffectsText = localized("live.anchor.setting.audio.effects")
    static let flipText = localized("live.anchor.setting.flip")
    static let mirrorText = localized("live.anchor.setting.mirror")
    static let videoParametersText = localized("live.anchor.setting.video.parameters")
    static let moreSettingText = localized("live.anchor.setting.more.setting")
    
    static let connectDisableText = localized("live.error.connectionDisable.linkMic")
    static let connectDisableForBattleText = localized("live.error.connectionDisable.battle")
    static let linkMicDisableTExt = localized("live.error.linkMicDisable.connecting")
    static let battleDisableText = localized("live.error.battleDisable.unconnected")
    static let confirmEndBattleText = localized("live.battle.confirm.end")
    static let endBattleAlertText = localized("live.battle.end.alert")
    static let alertCancelText = localized("live.alert.cancel")
    
    static let streamDashboardText = localized("live.streamDashboard.title")
}
