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
            manager.mediaManager.changeVideoEncParams(encType: .small)
            manager.onStartRequestIntraRoomConnection()
            coreView.requestIntraRoomConnection(userId: "", timeOut: timeOutValue, openCamera: true) {
                manager.toastSubject.send(.waitToLinkText)
            } onError: { code, message in
                let error = InternalError(code: code.rawValue, message: message)
                manager.toastSubject.send(error.localizedMessage)
                manager.onRequestIntraRoomConnectionFailed()
            }
            routerManager.router(action: .dismiss())
        }))
        
        data.append(LinkMicTypeCellData(image: .liveBundleImage("live_link_audio"),
                                        text: .audioLinkRequestText,
                                        action: {
            manager.onStartRequestIntraRoomConnection()
            coreView.requestIntraRoomConnection(userId: "", timeOut: timeOutValue, openCamera: false) {
                manager.toastSubject.send(.waitToLinkText)
            } onError: { code, message in
                let error = InternalError(code: code.rawValue, message: message)
                manager.toastSubject.send(error.localizedMessage)
                manager.onRequestIntraRoomConnectionFailed()
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
        let selfUserId = manager.coreUserState.selfInfo.userId
        connection.tapAction = { sender in
            if manager.coreCoGuestState.connectedUserList.count > 1 || manager.battleState.battleUsers.contains(where: {$0.userId == selfUserId}) {
                return
            } else {
                routerManager.router(action: .present(.connectionControl))
            }
        }
        
        connection.bindStateClosure = { button, cancellableSet in
            func updateButton(_ button: LSMenuButton, battleUsers: [BattleUser], connectedUserList: [TUIUserInfo]) {
                let isBattle = battleUsers.contains(where: {$0.userId == selfUserId})
                let isCoGuestConnected = connectedUserList.count > 1
                let imageName = isBattle || isCoGuestConnected ? "live_connection_disable_icon" : "live_connection_icon"
                button.setImage(.liveBundleImage(imageName), for: .normal)
            }
            
            let connectedUserListPublisher = manager.subscribeCoreViewState(StateSelector(keyPath: \CoGuestState.connectedUserList))
            let battleUsersPublisher = manager.subscribeState(StateSelector(keyPath: \LSBattleState.battleUsers))
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
            let selfUserId = manager.coreUserState.selfInfo.userId
            let isSelfInBattle = manager.battleState.battleUsers.contains(where: { $0.userId == selfUserId })
            if isSelfInBattle {
                self.confirmToExitBattle(manager: manager, routerManager: routerManager, coreView: coreView)
            } else {
                let isOnDisplayResult = manager.battleState.isOnDisplayResult
                let isSelfInConnection = manager.coHostState.connectedUsers.contains(where: { $0.userId == selfUserId })
                guard !isOnDisplayResult && isSelfInConnection else {
                    return
                }
                let config = TUIBattleConfig()
                config.duration = battleDuration
                config.needResponse = true
                config.extensionInfo = ""
                let requestUserIds = manager.coHostState.connectedUsers
                    .filter { $0.userId != selfUserId }
                    .map { $0.userId }
                coreView.requestBattle(config: config, userIdList: requestUserIds, timeout: battleRequestTimeout) { (battleId, battleUserList) in
                    manager.onRequestBattle(battleId: battleId, battleUserList: battleUserList)
                } onError: { _, _ in
                    
                }
            }
        }
        battle.bindStateClosure = { button, cancellableSet in
            let selfUserId = manager.coreUserState.selfInfo.userId
            let battleUsersPublisher = manager.subscribeState(StateSelector(keyPath: \LSBattleState.battleUsers))
            let connectedUsersPublisher = manager.subscribeState(StateSelector(keyPath: \LSCoHostState.connectedUsers))
            let displayResultPublisher = manager.subscribeState(StateSelector(keyPath: \LSBattleState.isOnDisplayResult))
          
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
            if !manager.coHostState.connectedUsers.isEmpty {
                return
            }
            routerManager.router(action: .present(.liveLinkControl))
        }
        linkMic.bindStateClosure = { button, cancellableSet in
            manager.subscribeState(StateSelector(keyPath: \LSCoHostState.connectedUsers))
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
            let settingModel = self.generateAnchorSettingModel(manager: manager, routerManager: routerManager, coreView: coreView)
            routerManager.router(action: .present(.featureSetting(settingModel)))
        }
        menus.append(setting)
        return menus
    }
    
    private func confirmToExitBattle(manager: LiveStreamManager, routerManager: LSRouterManager, coreView: LiveCoreView) {
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
                coreView.terminateBattle(battleId: manager.battleState.battleId) {
                } onError: { _, _ in
                }
                routerManager.router(action: .routeTo(.anchor))
            }
            routerManager.router(action: .present(.alert(info: alertInfo)))
        })
        items.append(endBattleItem)
        routerManager.router(action: .present(.listMenu(ActionPanelData(items: items))))
    }
    
    func generateAnchorSettingModel(manager: LiveStreamManager,
                                    routerManager: LSRouterManager,
                                    coreView: LiveCoreView) -> LSFeatureClickPanelModel {
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
                                       actionClosure: { [weak coreView] _ in
            guard let coreView = coreView else { return }
            coreView.switchCamera(isFront: !manager.coreMediaState.isFrontCamera)
        }))
        model.items.append(LSFeatureItem(normalTitle: .videoParametersText,
                                       normalImage: .liveBundleImage("live_setting_video_parameters"),
                                       designConfig: designConfig,
                                       actionClosure: { _ in
            routerManager.router(action: .dismiss(.panel, completion: { [weak self] in
                guard let _ = self else { return }
                routerManager.router(action: .present(.videoSetting))
            }))
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
            if !manager.coHostState.connectedUsers.isEmpty {
                return
            }
            if sender.isSelected {
                let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .g2)
                designConfig.backgroundColor = .white
                designConfig.lineColor = .g8
                let item = ActionItem(title: .cancelLinkMicRequestText, designConfig: designConfig) { _ in
                    routerManager.router(action: .dismiss())
                    manager.onStartCancelIntraRoomConnection()
                    coreView.cancelIntraRoomConnection(userId: "") {
                        manager.onCancelIntraRoomConnection()
                    } onError: { code, message in
                        let error = InternalError(code: code.rawValue, message: message)
                        manager.onCancelIntraRoomConnection()
                        manager.toastSubject.send(error.localizedMessage)
                    }
                }
                routerManager.router(action: .present(.listMenu(ActionPanelData(items: [item]))))
            } else {
                let isOnSeat = manager.coreCoGuestState.seatList.contains(where: { $0.userId == manager.coreUserState.selfInfo.userId })
                if isOnSeat {
                    self.confirmToTerminateCoGuest(routerManager: routerManager, coreView: coreView)
                } else {
                    let data = LiveRoomRootMenuDataCreator().generateLinkTypeMenuData(manager: manager, routerManager: routerManager, coreView: coreView)
                    routerManager.router(action: .present(.linkType(data)))
                }
            }
        }
        linkMic.bindStateClosure = { button, cancellableSet in
            manager.subscribeState(StateSelector(keyPath: \LSCoGuestState.coGuestStatus))
                .removeDuplicates()
                .receive(on: RunLoop.main)
                .sink { coGuestStatus in
                    self.onCoGuestStatusChanged(button: button, enable: true, coGuestStatus: manager.coGuestState.coGuestStatus)
                }
                .store(in: &cancellableSet)
            
            manager.subscribeState(StateSelector(keyPath: \LSCoHostState.connectedUsers))
                .removeDuplicates()
                .receive(on: RunLoop.main)
                .sink { users in
                    let isConnecting = users.count > 0
                    self.onCoGuestStatusChanged(button: button, enable: !isConnecting, coGuestStatus: manager.coGuestState.coGuestStatus)
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
    
    private func onCoGuestStatusChanged(button: UIButton, enable: Bool, coGuestStatus: LSCoGuestState.CoGuestStatus) {
        let imageName: String
        var isSelected = false
        
        if enable {
            isSelected = (coGuestStatus == .applying)
            imageName = (coGuestStatus == .linking)
                        ? "live_linked_icon"
                        : "live_link_icon"
        } else {
            isSelected = false
            imageName = "live_link_disable_icon"
        }
        
        button.isSelected = isSelected
        button.setImage(.liveBundleImage(imageName), for: .normal)
    }
    
    private func confirmToTerminateCoGuest(routerManager: LSRouterManager, coreView: LiveCoreView) {
        var items: [ActionItem] = []
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .redColor)
        designConfig.backgroundColor = .white
        designConfig.lineColor = .g8
        
        let terminteGoGuestItem = ActionItem(title: .confirmTerminateCoGuestText, designConfig: designConfig, actionClosure: { _ in
            coreView.terminateIntraRoomConnection()
            routerManager.router(action: .routeTo(.audience))
        })
        items.append(terminteGoGuestItem)
        routerManager.router(action: .present(.listMenu(ActionPanelData(items: items))))
    }
}

private extension String {
    static let videoLinkRequestText = localized("Apply for video link")
    static var audioLinkRequestText = localized("Apply for audio link")
    static let waitToLinkText = localized("You have submitted a link mic request, please wait for the author approval")
    static let beautyText = localized("Beauty")
    static let audioEffectsText = localized("Audio")
    static let flipText = localized("Flip")
    static let videoParametersText = localized("Video Config")
    static let confirmEndBattleText = localized("End PK")
    static let endBattleAlertText = localized("Are you sure you want to end the battle? The current result will be the final result after the end")
    static let alertCancelText = localized("Cancel")
    
    static let streamDashboardText = localized("Dashboard")
    static let cancelLinkMicRequestText = localized("Cancel application for link mic")
    static let confirmTerminateCoGuestText = localized("End Link")
}
