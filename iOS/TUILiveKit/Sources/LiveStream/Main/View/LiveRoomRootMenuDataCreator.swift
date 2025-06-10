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
import TUILiveResources

class LiveRoomRootMenuDataCreator {
    
    private let coreView: LiveCoreView
    private let manager: LiveStreamManager
    private let routerManager: LSRouterManager
    
    init(coreView: LiveCoreView, manager: LiveStreamManager, routerManager: LSRouterManager) {
        self.coreView = coreView
        self.manager = manager
        self.routerManager = routerManager
    }
    
    func generateBottomMenuData(isOwner: Bool, features: [BottomMenuFeature]) -> [LSButtonMenuInfo] {
        if isOwner {
            return ownerBottomMenu(features: features)
        } else {
            return memberBottomMenu()
        }
    }
    
    func generateLinkTypeMenuData() -> [LinkMicTypeCellData] {
        var data = [LinkMicTypeCellData]()
        let timeOutValue = 60
        data.append(LinkMicTypeCellData(image: internalImage("live_link_video"),
                                        text: .videoLinkRequestText,
                                        action: { [weak self] in
            guard let self = self else { return }
            manager.mediaManager.changeVideoEncParams(encType: .small)
            manager.onStartRequestIntraRoomConnection()
            coreView.requestIntraRoomConnection(userId: "", timeOut: timeOutValue, openCamera: true) { [weak self] in
                guard let self = self else { return }
                manager.toastSubject.send(.waitToLinkText)
            } onError: { [weak self] code, message in
                guard let self = self else { return }
                let error = InternalError(code: code.rawValue, message: message)
                manager.toastSubject.send(error.localizedMessage)
                manager.onRequestIntraRoomConnectionFailed()
            }
            routerManager.router(action: .dismiss())
        }))
        
        data.append(LinkMicTypeCellData(image: internalImage("live_link_audio"),
                                        text: .audioLinkRequestText,
                                        action: { [weak self] in
            guard let self = self else { return }
            manager.onStartRequestIntraRoomConnection()
            coreView.requestIntraRoomConnection(userId: "", timeOut: timeOutValue, openCamera: false) { [weak self] in
                guard let self = self else { return }
                manager.toastSubject.send(.waitToLinkText)
            } onError: { [weak self] code, message in
                guard let self = self else { return }
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
    func ownerBottomMenu(features: [BottomMenuFeature]) -> [LSButtonMenuInfo] {
        var menus: [LSButtonMenuInfo] = []
        if features.contains(.coHost) {
            var connection = LSButtonMenuInfo(normalIcon: "live_connection_icon", normalTitle: .coHostText)
            let selfUserId = manager.coreUserState.selfInfo.userId
            connection.tapAction = { [weak self] sender in
                guard let self = self else { return }
                if manager.coreCoGuestState.connectedUserList.count > 1 || manager.battleState.battleUsers.contains(where: {$0.userId == selfUserId}) {
                    return
                } else {
                    routerManager.router(action: .present(.connectionControl))
                }
            }
            
            connection.bindStateClosure = { [weak self] button, cancellableSet in
                guard let self = self else { return }
                let connectedUserListPublisher = manager.subscribeCoreViewState(StateSelector(keyPath: \CoGuestState.connectedUserList))
                let battleUsersPublisher = manager.subscribeState(StateSelector(keyPath: \LSBattleState.battleUsers))
                connectedUserListPublisher
                    .removeDuplicates()
                    .combineLatest(battleUsersPublisher.removeDuplicates())
                    .receive(on: RunLoop.main)
                    .sink { [weak button] seatList, battleUsers in
                        let isBattle = battleUsers.contains(where: {$0.userId == selfUserId})
                        let isCoGuestConnected = seatList.count > 1
                        let imageName = isBattle || isCoGuestConnected ? "live_connection_disable_icon" : "live_connection_icon"
                        button?.setImage(internalImage(imageName), for: .normal)
                    }
                    .store(in: &cancellableSet)
            }
            menus.append(connection)
        }
        
        if features.contains(.battle) {
            var battle = LSButtonMenuInfo(normalIcon: "live_battle_icon", normalTitle: .battleText)
            battle.tapAction = { [weak self] sender in
                guard let self = self else { return }
                let selfUserId = manager.coreUserState.selfInfo.userId
                let isSelfInBattle = manager.battleState.battleUsers.contains(where: { $0.userId == selfUserId })
                if isSelfInBattle {
                    confirmToExitBattle()
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
                    coreView.requestBattle(config: config, userIdList: requestUserIds, timeout: battleRequestTimeout) { [weak self] (battleId, battleUserList) in
                        guard let self = self else { return }
                        manager.onRequestBattle(battleId: battleId, battleUserList: battleUserList)
                    } onError: { [weak self] code, message in
                        guard let self = self else { return }
                        let err = InternalError(code: code.rawValue, message: message)
                        manager.onError(err)
                    }
                }
            }
            battle.bindStateClosure = { [weak self] button, cancellableSet in
                guard let self = self else { return }
                let selfUserId = manager.coreUserState.selfInfo.userId
                let battleUsersPublisher = manager.subscribeState(StateSelector(keyPath: \LSBattleState.battleUsers))
                let connectedUsersPublisher = manager.subscribeState(StateSelector(keyPath: \LSCoHostState.connectedUsers))
                let displayResultPublisher = manager.subscribeState(StateSelector(keyPath: \LSBattleState.isOnDisplayResult))
              
                battleUsersPublisher
                    .removeDuplicates()
                    .receive(on: RunLoop.main)
                    .sink { [weak button] battleUsers in
                        let isOnBattle = battleUsers.contains(where: {$0.userId == selfUserId})
                        let imageName = isOnBattle ? "live_battle_exit_icon" : "live_battle_icon"
                        button?.setImage(internalImage(imageName), for: .normal)
                    }
                    .store(in: &cancellableSet)

                displayResultPublisher
                    .removeDuplicates()
                    .receive(on: RunLoop.main)
                    .sink { [weak button] display in
                        let imageName = display ?
                                            "live_battle_disable_icon" :
                                            "live_battle_icon"
                        button?.setImage(internalImage(imageName), for: .normal)
                    }
                    .store(in: &cancellableSet)
                
                connectedUsersPublisher
                    .removeDuplicates()
                    .combineLatest(battleUsersPublisher)
                    .receive(on: RunLoop.main)
                    .sink { [weak button] connectedUsers, battleUsers in
                        let isSelfInBattle = battleUsers.contains(where: { $0.userId == selfUserId })
                        guard !isSelfInBattle else { return }
                        let isSelfInConnection = connectedUsers.contains(where: { $0.userId == selfUserId })
                        
                        let imageName = isSelfInConnection ?
                                            "live_battle_icon" :
                                            "live_battle_disable_icon"
                        button?.setImage(internalImage(imageName), for: .normal)
                    }.store(in: &cancellableSet)
            }
            menus.append(battle)
        }
        
        if features.contains(.coGuest) {
            var linkMic = LSButtonMenuInfo(normalIcon: "live_link_icon", normalTitle: .coGuestText)
            linkMic.tapAction = { [weak self] sender in
                guard let self = self else { return }
                if !manager.coHostState.connectedUsers.isEmpty {
                    return
                }
                routerManager.router(action: .present(.liveLinkControl))
            }
            linkMic.bindStateClosure = { [weak self] button, cancellableSet in
                guard let self = self else { return }
                manager.subscribeState(StateSelector(keyPath: \LSCoHostState.connectedUsers))
                    .map { !$0.isEmpty }
                    .receive(on: RunLoop.main)
                    .sink { [weak button] isConnecting in
                        let imageName = isConnecting ? "live_link_disable_icon" : "live_link_icon"
                        button?.setImage(internalImage(imageName), for: .normal)
                    }
                    .store(in: &cancellableSet)
            }
            menus.append(linkMic)
        }
        
        var setting = LSButtonMenuInfo(normalIcon: "live_more_btn_icon", normalTitle: .MoreText)
        setting.tapAction = { [weak self] sender in
            guard let self = self else { return }
            let settingModel = generateAnchorSettingModel(features: features)
            routerManager.router(action: .present(.featureSetting(settingModel)))
        }
        menus.append(setting)
        return menus
    }
    
    private func confirmToExitBattle() {
        var items: [ActionItem] = []
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .redColor)
        designConfig.backgroundColor = .white
        designConfig.lineColor = .g8
        let endBattleItem = ActionItem(title: .confirmEndBattleText, designConfig: designConfig, actionClosure: {  [weak self] _ in
            guard let self = self else { return }
            let alertInfo = LSAlertInfo(description: String.endBattleAlertText, imagePath: nil,
                                      cancelButtonInfo: (String.alertCancelText, .g3),
                                      defaultButtonInfo: (String.confirmEndBattleText, .redColor)) { [weak self] _ in
                guard let self = self else { return }
                routerManager.router(action: .routeTo(.anchor))
            } defaultClosure: { [weak self] alertPanel in
                guard let self = self else { return }
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
    
    func generateAnchorSettingModel(features: [BottomMenuFeature]) -> LSFeatureClickPanelModel {
        let model = LSFeatureClickPanelModel()
        model.itemSize = CGSize(width: 56.scale375(), height: 80.scale375Height())
        model.itemDiff = 12.scale375()
        var designConfig = LSFeatureItemDesignConfig()
        designConfig.backgroundColor = .bgEntrycardColor
        designConfig.cornerRadius = 10
        designConfig.titleFont = .customFont(ofSize: 12)
        designConfig.titileColor = .textPrimaryColor
        designConfig.type = .imageAboveTitleBottom
        if features.contains(.beauty) {
            model.items.append(LSFeatureItem(normalTitle: .beautyText,
                                           normalImage: internalImage("live_video_setting_beauty")?.withTintColor(.textPrimaryColor),
                                           designConfig: designConfig,
                                           actionClosure: { [weak self] _ in
                guard let self = self else { return }
                routerManager.router(action: .dismiss(.panel, completion: { [weak self] in
                    guard let self = self else { return }
                    routerManager.router(action: .present(.beauty))
                }))
                let isEffectBeauty = (TUICore.getService(TUICore_TEBeautyService) != nil)
                DataReporter.reportEventData(eventKey: isEffectBeauty ? Constants.DataReport.kDataReportPanelShowLiveRoomBeautyEffect :
                                                Constants.DataReport.kDataReportPanelShowLiveRoomBeauty)
            }))
        }
        if features.contains(.soundEffect) {
            model.items.append(LSFeatureItem(normalTitle: .audioEffectsText,
                                           normalImage: internalImage("live_setting_audio_effects")?.withTintColor(.textPrimaryColor),
                                           designConfig: designConfig,
                                           actionClosure: { [weak self] _ in
                guard let self = self else { return }
                routerManager.router(action: .present(.audioEffect))
            }))
        }
        model.items.append(LSFeatureItem(normalTitle: .flipText,
                                       normalImage: internalImage("live_video_setting_flip")?.withTintColor(.textPrimaryColor),
                                       designConfig: designConfig,
                                       actionClosure: { [weak self] _ in
            guard let self = self else { return }
            coreView.switchCamera(isFront: !manager.coreMediaState.isFrontCamera)
        }))
        model.items.append(LSFeatureItem(normalTitle: .mirrorText,
                                         normalImage: internalImage("live_video_setting_mirror")?.withTintColor(.textPrimaryColor),
                                         designConfig: designConfig,
                                         actionClosure: { [weak self] _ in
            guard let self = self else { return }
            coreView.enableMirror(enable: !manager.coreMediaState.isMirrorEnabled)
        }))
        model.items.append(LSFeatureItem(normalTitle: .streamDashboardText,
                                       normalImage: internalImage("live_setting_stream_dashboard")?.withTintColor(.textPrimaryColor),
                                       designConfig: designConfig,
                                       actionClosure: { [weak self] _ in
            guard let self = self else { return }
            routerManager.router(action: .dismiss(.panel, completion: { [weak self] in
                guard let self = self else { return }
                routerManager.router(action: .present(.streamDashboard))
            }))
        }))
        return model
    }
    
    func memberBottomMenu() -> [LSButtonMenuInfo] {
        var menus: [LSButtonMenuInfo] = []
        var streamDashboard = LSButtonMenuInfo(normalIcon: "live_stream_dashboard_icon", normalTitle: "")
        streamDashboard.tapAction = { [weak self] sender in
            guard let self = self else { return }
            routerManager.router(action: .present(.streamDashboard))
        }
        menus.append(streamDashboard)
        var gift = LSButtonMenuInfo(normalIcon: "live_gift_icon", normalTitle: "")
        gift.tapAction = { [weak self] sender in
            guard let self = self else { return }
            routerManager.router(action: .present(.giftView))
        }
        menus.append(gift)
        var linkMic = LSButtonMenuInfo(normalIcon: "live_link_icon", selectIcon: "live_linking_icon")
        linkMic.tapAction = { [weak self] sender in
            guard let self = self else { return }
            if !manager.coHostState.connectedUsers.isEmpty {
                return
            }
            if sender.isSelected {
                let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .g2)
                designConfig.backgroundColor = .white
                designConfig.lineColor = .g8
                let item = ActionItem(title: .cancelLinkMicRequestText, designConfig: designConfig) { [weak self] _ in
                    guard let self = self else { return }
                    routerManager.router(action: .dismiss())
                    manager.onStartCancelIntraRoomConnection()
                    coreView.cancelIntraRoomConnection(userId: "") { [weak self] in
                        guard let self = self else { return }
                        manager.onCancelIntraRoomConnection()
                    } onError: { [weak self] code, message in
                        guard let self = self else { return }
                        let error = InternalError(code: code.rawValue, message: message)
                        manager.onCancelIntraRoomConnection()
                        manager.toastSubject.send(error.localizedMessage)
                    }
                }
                routerManager.router(action: .present(.listMenu(ActionPanelData(items: [item]))))
            } else {
                let selfUserId = manager.coreUserState.selfInfo.userId
                let isOnSeat = manager.coreCoGuestState.seatList.contains(where: { $0.userId == selfUserId })
                if isOnSeat {
                    confirmToTerminateCoGuest()
                } else {
                    let data = generateLinkTypeMenuData()
                    routerManager.router(action: .present(.linkType(data)))
                }
            }
        }
        linkMic.bindStateClosure = { [weak self] button, cancellableSet in
            guard let self = self else { return }
            manager.subscribeState(StateSelector(keyPath: \LSCoGuestState.coGuestStatus))
                .removeDuplicates()
                .receive(on: RunLoop.main)
                .sink { [weak self] coGuestStatus in
                    guard let self = self else { return }
                    onCoGuestStatusChanged(button: button, enable: true, coGuestStatus: coGuestStatus)
                }
                .store(in: &cancellableSet)
            
            manager.subscribeState(StateSelector(keyPath: \LSCoHostState.connectedUsers))
                .removeDuplicates()
                .receive(on: RunLoop.main)
                .sink { [weak self] users in
                    guard let self = self else { return }
                    let isConnecting = users.count > 0
                    onCoGuestStatusChanged(button: button, enable: !isConnecting, coGuestStatus: manager.coGuestState.coGuestStatus)
                }
                .store(in: &cancellableSet)
            
        }
        menus.append(linkMic)
        var like = LSButtonMenuInfo(normalIcon: "live_like_icon")
        like.tapAction = { [weak self] sender in
            guard let self = self else { return }
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
        button.setImage(internalImage(imageName), for: .normal)
    }
    
    private func confirmToTerminateCoGuest() {
        var items: [ActionItem] = []
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .redColor)
        designConfig.backgroundColor = .white
        designConfig.lineColor = .g8
        
        let terminteGoGuestItem = ActionItem(title: .confirmTerminateCoGuestText, designConfig: designConfig, actionClosure: { [weak self] _ in
            guard let self = self else { return }
            coreView.terminateIntraRoomConnection()
            routerManager.router(action: .routeTo(.audience))
        })
        items.append(terminteGoGuestItem)
        routerManager.router(action: .present(.listMenu(ActionPanelData(items: items))))
    }
}

private extension String {
    static let videoLinkRequestText = internalLocalized("Apply for video link")
    static var audioLinkRequestText = internalLocalized("Apply for audio link")
    static let waitToLinkText = internalLocalized("You have submitted a link mic request, please wait for the author approval")
    static let beautyText = internalLocalized("Beauty")
    static let audioEffectsText = internalLocalized("Audio")
    static let flipText = internalLocalized("Flip")
    static let mirrorText = internalLocalized("Mirror")
    static let confirmEndBattleText = internalLocalized("End PK")
    static let endBattleAlertText = internalLocalized("Are you sure you want to end the battle? The current result will be the final result after the end")
    static let alertCancelText = internalLocalized("Cancel")
    
    static let streamDashboardText = internalLocalized("Dashboard")
    static let cancelLinkMicRequestText = internalLocalized("Cancel application for link mic")
    static let confirmTerminateCoGuestText = internalLocalized("End Link")
    static let coHostText = internalLocalized("Host")
    static let battleText = internalLocalized("Battle")
    static let coGuestText = internalLocalized("Guest")
    static let MoreText = internalLocalized("More")
}
