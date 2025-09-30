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
import AtomicXCore

class AnchorMenuDataCreator {
    
    private weak var coreView: LiveCoreView?
    private let manager: AnchorManager
    private let routerManager: AnchorRouterManager
    
    init(coreView: LiveCoreView, manager: AnchorManager, routerManager: AnchorRouterManager) {
        self.coreView = coreView
        self.manager = manager
        self.routerManager = routerManager
    }
    
    func generateBottomMenuData(features: [AnchorBottomMenuFeature]) -> [AnchorButtonMenuInfo] {
        ownerBottomMenu(features: features)
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
}

extension AnchorMenuDataCreator {
    private func ownerBottomMenu(features: [AnchorBottomMenuFeature]) -> [AnchorButtonMenuInfo] {
        var menus: [AnchorButtonMenuInfo] = []
        if features.contains(.coHost) {
            var connection = AnchorButtonMenuInfo(normalIcon: "live_connection_icon", normalTitle: .coHostText)
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
                let connectedUserListPublisher = manager.subscribeCoreViewState(StatePublisherSelector(keyPath: \CoGuestState.connectedUserList))
                let battleUsersPublisher = manager.subscribeState(StateSelector(keyPath: \AnchorBattleState.battleUsers))
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
            var battle = AnchorButtonMenuInfo(normalIcon: "live_battle_icon", normalTitle: .battleText)
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
                    config.duration = anchorBattleDuration
                    config.needResponse = true
                    config.extensionInfo = ""
                    let requestUserIds = manager.coHostState.connectedUsers
                        .filter { $0.userId != selfUserId }
                        .map { $0.userId }
                    coreView?.requestBattle(config: config, userIdList: requestUserIds, timeout: anchorBattleRequestTimeout) { [weak self] (battleId, battleUserList) in
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
                let battleUsersPublisher = manager.subscribeState(StateSelector(keyPath: \AnchorBattleState.battleUsers))
                let connectedUsersPublisher = manager.subscribeState(StateSelector(keyPath: \AnchorCoHostState.connectedUsers))
                let displayResultPublisher = manager.subscribeState(StateSelector(keyPath: \AnchorBattleState.isOnDisplayResult))
              
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
            var linkMic = AnchorButtonMenuInfo(normalIcon: "live_link_icon",animateIcon: ["live_link_animate1_icon", "live_link_animate2_icon", "live_link_animate3_icon"], normalTitle: .coGuestText)
            linkMic.tapAction = { [weak self] sender in
                guard let self = self else { return }
                if !manager.coHostState.connectedUsers.isEmpty {
                    return
                }
                routerManager.router(action: .present(.liveLinkControl))
            }
            linkMic.bindStateClosure = { [weak self] button, cancellableSet in
                guard let self = self else { return }
                manager.subscribeCoreViewState(StatePublisherSelector(keyPath: \CoHostState.connectedUserList))
                    .map { !$0.isEmpty }
                    .receive(on: RunLoop.main)
                    .removeDuplicates()
                    .sink { [weak button] isConnecting in
                        let imageName = isConnecting ? "live_link_disable_icon" : "live_link_icon"
                        button?.setImage(internalImage(imageName), for: .normal)
                    }
                    .store(in: &cancellableSet)
                
                manager.subscribeCoreViewState(StatePublisherSelector(keyPath: \CoGuestState.connectedUserList))
                    .map { $0.count > 1 }
                    .combineLatest(manager.subscribeCoreViewState(StatePublisherSelector(keyPath: \CoHostState.connectedUserList)).map { !$0.isEmpty })
                    .receive(on: RunLoop.main)
                    .sink { [weak button] isGuestLinking, isHostConnecting in
                        if isHostConnecting {
                            return
                        }
                        isGuestLinking ? button?.startAnimating() : button?.stopAnimating()
                    }
                    .store(in: &cancellableSet)
            }
            menus.append(linkMic)
        }
        
        var setting = AnchorButtonMenuInfo(normalIcon: "live_more_btn_icon", normalTitle: .MoreText)
        setting.tapAction = { [weak self] sender in
            guard let self = self else { return }
            let settingModel = generateSettingModel(features: features)
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
            let alertInfo = AnchorAlertInfo(description: String.endBattleAlertText, imagePath: nil,
                                            cancelButtonInfo: (String.cancelText, .g3),
                                            defaultButtonInfo: (String.confirmEndBattleText, .redColor)) { [weak self] _ in
                guard let self = self else { return }
                routerManager.router(action: .routeTo(.anchor))
            } defaultClosure: { [weak self] alertPanel in
                guard let self = self else { return }
                coreView?.terminateBattle(battleId: manager.battleState.battleId) {
                } onError: { _, _ in
                }
                routerManager.router(action: .routeTo(.anchor))
            }
            routerManager.router(action: .dismiss(.panel, completion: { [weak self] in
                guard let self = self else { return }
                routerManager.router(action: .present(.alert(info: alertInfo)))
            }))
        })
        items.append(endBattleItem)
        routerManager.router(action: .present(.listMenu(ActionPanelData(items: items, cancelText: .cancelText))))
    }
    
    private func generateSettingModel(features: [AnchorBottomMenuFeature]) -> AnchorFeatureClickPanelModel {
        let model = AnchorFeatureClickPanelModel()
        model.itemSize = CGSize(width: 56.scale375(), height: 80.scale375Height())
        model.itemDiff = 12.scale375()
        var designConfig = AnchorFeatureItemDesignConfig()
        designConfig.backgroundColor = .bgEntrycardColor
        designConfig.cornerRadius = 10
        designConfig.titleFont = .customFont(ofSize: 12)
        designConfig.titileColor = .textPrimaryColor
        designConfig.type = .imageAboveTitleBottom
        if features.contains(.beauty) {
            model.items.append(AnchorFeatureItem(normalTitle: .beautyText,
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
            model.items.append(AnchorFeatureItem(normalTitle: .audioEffectsText,
                                                 normalImage: internalImage("live_setting_audio_effects")?.withTintColor(.textPrimaryColor),
                                                 designConfig: designConfig,
                                                 actionClosure: { [weak self] _ in
                guard let self = self else { return }
                routerManager.router(action: .present(.audioEffect))
            }))
        }
        model.items.append(AnchorFeatureItem(normalTitle: .flipText,
                                             normalImage: internalImage("live_video_setting_flip")?.withTintColor(.textPrimaryColor),
                                             designConfig: designConfig,
                                             actionClosure: { [weak self] _ in
            guard let self = self else { return }
            coreView?.switchCamera(isFront: !manager.coreMediaState.isFrontCamera)
        }))
        model.items.append(AnchorFeatureItem(normalTitle: .mirrorText,
                                             normalImage: internalImage("live_video_setting_mirror")?.withTintColor(.textPrimaryColor),
                                             designConfig: designConfig,
                                             actionClosure: { [weak self] _ in
            guard let self = self else { return }
            coreView?.enableMirror(enable: !manager.coreMediaState.isMirrorEnabled)
        }))
        model.items.append(AnchorFeatureItem(normalTitle: .streamDashboardText,
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
    static let cancelText = internalLocalized("Cancel")
    
    static let streamDashboardText = internalLocalized("Dashboard")
    static let cancelLinkMicRequestText = internalLocalized("Cancel application for link mic")
    static let confirmTerminateCoGuestText = internalLocalized("End Link")
    static let coHostText = internalLocalized("Host")
    static let battleText = internalLocalized("Battle")
    static let coGuestText = internalLocalized("Guest")
    static let MoreText = internalLocalized("More")
}
