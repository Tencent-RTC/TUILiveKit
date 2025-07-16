//
//  AudienceMenuDataCreator.swift
//  TUILiveKit
//
//  Created by aby on 2024/5/31.
//

import Foundation
import RTCRoomEngine
import TUICore
import RTCCommon
import LiveStreamCore

class AudienceRootMenuDataCreator {
    
    private let coreView: LiveCoreView
    private let manager: AudienceManager
    private let routerManager: AudienceRouterManager
    
    init(coreView: LiveCoreView, manager: AudienceManager, routerManager: AudienceRouterManager) {
        self.coreView = coreView
        self.manager = manager
        self.routerManager = routerManager
    }
    
    func generateBottomMenuData(isDisableCoGuest: Bool = false) -> [AudienceButtonMenuInfo] {
        return memberBottomMenu(isDisableCoGuest: isDisableCoGuest)
    }
    
    func generateLinkTypeMenuData() -> [LinkMicTypeCellData] {
        var data = [LinkMicTypeCellData]()
        let timeOutValue = 60
        data.append(LinkMicTypeCellData(image: internalImage("live_link_video"),
                                        text: .videoLinkRequestText,
                                        action: { [weak self] in
            guard let self = self else { return }
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

extension AudienceRootMenuDataCreator {
    func memberBottomMenu(isDisableCoGuest: Bool = false) -> [AudienceButtonMenuInfo] {
        var menus: [AudienceButtonMenuInfo] = []
        var streamDashboard = AudienceButtonMenuInfo(normalIcon: "live_stream_dashboard_icon", normalTitle: "")
        streamDashboard.tapAction = { [weak self] sender in
            guard let self = self else { return }
            routerManager.router(action: .present(.streamDashboard))
        }
        menus.append(streamDashboard)
        var gift = AudienceButtonMenuInfo(normalIcon: "live_gift_icon", normalTitle: "")
        gift.tapAction = { [weak self] sender in
            guard let self = self else { return }
            routerManager.router(action: .present(.giftView))
        }
        menus.append(gift)
        if !isDisableCoGuest {
            var linkMic = AudienceButtonMenuInfo(normalIcon: "live_link_icon", selectIcon: "live_linking_icon")
            linkMic.tapAction = { [weak self] sender in
                guard let self = self else { return }
                if !manager.coreCoHostState.connectedUserList.isEmpty {
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
                    routerManager.router(action: .present(.listMenu(ActionPanelData(items: [item], cancelText: .cancelText))))
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
                manager.subscribeState(StateSelector(keyPath: \AudienceCoGuestState.coGuestStatus))
                    .removeDuplicates()
                    .receive(on: RunLoop.main)
                    .sink { [weak self] coGuestStatus in
                        guard let self = self else { return }
                        onCoGuestStatusChanged(button: button, enable: true, coGuestStatus: coGuestStatus)
                    }
                    .store(in: &cancellableSet)
                
                manager.subscribeCoreViewState(StateSelector(keyPath: \CoHostState.connectedUserList))
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
        }
        return menus
    }
    
    private func onCoGuestStatusChanged(button: UIButton, enable: Bool, coGuestStatus: AudienceCoGuestState.CoGuestStatus) {
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
        routerManager.router(action: .present(.listMenu(ActionPanelData(items: items, cancelText: .cancelText))))
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
    
    static let cancelLinkMicRequestText = internalLocalized("Cancel application for link mic")
    static let confirmTerminateCoGuestText = internalLocalized("End Link")
    static let coGuestText = internalLocalized("Guest")
    static let cancelText = internalLocalized("Cancel")
}
