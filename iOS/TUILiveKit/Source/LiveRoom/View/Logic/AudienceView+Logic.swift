//
//  AudienceView+Logic.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/12/29.
//

import TUICore

// MARK: initialize

extension AudienceView {
    func initFunction() {
        initData()
        registerObserver()
    }

    private func initData() {
        engineService.liveKitStore.selfInfo.userId = TUILogin.getUserID() ?? ""
        engineService.liveKitStore.selfInfo.avatarUrl.value = TUILogin.getFaceUrl() ?? ""
        engineService.liveKitStore.selfInfo.name.value = TUILogin.getNickName() ?? ""
        engineService.liveKitStore.selfInfo.role.value = .none
        engineService.liveKitStore.selfInfo.status.value = UserInteractionStatus.none
    }

    private func registerObserver() {
        liveRoomInfo.userLiveStatus.addObserver(self) { [weak self] _, _ in
            if self?.liveRoomInfo.userLiveStatus.value == UserLiveStatus.none {
                self?.isPresentEndView(isPresent: true)
            }
        }

        state.addObserver(self) { [weak self] type, _ in
            switch type {
            case .willDisplay:
                self?.startDisplay()
            case .didDisplay:
                self?.displayComplete()
            case .didEndDisplay:
                self?.endDisplay()
            default:
                break
            }
        }
    }

    private func initStreamInfo() {
        engineService.getUserList(onSuccess: nil, onError: nil)
        engineService.fetchRoomInfo(onSuccess: nil, onError: nil)
    }

    func deinitFunction() {
        if liveRoomInfo.userLiveStatus.value == .playing {
            engineService.exitRoom(onSuccess: nil, onError: nil)
        }
        
        EngineManager.removeRoomEngineService(roomId: roomId)
    }
}

// MARK: life circle

extension AudienceView {
    private func startDisplay() {
        guard liveRoomInfo.userLiveStatus.value != .playing, !isEnteringRoom else { return }
        engineService.muteAllRemoteAudio(isMute: true)
        isEnteringRoom = true
        engineService.enterRoom(roomId: roomId) { [weak self] in
            guard let self = self else { return }
            self.initStreamInfo()
            self.engineService.changeSelfRole(role: .audience)
            self.engineService.changeSelfStatus(status: .none)
            self.engineService.changeUserLiveStatus(userLiveStatus: .playing)

            self.isPresentEndView(isPresent: false)
            self.isEnteringRoom = false
        } onError: { [weak self] _, _ in
            guard let self = self else { return }
            self.isPresentEndView(isPresent: true)
            self.isEnteringRoom = false
        }
    }

    private func displayComplete() {
        engineService.muteAllRemoteAudio(isMute: false)
    }

    private func endDisplay() {
        engineService.muteAllRemoteAudio(isMute: true)
    }
}

// MARK: UI Management

extension AudienceView {
    private func isPresentEndView(isPresent: Bool) {
        audienceEndView.update(avatarUrl: liveRoomInfo.anchorInfo.value.avatarUrl.value, userName: liveRoomInfo.anchorInfo.value.name.value)
        audienceEndView.isHidden = !isPresent
        if isPresent {
            engineService.changeSelfRole(role: .audience)
            engineService.changeSelfStatus(status: .none)
            engineService.exitRoom(onSuccess: nil, onError: nil)
        }
    }
}
