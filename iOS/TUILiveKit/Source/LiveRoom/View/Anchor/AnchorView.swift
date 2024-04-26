//
//  File.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/12/14.
//

import Foundation
import TUICore
import RTCRoomEngine
import Combine

class AnchorView: UIView {
    private var cancellable = Set<AnyCancellable>()
    var startLiveBlock:(()->Void)?
    private var liveRoomInfo:LiveRoomInfo {
          engineService.liveRoomInfo
    }
    
    private var engineService: RoomEngineService {
        EngineManager.getRoomEngineService(roomId: self.roomId)
    }
    private let roomId : String
    
    private lazy var videoView: AnchorVideoView = {
        AnchorVideoView(engineService: self.engineService)
    }()
    
    private var prepareView: AnchorPrepareView?
    private var livingView: AnchorLivingView?
    
    init(roomId : String) {
        self.roomId = roomId
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        initData()
        initView()
        registerObserver()
    }
    
    func updateRootViewOrientation(isPortrait: Bool) {
        engineService.liveKitStore.isPortrait.value = isPortrait
        prepareView?.updateRootViewOrientation(isPortrait: isPortrait)
        livingView?.updateRootViewOrientation(isPortrait: isPortrait)
    }
   
    deinit {
        engineService.stopLocalPreview()
        engineService.closeLocalMicrophone()
        EngineManager.removeRoomEngineService(roomId: roomId)
    }
}



extension AnchorView {
    
    private func initData() {
        engineService.liveKitStore.selfInfo.userId = TUILogin.getUserID() ?? ""
        engineService.liveKitStore.selfInfo.avatarUrl.value = TUILogin.getFaceUrl() ?? ""
        engineService.liveKitStore.selfInfo.name.value = TUILogin.getNickName() ?? ""
        engineService.liveKitStore.selfInfo.role.value = .none
        engineService.liveKitStore.selfInfo.status.value = UserInteractionStatus.none
        engineService.liveKitStore.isPortrait.value = WindowUtils.isPortrait
        engineService.liveKitStore.selfInfo.audioInfo.enableVoiceEarMonitor.value = false
        liveRoomInfo.interactionType.value = .broadcast
        liveRoomInfo.userLiveStatus.value = .previewing
        liveRoomInfo.name.value = TUILogin.getNickName() ?? ""
        engineService.initLivingConfig()
    }
    
    private func initView() {
        addSubview(videoView)
        videoView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        backgroundColor = .black
        let prepareView = AnchorPrepareView(engineService: engineService)
        addSubview(prepareView)
        prepareView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        self.prepareView = prepareView
        engineService.changeUserLiveStatus(userLiveStatus: .previewing)
        engineService.openLocalCamera() {
        } onError: { [weak self] code, message in
            let statusString = String(code.rawValue) + "," + message
            self?.makeToast(.localizedReplace(.operateFailedText, replace: statusString))
        }
    }

    private func registerObserver() {
        engineService.liveKitStore.$applyLinkAudienceList
            .receive(on: RunLoop.main)
            .sink { [weak self] _ in
                guard let self = self else{ return}
                let applyLinkCount = self.engineService.liveKitStore.applyLinkAudienceList.count
                self.livingView?.showLinkMicFloatView(isPresent: applyLinkCount > 0)
            }.store(in: &cancellable)
        
        liveRoomInfo.userLiveStatus.addObserver(self) { [weak self] userLiveStatus, _ in
            if userLiveStatus == .pushing {
                self?.createRoom()
            } else if userLiveStatus == .none {
                self?.anchorExit()
            }
        }
    }

}

// MARK: Action

extension AnchorView {
    private func anchorExit() {
        guard let livingView = livingView else { return }
        engineService.destroyRoom(onSuccess: nil, onError: nil)
        let liveDataModel = LiveDataModel(liveDuration: abs(Int(Date().timeIntervalSince1970 - liveRoomInfo.createTime)),
                                          audienceCount: liveRoomInfo.audienceCount.value,
                                          messageCount: livingView.getBarrageCount(),
                                          giftIncome: liveRoomInfo.giftIncome,
                                          giftPeopleCount: liveRoomInfo.giftPeopleMap.count,
                                          likeCount: livingView.getLikeCount())
        let endView = AnchorEndView(liveDataModel: liveDataModel)
        addSubview(endView)
        endView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }

    private func startLiving() {
        prepareView?.alpha = 0
        let livingView = AnchorLivingView(engineService: engineService)
        livingView.alpha = 0
        addSubview(livingView)
        livingView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        self.livingView = livingView
        UIView.animate(withDuration: 0.5) {
            livingView.alpha = 1
        } completion: { _ in
            self.prepareView?.removeFromSuperview()
            self.prepareView = nil
        }
        startLiveBlock?()
    }

    private func createRoom() {
        if liveRoomInfo.name.value.isEmpty {
            makeToast(.roomNameEmptyToast)
            return
        }
        startLiving()
        let roomInfo = TUIRoomInfo()
        roomInfo.roomType = .conference
        roomInfo.roomId = liveRoomInfo.roomId.value
        roomInfo.name = liveRoomInfo.name.value
        roomInfo.seatMode = .applyToTake
        roomInfo.roomType = .live
        roomInfo.isSeatEnabled = true
        engineService.createRoom(roomInfo: roomInfo) { [weak self] in
            self?.engineService.enterRoom(roomId: roomInfo.roomId) { [weak self] in
                let _  = self?.engineService.takeSeat(onAccepted: { [weak self] _, _ in
                    self?.engineService.openLocalMicrophone()
                })
                self?.engineService.changeSelfRole(role: .anchor)
                self?.engineService.changeSelfStatus(status: .none)
            } onError: { [weak self] code, message in
                debugPrint("code:\(code) message:\(message)")
                self?.makeToast(message)
                let statusString = String(code.rawValue) + "," + message
                let panel = AlertPanel(titleText: .enterRoomFailedTitleText,
                                       messageText: .localizedReplace(.enterRoomFailedMessageText, replace: statusString),
                                       buttonText: .confirmText) {
                    WindowUtils.getCurrentWindowViewController()?.backToPreviousPage()
                }
                PopupPanelController.alertView(panel)
            }
        } onError: { [weak self] code, message in
            debugPrint("code:\(code) message:\(message)")
            guard let self = self else{ return}
            self.makeToast(message)
            WindowUtils.getCurrentWindowViewController()?.backToPreviousPage()
        }
    }

}

private extension String {
    static var enterRoomFailedTitleText = {
        localized("live.alert.enterRoom.failed.title")
    }()

    static var enterRoomFailedMessageText = {
        localized("live.alert.enterRoom.failed.message.xxx")
    }()

    static var confirmText = {
        localized("live.alert.confirm")
    }()

    static var roomNameEmptyToast = {
        localized("live.anchor.room.name.empty.toast")
    }()
    
    static var operateFailedText: String {
        localized("live.operation.fail.xxx")
    }
}
