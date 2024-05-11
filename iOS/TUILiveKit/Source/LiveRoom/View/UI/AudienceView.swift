//
//  AudienceView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/10/19.
//

import Foundation

enum AudienceViewState {
    case `default`
    case willDisplay
    case didDisplay
    case didEndDisplay
    case destroy
}

class AudienceView: UIView {
    @WeakLazyInjected var engineManager:EngineServiceProvider?
    var state: Observable<AudienceViewState> = Observable(.default)
    var isEnteringRoom = false
    var roomId: String

    private(set) lazy var engineService: RoomEngineService = {
        guard let engineManager = engineManager else {
            return RoomEngineService(liveRoomInfo: LiveRoomInfo())
        }
        return engineManager.getRoomEngineService(roomId: roomId)
    }()

    var liveRoomInfo: LiveRoomInfo {
        engineService.liveRoomInfo
    }

    lazy var videoView: AudienceVideoView = {
        AudienceVideoView(engineService: self.engineService)
    }()

    lazy var livingView: AudienceLivingView = {
        AudienceLivingView(engineService: self.engineService)
    }()

    lazy var audienceEndView: AudienceEndView = {
        let view = AudienceEndView(avatarUrl: liveRoomInfo.anchorInfo.value.avatarUrl.value,
                                    userName: liveRoomInfo.anchorInfo.value.name.value)
        view.isHidden = true
        return view
    }()

    init(roomId: String) {
        self.roomId = roomId
        super.init(frame: .zero)
        initFunction()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }

    deinit {
        debugPrint("deinit \(self)")
        deinitFunction()
    }
}

// MARK: Layout

extension AudienceView {
    func constructViewHierarchy() {
        backgroundColor = .clear
        addSubview(videoView)
        addSubview(livingView)
        addSubview(audienceEndView)
    }

    func activateConstraints() {
        videoView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })

        livingView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }

        audienceEndView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
}

private extension String {
    static var enterRoomFailedMessageText = {
        localized("live.alert.enterRoom.failed.message.xxx")
    }()
}
