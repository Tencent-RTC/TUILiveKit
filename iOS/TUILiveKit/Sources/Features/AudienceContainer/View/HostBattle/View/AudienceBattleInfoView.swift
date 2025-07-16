//
//  AudienceBattleInfoView.swift
//  TUILiveKit
//
//  Created by krabyu /on 2024/9/4.
//

import UIKit
import Combine
import RTCCommon
import RTCRoomEngine
import LiveStreamCore

public enum AudienceBattleResultType {
    case draw
    case victory
    case defeat
}

class AudienceBattleInfoView: RTCBaseView {
    private lazy var battleIdPublisher = manager.subscribeState(StateSelector(keyPath: \AudienceBattleState.battleId))
    private lazy var connectedUsersPublisher = manager.subscribeCoreViewState(StateSelector(keyPath: \CoHostState.connectedUserList))
    private lazy var battleUsersPublisher = manager.subscribeState(StateSelector(keyPath: \AudienceBattleState.battleUsers))
    private lazy var isBattleRunningPublisher = manager.subscribeState(StateSelector(keyPath: \AudienceBattleState.isBattleRunning))
    private lazy var durationCountDownPublisher = manager.subscribeState(StateSelector(keyPath: \AudienceBattleState.durationCountDown))
    private lazy var isOnDisplayResultPublisher = manager.subscribeState(StateSelector(keyPath: \AudienceBattleState.isOnDisplayResult))
    
    private var battleState: AudienceBattleState {
        manager.battleState
    }
    private var coHostState: CoHostState {
        manager.coreCoHostState
    }
    
    private weak var coreView: LiveCoreView?
    private let manager: AudienceManager
    private let routerManager: AudienceRouterManager
    private let isOwner: Bool
    private var cancellableSet: Set<AnyCancellable> = []
    private var isBattleStarted = false
    
    private var ownerId: String {
        manager.coreRoomState.ownerInfo.userId
    }
    
    init(manager: AudienceManager, routerManager: AudienceRouterManager, isOwner: Bool, coreView: LiveCoreView) {
        self.manager = manager
        self.routerManager = routerManager
        self.isOwner = isOwner
        self.coreView = coreView
        super.init(frame: .zero)
        backgroundColor = .clear
        self.isUserInteractionEnabled = false
    }
    
    private lazy var singleBattleScoreView: AnchorSingleBattleScoreView = {
        let view = AnchorSingleBattleScoreView()
        view.isHidden = true
        return view
    }()
    
    private let battleTimeView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = internalImage("live_battle_time_background_icon")
        imageView.isHidden = true
        return imageView
    }()
    
    private let startBattleImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = internalImage("live_battle_start")
        imageView.isHidden = true
        return imageView
    }()
    
    private let battleResultImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.contentMode = .scaleAspectFit
        imageView.isHidden = true
        return imageView
    }()
    
    private lazy var battleClockButton: UIButton = {
        let button = UIButton()
        button.setImage(internalImage("live_battle_clock_icon"), for: .normal)
        button.setTitleColor(.white, for: .normal)
        button.titleLabel?.textAlignment = .center
        button.titleLabel?.font = UIFont.systemFont(ofSize: 16)
        button.setTitle("0:00", for: .normal)
        return button
    }()
    
    override func constructViewHierarchy() {
        addSubview(singleBattleScoreView)
        addSubview(battleTimeView)
        addSubview(startBattleImageView)
        addSubview(battleResultImageView)
        battleTimeView.addSubview(battleClockButton)
    }
    
    override func activateConstraints() {
        singleBattleScoreView.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.leading.trailing.equalToSuperview()
            make.height.equalTo(18.scale375Height())
        }
        battleTimeView.snp.makeConstraints { make in
            make.top.equalTo(singleBattleScoreView.snp.bottom)
            make.centerX.equalToSuperview()
            make.width.equalTo(72.scale375())
            make.height.equalTo(22.scale375Height())
        }
        startBattleImageView.snp.makeConstraints { make in
            make.center.equalToSuperview()
            make.width.equalTo(240.scale375())
            make.height.equalTo(120.scale375Height())
        }
        battleResultImageView.snp.makeConstraints { make in
            make.center.equalToSuperview()
            make.width.equalTo(234.scale375())
        }
        battleClockButton.snp.makeConstraints { make in
            make.center.equalToSuperview()
            make.width.equalTo(157.scale375())
            make.height.equalTo(156.scale375Height())
        }
    }
    
    override func bindInteraction() {
        subscribeBattleState()
        subscribeFloatWindowState()
    }
    
    override var isHidden: Bool {
        didSet {
            if !isHidden && FloatWindow.shared.isShowingFloatWindow() {
                isHidden = true
            }
        }
    }

    func updateView(userInfos: [LiveStreamCore.BattleUserViewModel]) {
        userInfos.forEach { battleUserViewModel in
            for (index, battleUser) in manager.battleManager.state.battleUsers.enumerated() {
                if battleUser.userId == battleUserViewModel.battleUser.userId {
                    manager.battleManager.updateBattleUserRectFromIndex(rect: battleUserViewModel.rect, index: index)
                    break
                }
            }
        }
        onBattleScoreChanged()
    }
    
    private func subscribeBattleState() {
        battleIdPublisher
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] battleId in
                guard let self = self else { return }
                if battleId.isEmpty {
                    self.stopDisplayBattleResult()
                }
            }
            .store(in: &cancellableSet)
        
        connectedUsersPublisher
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] connectedUsers in
                guard let self = self else { return }
                self.onConnectedUsersChanged(connectionUsers: connectedUsers)
            }
            .store(in: &cancellableSet)
        
        isBattleRunningPublisher
            .dropFirst()
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] start in
                guard let self = self else { return }
                self.onBattleStartChanged(start: start)
            }
            .store(in: &cancellableSet)
        
        battleUsersPublisher
            .dropFirst()
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] battleUsers in
                guard let self = self else { return }
                self.onBattleScoreChanged(battleUsers: battleUsers)
            }
            .store(in: &cancellableSet)
        
        durationCountDownPublisher
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] duration in
                guard let self = self else { return }
                self.onDurationCountDown(duration: duration)
            }
            .store(in: &cancellableSet)
        
        isOnDisplayResultPublisher
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] display in
                guard let self = self else { return }
                self.onResultDisplay(display: display)
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeFloatWindowState() {
        FloatWindow.shared.subscribeShowingState()
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] isShow in
                guard let self = self, isBattleStarted else { return }
                isHidden = isShow
            }
            .store(in: &cancellableSet)
    }
    
    private func onConnectedUsersChanged(connectionUsers: [TUIConnectionUser]) {
        onBattleScoreChanged(battleUsers: battleState.battleUsers)
    }
    
    private func onBattleScoreChanged(battleUsers: [AudienceBattleUser]) {
        guard !battleState.battleUsers.isEmpty && !coHostState.connectedUserList.isEmpty else {
            manager.onBattleExited()
            return
        }
        onBattleScoreChanged()
    }
    
    private func onBattleStartChanged(start: Bool) {
        start ? onBattleStart() : onBattleEnd()
    }
    
    private func onDurationCountDown(duration: Int) {
        updateTime(duration)
    }
    
    private func onResultDisplay(display: Bool) {
        if display {
            if let owner = battleState.battleUsers.first(where: { $0.userId == ownerId }) {
                let type: AnchorBattleResultType = manager.battleManager.isBattleDraw() ? .draw :
                    (owner.ranking == 1 ? .victory : .defeat)
                showBattleResult(type: type)
            }
        } else {
            stopDisplayBattleResult()
        }
    }
    
    private func onBattleScoreChanged() {
        let battleUsers = battleState.battleUsers
        guard !battleUsers.isEmpty else { return }
        
        var battleUserMap: [String: AudienceBattleUser] = [:]
        for battleUser in battleState.battleUsers {
            battleUserMap[battleUser.userId] = battleUser
        }
        
        // single battle: only 2 users in connecting and battling (1v1 battle)
        var singleBattleUserMap: [String: AudienceBattleUser] = [:]
        if coHostState.connectedUserList.count == 2 {
            for connectedUser in coHostState.connectedUserList {
                if let battleUser = battleUserMap[connectedUser.userId] {
                    singleBattleUserMap[battleUser.userId] = battleUser
                }
            }
        }
        
        let isSingleBattle = singleBattleUserMap.count == 2
        if isSingleBattle {
            let userList = Array(singleBattleUserMap.values)
            // owner on left
            let firstUser = userList[0]
            let secondUser = userList[1]
            if firstUser.rect.origin.x < secondUser.rect.origin.x {
                updateData(leftUser: firstUser, rightUser: secondUser)
            } else {
                updateData(leftUser: secondUser, rightUser: firstUser)
            }
        }
    }
    
    private func updateData(leftUser: AudienceBattleUser, rightUser: AudienceBattleUser) {
        singleBattleScoreView.isHidden = false
        singleBattleScoreView.updateScores(leftScore: Int(leftUser.score), rightScore: Int(rightUser.score))
//        singleBattleScoreView.updateScores(leftScore: leftUser.userId, rightScore: rightUser.userId)
    }
        
    private func onBattleStart() {
        isBattleStarted = true
        isHidden = false
        battleTimeView.isHidden = false
        if isOwner && battleState.isShowingStartView {
            startBattleImageView.isHidden = false
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                self.startBattleImageView.isHidden = true
            }
        }
    }
    
    private func onBattleEnd() {
        isHidden = false
        battleClockButton.setTitle(.battleEndText, for: .normal)
    }
    
    private func updateTime(_ time: Int){
        let title = time == 0 ? String.battleEndText : String(format: "%d:%02d", time / 60 ,time % 60)
        battleClockButton.setTitle(title, for: .normal)
    }
    
    private func showBattleResult(type: AnchorBattleResultType) {
        var imageName = ""
        switch type {
            case .draw:
                imageName = "live_battle_result_draw_icon"
            case .victory:
                imageName = "live_battle_result_win_icon"
            case .defeat:
                imageName = "live_battle_result_lose_icon"
        }
        
        battleResultImageView.isHidden = false
        battleResultImageView.image = internalImage(imageName)
    }
    
    private func stopDisplayBattleResult() {
        isBattleStarted = false
        isHidden = true
        battleResultImageView.isHidden = true
    }
}

private extension String {
    static let battleEndText = internalLocalized("PK End")
    }
