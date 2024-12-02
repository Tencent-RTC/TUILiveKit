//
//  BattleInfoView.swift
//  TUILiveKit
//
//  Created by krabyu /on 2024/9/4.
//

import UIKit
import Combine
import RTCCommon

public enum BattleResultType {
    case draw
    case victory
    case defeat
}

class BattleInfoView: RTCBaseView {
    private lazy var battleIdPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.battleId))
    private lazy var connectedUsersPublisher = manager.subscribeCoHostState(StateSelector(keyPath: \LSCoHostState.connectedUsers))
    private lazy var battleUsersPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.battleUsers))
    private lazy var isBattleRunningPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.isBattleRunning))
    private lazy var receivedBattleRequestPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.receivedBattleRequest))
    private lazy var isInWaitingPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.isInWaiting))
    private lazy var durationCountDownPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.durationCountDown))
    private lazy var isOnDisplayResultPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.isOnDisplayResult))
    
    private var battleState: LSBattleState {
        manager.battleState
    }
    private var coHostState: LSCoHostState {
        manager.coHostState
    }
    
    private let manager: LiveStreamManager
    private let routerManager: LSRouterManager
    private let isOwner: Bool
    private var cancellableSet: Set<AnyCancellable> = []
    
    private var ownerId: String {
        manager.roomState.ownerInfo.userId
    }
    
    init(manager: LiveStreamManager, routerManager: LSRouterManager, isOwner: Bool) {
        self.manager = manager
        self.routerManager = routerManager
        self.isOwner = isOwner
        super.init(frame: .zero)
        backgroundColor = .clear
    }
    
    private lazy var singleBattleScoreView: SingleBattleScoreView = {
        let view = SingleBattleScoreView()
        view.isHidden = true
        return view
    }()
    
    private let battleTimeView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = .liveBundleImage("live_battle_time_background_icon")
        imageView.isHidden = true
        return imageView
    }()
    
    private let startBattleImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = .liveBundleImage("live_battle_start")
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
        button.setImage(UIImage(named: "live_battle_clock_icon"), for: .normal)
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
            make.top.equalToSuperview().offset(128.scale375Height())
            make.leading.trailing.equalToSuperview()
            make.height.equalTo(18.scale375Height())
        }
        battleTimeView.snp.makeConstraints { make in
            make.top.equalTo(singleBattleScoreView.snp.bottom).offset(-2.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalTo(72.scale375())
            make.height.equalTo(22.scale375Height())
        }
        startBattleImageView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(214.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalTo(240.scale375())
            make.height.equalTo(120.scale375Height())
        }
        battleResultImageView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(251.scale375Height())
            make.width.equalTo(234.scale375())
            make.centerX.equalToSuperview()
        }
        battleClockButton.snp.makeConstraints { make in
            make.center.equalToSuperview()
            make.width.equalTo(157.scale375())
            make.height.equalTo(156.scale375Height())
        }
    }
    
    override func bindInteraction() {
        subscribeBattleState()
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
        
        receivedBattleRequestPublisher
            .removeDuplicates(by: {(firstRequest, secondRequest) -> Bool in
                return firstRequest?.info.battleId == secondRequest?.info.battleId &&  
                firstRequest?.inviter.userId == secondRequest?.inviter.userId
            })
            .receive(on: RunLoop.main)
            .sink { [weak self] receivedRequest in
                guard let self = self else { return }
                self.onReceivedBattleRequestChanged(battleUser: receivedRequest?.inviter)
            }
            .store(in: &cancellableSet)
        
        isInWaitingPublisher
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] inWaiting in
                guard let self = self else { return }
                self.onInWaitingChanged(inWaiting: inWaiting)
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
    
    private func onConnectedUsersChanged(connectionUsers: [ConnectionUser]) {
        onBattleScoreChanged(battleUsers: battleState.battleUsers)
    }
    
    private func onBattleScoreChanged(battleUsers: [BattleUser]) {
        guard !battleState.battleUsers.isEmpty && !coHostState.connectedUsers.isEmpty else {
            manager.battleManager.resetBattleId()
            return
        }
        onBattleScoreChanged()
    }
    
    private func onBattleStartChanged(start: Bool) {
        start ? onBattleStart() : onBattleEnd()
    }
    
    private func onReceivedBattleRequestChanged(battleUser: BattleUser?) {
        guard let battleUser = battleUser else {
            routerManager.router(action: .dismiss(.alert))
            return
        }
        let alertInfo = LSAlertInfo(description: .localizedReplace(.battleInvitationText, replace: battleUser.userName),
                                    imagePath: battleUser.avatarUrl,
                                    cancelButtonInfo: (String.rejectText, .g3),
                                    defaultButtonInfo: (String.acceptText, .b1)) { [weak self] _ in
            guard let self = self else { return }
            manager.battleManager.rejectBattle()
            routerManager.router(action: .dismiss(.alert))
        } defaultClosure: { [weak self] _ in
            guard let self = self else { return }
            manager.battleManager.acceptBattle()
            routerManager.router(action: .dismiss(.alert))
        }
        routerManager.router(action: .present(.alert(info: alertInfo)))
    }
    
    private func onInWaitingChanged(inWaiting: Bool) {
        if inWaiting {
            routerManager.router(action: .present(.battleCountdown(battleRequestTimeout)))
        } else {
            let topRoute = routerManager.routerState.routeStack.last
            switch topRoute {
            case .battleCountdown(_):
                routerManager.router(action: .dismiss())
            default:
                break
            }
        }
    }
    
    private func onDurationCountDown(duration: Int) {
        updateTime(duration)
    }
    
    private func onResultDisplay(display: Bool) {
        if display {
            if let owner = battleState.battleUsers.first(where: { $0.userId == ownerId }) {
                let type: BattleResultType = manager.battleManager.isBattleDraw() ? .draw :
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
        
        var battleUserMap: [String: BattleUser] = [:]
        for battleUser in battleState.battleUsers {
            battleUserMap[battleUser.userId] = battleUser
        }
        
        // single battle: only 2 users in connecting and battling (1v1 battle)
        var singleBattleUserMap: [String: BattleUser] = [:]
        if coHostState.connectedUsers.count == 2 {
            for connectedUser in coHostState.connectedUsers {
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
            if firstUser.userId == ownerId {
                updateData(leftUser: firstUser, rightUser: secondUser)
            } else {
                updateData(leftUser: secondUser, rightUser: firstUser)
            }
        }
    }
    
    private func updateData(leftUser: BattleUser, rightUser: BattleUser) {
        singleBattleScoreView.isHidden = false
        singleBattleScoreView.updateScores(leftScore: Int(leftUser.score), rightScore: Int(rightUser.score))
    }
    
    private func onBattleStart() {
        singleBattleScoreView.isHidden = true
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
    
    private func showBattleResult(type: BattleResultType) {
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
        battleResultImageView.image = .liveBundleImage(imageName)
    }
    
    private func stopDisplayBattleResult() {
        isHidden = true
        battleResultImageView.isHidden = true
    }
}

private extension String {
    static let battleEndText = localized("live.battle.end")
    static let battleInvitationText = localized("live.battle.invitation.desc.xxx")
    static let rejectText = localized("live.alert.refuse")
    static let acceptText = localized("live.anchor.link.accept.title")
}
