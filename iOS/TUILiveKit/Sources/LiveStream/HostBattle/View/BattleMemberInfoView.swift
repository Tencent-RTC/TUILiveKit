//
//  BattleMemberInfoView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/9/4.
//

import UIKit
import RTCCommon
import Combine

class BattleMemberInfoView: RTCBaseView {
    private lazy var battleIdPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.battleId))
    private lazy var connectedUserPublisher = manager.subscribeCoHostState(StateSelector(keyPath: \LSCoHostState.connectedUsers))
    private lazy var isBattleRunningPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.isBattleRunning))
    private lazy var battleUsersPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.battleUsers))
    private lazy var isOnDisplayResultPublisher = manager.subscribeBattleState(StateSelector(keyPath: \LSBattleState.isOnDisplayResult))
    
    private let manager: LiveStreamManager
    private var userId: String
    private var cancellableSet: Set<AnyCancellable> = []
    
    private var battleState: LSBattleState {
        manager.battleState
    }
    private var coHostState: LSCoHostState {
        manager.coHostState
    }
    
    private let maxRankingValue = 9
    
    init(manager: LiveStreamManager, userId: String) {
        self.manager = manager
        self.userId = userId
        super.init(frame: .zero)
    }
    
    private let scoreView: UIView = {
        let view = UIView()
        view.backgroundColor = .g2.withAlphaComponent(0.4)
        view.layer.cornerRadius = 12.scale375Height()
        return view
    }()
    
    private let rankImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = .liveBundleImage("live_battle_ranking_\(1)_icon")
        return imageView
    }()
    
    private let scoreLabel: UILabel = {
        let label = UILabel()
        label.textColor = .white
        label.font = .customFont(ofSize: 12, weight: .bold)
        label.textAlignment = .left
        label.text = "0"
        return label
    }()
    
    private let connectionView: UIView = {
        let view = UIView()
        view.backgroundColor = .g2.withAlphaComponent(0.4)
        view.layer.cornerRadius = 10.scale375Height()
        return view
    }()
    
    private let connectionStatusLabel: UILabel = {
        let label = UILabel()
        label.textColor = .white
        label.font = .customFont(ofSize: 12)
        label.textAlignment = .center
        label.text = .connectingText
        label.sizeToFit()
        return label
    }()
    
    override func constructViewHierarchy() {
        addSubview(scoreView)
        addSubview(connectionView)
        scoreView.addSubview(rankImageView)
        scoreView.addSubview(scoreLabel)
        connectionView.addSubview(connectionStatusLabel)
    }
    
    override func activateConstraints() {
        scoreView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(8.scale375Height())
            make.leading.equalToSuperview().offset(8.scale375())
            make.height.equalTo(24.scale375Height())
            make.width.equalTo(65.scale375())
        }
        rankImageView.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.leading.equalToSuperview().offset(4.scale375())
            make.width.height.equalTo(16.scale375())
        }
        scoreLabel.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.leading.equalTo(rankImageView.snp.trailing).offset(2.scale375())
            make.height.equalTo(14.scale375Height())
        }
       
        connectionView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(10.scale375Height())
            make.leading.equalToSuperview().offset(8.scale375())
            make.trailing.lessThanOrEqualToSuperview().offset(-8.scale375())
        }

        connectionStatusLabel.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview().inset(8.scale375())
            make.top.bottom.equalToSuperview().inset(4.scale375Height())
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
                    self.reset()
                }
            }
            .store(in: &cancellableSet)
        
        connectedUserPublisher
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] connectedUsers in
                guard let self = self else { return }
                onConnectedListChanged(connectionUsers: connectedUsers)
            }
            .store(in: &cancellableSet)
        
        isBattleRunningPublisher
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] start in
                guard let self = self else { return }
                self.onBattleStartChanged(start: start)
            }
            .store(in: &cancellableSet)
        
        battleUsersPublisher
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] battleUsers in
                guard let self = self else { return }
                self.onBattleScoreChanged(battleUsers: battleUsers)
            }
            .store(in: &cancellableSet)
        
        
        isOnDisplayResultPublisher
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] display in
                guard let self = self else { return }
                self.onBattleResultDisplay(display: display)
            }
            .store(in: &cancellableSet)
    }
    
    private func onConnectedListChanged(connectionUsers: [ConnectionUser]) {
        onBattleScoreChanged(battleUsers: battleState.battleUsers)
    }
    
    private func onBattleScoreChanged(battleUsers: [BattleUser]) {
        guard !battleUsers.isEmpty && !coHostState.connectedUsers.isEmpty else {
            manager.battleManager.resetBattleId()
            return
        }
        var battleUserMap: [String: BattleUser] = [:]
        for battleUser in battleUsers {
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
        isSingleBattle ? reset() : setData(user: battleUserMap[userId])
    }

    private func onBattleStartChanged(start: Bool) {
        start ? onBattleStart() : onBattleEnd()
    }
    
    private func onBattleResultDisplay(display: Bool) {
        if !display {
            reset()
        }
    }
    
    private func setData(user: BattleUser?) {
        isHidden = false
        if let user = user {
            showBattleView(show: true)
            scoreLabel.text = "\(user.score)"
            if user.ranking > 0 && user.ranking <= maxRankingValue {
                rankImageView.image = .liveBundleImage("live_battle_ranking_\(user.ranking)_icon")
            }
        } else {
            showBattleView(show: false)
        }
    }
    
    private func reset() {
        isHidden = true
        scoreView.isHidden = true
        connectionView.isHidden = true
    }
    
    private func showBattleView(show: Bool) {
        isHidden = false
        scoreView.isHidden = !show
        connectionView.isHidden = show
    }
    
    private func onBattleStart() {
        reset()
    }
    
    private func onBattleEnd() {
        guard !battleState.battleUsers.isEmpty && !coHostState.connectedUsers.isEmpty else { return }
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
        if !isSingleBattle {
            setData(user: battleUserMap[userId])
        }
    }
}

private extension String {
    static let connectingText = localized("live.connection.connecting")
}
