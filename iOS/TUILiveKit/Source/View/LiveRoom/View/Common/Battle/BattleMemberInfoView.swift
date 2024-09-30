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
    
    private var store: LiveStore
    private var userId: String = ""
    private lazy var isBattleRunningPublisher = store.select(BattleSelectors.getIsBattleRunning)
    private lazy var battleUsersPublisher = store.select(BattleSelectors.getBattleUsers)
    private lazy var connectedUsersPublisher = store.select(ConnectionSelectors.getConnectedUsers)
    
    private var cancellableSet: Set<AnyCancellable> = []
    
    init(store: LiveStore) {
        self.store = store
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
            make.height.equalTo(24.scale375Height())
            make.width.equalTo(48.scale375())
        }
        connectionStatusLabel.snp.makeConstraints { make in
            make.center.equalToSuperview()
            make.width.equalTo(36.scale375())
            make.height.equalTo(20.scale375Height())
        }
    }
    
    override func bindInteraction() {
        subscribeBattleState()
    }
    
    private func subscribeBattleState() {
        var lastIsBattleRunning = store.selectCurrent(BattleSelectors.getIsBattleRunning)
        isBattleRunningPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] isBattleRunning in
                guard let self = self, lastIsBattleRunning != isBattleRunning else { return }
                lastIsBattleRunning = isBattleRunning
                if !isBattleRunning {
                    DispatchQueue.main.asyncAfter(deadline: .now() + battleEndInfoDuration) { [weak self] in
                        guard let self = self else { return }
                        self.scoreView.isHidden = true
                        self.isHidden = true
                        self.store.dispatch(action: BattleActions.clearBattleUsers())
                    }
                }
            }
            .store(in: &cancellableSet)
        
        battleUsersPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] battleUsers in
                guard let self = self else { return }
                self.updateView(battleUsers: battleUsers)
                if let selfBattleInfo = battleUsers.filter({ $0.userId == self.userId}).first {
                    self.updateSelfBattleInfo(info: selfBattleInfo)
                }
                
            }
            .store(in: &cancellableSet)
        
        connectedUsersPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] connectedUsers in
                guard let self = self else { return }
                if connectedUsers.count < 3 {
                    self.isHidden = true
                }
            }
            .store(in: &cancellableSet)
    }
    
    func updateUserInfo(_ renderModel: VideoRenderModel) {
        userId = renderModel.userId
    }
    
    private func updateView(battleUsers: [BattleUser]) {
        self.isHidden = !isMultiBattleLayout(battleUsers: battleUsers)
        updateDecorationViewPresentation()
    }
    
    private func updateSelfBattleInfo(info: BattleUser ) {
        scoreLabel.text = "\(info.score)"
        rankImageView.image = .liveBundleImage("live_battle_ranking_\(info.ranking)_icon")
    }
    
    private func updateDecorationViewPresentation() {
        let isOnBattle = store.selectCurrent(BattleSelectors.getBattleUsers).contains(where: { $0.userId == userId })
        showBattleView(isPresent: isOnBattle)
    }
    
    private func isMultiBattleLayout(battleUsers: [BattleUser]) -> Bool {
        return battleUsers.count >= 1 && store.selectCurrent(ConnectionSelectors.getConnectedUsers).count > 2
    }
    
    private func showBattleView(isPresent: Bool) {
        scoreView.isHidden = !isPresent
        connectionView.isHidden = isPresent
    }
}

private extension String {
    static let connectingText = localized("live.connection.connecting")
}
