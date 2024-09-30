//
//  SingleBattleScoreView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/9/4.
//

import UIKit
import SnapKit
import Combine

class SingleBattleScoreView: UIView {
    private var leftScore: Int = 0
    private var rightScore: Int = 0
    private var store: LiveStore
    private lazy var battleUsersPublisher = store.select(BattleSelectors.getBattleUsers)
    private lazy var connectedUsersPublisher = store.select(ConnectionSelectors.getConnectedUsers)
    
    private var cancellableSet: Set<AnyCancellable> = []
    
    init(store: LiveStoreProvider) {
        self.store = store
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private let leftScoreBar: UIView = {
        let view = UIView()
        view.backgroundColor = .b1
        return view
    }()
    private lazy var leftScoreLabel: UILabel = {
        let label = UILabel()
        label.textColor = .white
        label.text = "\(leftScore)"
        label.font = .customFont(ofSize: 12)
        return label
    }()
    private let rightScoreBar: UIView = {
        let view = UIView()
        view.backgroundColor = .pinkColor
        return view
    }()
    private lazy var rightScoreLabel: UILabel = {
        let label = UILabel()
        label.textColor = .white
        label.text = "\(rightScore)"
        label.font = .customFont(ofSize: 12)
        return label
    }()
    private let dividerImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = .liveBundleImage("live_battle_score_divider_icon")
        return imageView
    }()
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }
    
    func updateScore(leftScore: Int, rightScore: Int) {
        self.leftScore = leftScore
        self.rightScore = rightScore
        updateScoreBars()
    }
}

// MARK: - Layout

extension SingleBattleScoreView {
    private func constructViewHierarchy() {
        addSubview(leftScoreBar)
        addSubview(rightScoreBar)
        addSubview(dividerImageView)
        leftScoreBar.addSubview(leftScoreLabel)
        rightScoreBar.addSubview(rightScoreLabel)
    }
    
    private func activateConstraints() {
        leftScoreBar.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(2.scale375Height())
            make.leading.equalToSuperview()
            make.height.equalTo(14.scale375Height())
            make.width.equalToSuperview().multipliedBy(0.5)
        }
        dividerImageView.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.leading.equalTo(leftScoreBar.snp.trailing).offset(-9.scale375Height())
            make.height.width.equalTo(18.scale375Height())
        }
        rightScoreBar.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(2)
            make.leading.equalTo(leftScoreBar.snp.trailing)
            make.trailing.equalToSuperview()
            make.height.equalTo(leftScoreBar)
        }
        leftScoreLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(8.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(14.scale375Height())
        }
        rightScoreLabel.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-8.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(14.scale375Height())
        }
    }
    
    private func bindInteraction() {
        subscribeBattleState()
    }
    
    private func subscribeBattleState() {
        battleUsersPublisher
            .receive(on: RunLoop.main)
            .sink {  [weak self] battleUsers in
                guard let self = self else { return }
                if battleUsers.count >= 1 && self.store.selectCurrent(ConnectionSelectors.getConnectedUsers).count == 2 {
                    self.isHidden = false
                } else {
                    self.isHidden = true
                }
            }
            .store(in: &cancellableSet)
                
        connectedUsersPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] connectedUsers in
                guard let self = self else { return }
                if connectedUsers.count == 2 && self.store.selectCurrent(BattleSelectors.getBattleUsers).count >= 1 {
                    self.isHidden = false
                }
                if connectedUsers.isEmpty {
                    self.isHidden = true
                }
                
            }
            .store(in: &cancellableSet)
    }
}

// MARK: Update Score

extension SingleBattleScoreView {
    private func updateScoreBars() {
        updateScoreBarLabel()
        updateScoreBarsLayout()
    }
    
    private func updateScoreBarLabel() {
        leftScoreLabel.text = "\(leftScore)"
        rightScoreLabel.text = "\(rightScore)"
    }
    
    private func updateScoreBarsLayout() {
        let totalScore: CGFloat = CGFloat(leftScore + rightScore)
        var leftScoreBarHorizontalPercent = totalScore > 0 ? CGFloat(leftScore) / totalScore : 0.5
        
        if leftScoreBarHorizontalPercent == 0 {
            leftScoreBarHorizontalPercent += 0.1
        } else if leftScoreBarHorizontalPercent == 1 {
            leftScoreBarHorizontalPercent -= 0.1
        }
        
        UIView.animate(withDuration: 0.5) { [weak self] in
            guard let self = self else { return }
            self.leftScoreBar.snp.remakeConstraints { make in
                make.top.equalToSuperview().offset(2.scale375Height())
                make.leading.equalToSuperview()
                make.height.equalTo(14.scale375Height())
                make.width.equalToSuperview().multipliedBy(leftScoreBarHorizontalPercent)
            }
            self.rightScoreBar.snp.remakeConstraints { make in
                make.top.equalToSuperview().offset(2.scale375Height())
                make.leading.equalTo(self.leftScoreBar.snp.trailing)
                make.trailing.equalToSuperview()
                make.height.equalTo(self.leftScoreBar)
            }
            self.dividerImageView.snp.remakeConstraints { make in
                make.top.equalToSuperview()
                make.centerX.equalTo(self.leftScoreBar.snp.trailing)
                make.width.height.equalTo(18.scale375Height())
            }
            self.layoutIfNeeded()
        }
    }
}
