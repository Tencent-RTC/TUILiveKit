//
//  SingleBattleScoreView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/9/4.
//

import UIKit
import SnapKit
import Combine
import RTCCommon

class SingleBattleScoreView: UIView {
    private var leftScore: Int = 0
    private var rightScore: Int = 0
    
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
        isViewReady = true
    }
    
    func updateScores(leftScore: Int, rightScore: Int) {
        self.leftScore = leftScore
        self.rightScore = rightScore
        updateScoreBars()
    }
    
    func updateScores(leftScore: String, rightScore: String) {
        self.leftScoreLabel.text = leftScore
        self.rightScoreLabel.text = rightScore
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
            make.top.equalToSuperview()
            make.leading.equalToSuperview()
            make.height.equalTo(18.scale375Height())
            make.width.equalToSuperview().multipliedBy(0.5)
        }
        dividerImageView.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.leading.equalTo(leftScoreBar.snp.trailing).offset(-9.scale375Height())
            make.height.width.equalTo(18.scale375Height())
        }
        rightScoreBar.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.leading.equalTo(leftScoreBar.snp.trailing)
            make.trailing.equalToSuperview()
            make.height.equalTo(leftScoreBar)
        }
        leftScoreLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(8.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(18.scale375Height())
        }
        rightScoreLabel.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-8.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(18.scale375Height())
        }
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
                make.top.equalToSuperview()
                make.leading.equalToSuperview()
                make.height.equalTo(18.scale375Height())
                make.width.equalToSuperview().multipliedBy(leftScoreBarHorizontalPercent)
            }
            self.rightScoreBar.snp.remakeConstraints { make in
                make.top.equalToSuperview()
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
