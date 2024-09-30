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
    var onTimeEnd: (()->Void)?
    
    private var store: LiveStoreProvider
    private var durationTime: Int = 0
    private var remainingTime: Int = 0
    private var countdownTimer: Timer?
    private lazy var isBattleRunningPublisher = store.select(BattleSelectors.getIsBattleRunning)
    private lazy var battleScorePublisher = store.select(BattleSelectors.getBattleUsers)
    private lazy var connectedUsersPublisher = store.select(ConnectionSelectors.getConnectedUsers)
    private var cancellableSet: Set<AnyCancellable> = []
    
    init(store: LiveStoreProvider) {
        self.store = store
        super.init(frame: .zero)
        backgroundColor = .clear
    }
    
    private lazy var singleBattleScoreView: SingleBattleScoreView = {
        let view = SingleBattleScoreView(store: self.store)
        return view
    }()
    
    private let battleTimeView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = .liveBundleImage("live_battle_time_background_icon")
        imageView.isHidden = true
        return imageView
    }()
    
    private lazy var battleClockButton: UIButton = {
        let button = UIButton()
        button.setImage(UIImage(named: "live_battle_clock_icon"), for: .normal)
        button.setTitleColor(.white, for: .normal)
        button.titleLabel?.textAlignment = .center
        button.titleLabel?.font = UIFont.systemFont(ofSize: 16)
        button.setTitle("\(remainingTime / 60):\(String(format: "%02d", remainingTime % 60))", for: .normal)
        return button
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
        var lastIsBattleRunning = store.selectCurrent(BattleSelectors.getIsBattleRunning)
        isBattleRunningPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] isBattleRunning in
                guard let self = self, lastIsBattleRunning != isBattleRunning else { return }
                lastIsBattleRunning = isBattleRunning
                let selfUserId = store.selectCurrent(UserSelectors.getSelfInfo).userId
                if isBattleRunning {
                    let duration = self.store.selectCurrent(BattleSelectors.getBattleDuration)
                    self.startCountdown(timeInSeconds: Int(duration))
                    self.battleTimeView.isHidden = false
                    if store.selectCurrent(BattleSelectors.getBattleUsers).contains(where: {$0.userId == selfUserId}) {
                        self.showStartBattleTips()
                    }
                } else {
                    self.countdownTimer?.invalidate()
                    self.battleClockButton.setTitle(.battleEndText, for: .normal)
                    self.store.dispatch(action: BattleActions.setIsOnDisplayResult(payload: true))
                    self.showBattleResult(store: store)
                    DispatchQueue.main.asyncAfter(deadline: .now() + battleEndInfoDuration) { [weak self] in
                        guard let self = self else { return }
                        self.battleResultImageView.isHidden = true
                        self.battleTimeView.isHidden = true
                        self.singleBattleScoreView.isHidden = true
                        self.store.dispatch(action: BattleActions.clearBattleUsers())
                        self.store.dispatch(action: BattleActions.setIsOnDisplayResult(payload: false))
                    }
                }
            }
            .store(in: &cancellableSet)
        
        battleScorePublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] battleUsers in
                guard let self = self else { return }
                if battleUsers.count >= 2, self.store.selectCurrent(ConnectionSelectors.getConnectedUsers).count == 2 {
                    self.handleBattleScoreChanged(battleUsers: battleUsers)
                }
            }
            .store(in: &cancellableSet)
        
        connectedUsersPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] connectedUsers in
                guard let self = self else { return }
                if connectedUsers.isEmpty {
                    self.battleTimeView.isHidden = true
                    self.countdownTimer?.invalidate()
                }
            }
            .store(in: &cancellableSet)
    }
    
    func showStartBattleTips() {
        self.startBattleImageView.isHidden = false
        DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
            self.startBattleImageView.isHidden = true
        }
    }
    
    private func handleBattleScoreChanged(battleUsers: [BattleUser]) {
        let renderModel = VideoRenderModel(battleUser: battleUsers[0])
        guard let videoRenderCell = store.renderManager.findRenderView(renderModel)?.superview?.superview else { return }
        
        let isVideoViewInLeft = videoRenderCell.frame.origin.x < UIScreen.main.bounds.width / 2
        let leftScore = isVideoViewInLeft ? Int(battleUsers[0].score) : Int(battleUsers[1].score)
        let rightScore = isVideoViewInLeft ? Int(battleUsers[1].score) : Int(battleUsers[0].score)
        
        updateSingleBattleScore(leftScore: leftScore, rightScore: rightScore)
    }
    
    func startCountdown(timeInSeconds time: Int) {
        durationTime = time
        remainingTime = durationTime
        updateTime(time)
        countdownTimer = Timer.scheduledTimer(timeInterval: 1.0, target: self, selector: #selector(tick), userInfo: nil, repeats: true)
    }
    
    func updateSingleBattleScore(leftScore: Int, rightScore: Int) {
        singleBattleScoreView.updateScore(leftScore: leftScore, rightScore: rightScore)
    }
    
    func showBattleResult(store: LiveStore) {
        var imageName = ""
        switch getBattleResultType(store: store) {
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
    
    private func getBattleResultType(store: LiveStore) -> BattleResultType {
        let battleUsers = store.selectCurrent(BattleSelectors.getBattleUsers)
        
        if areAllRankingsEqual(users: battleUsers) {
            return .draw
        }
        
        guard let winner = battleUsers.sorted(by: { $0.ranking < $1.ranking }).first else { return .draw }
        if winner.userId == store.selectCurrent(RoomSelectors.getRoomOwnerInfo).userId {
            return .victory
        } else {
            return .defeat
        }
    }
    
    private func areAllRankingsEqual(users: [BattleUser]) -> Bool {
        guard let firstRanking = users.first?.ranking else {
            return true
        }
        
        for user in users where user.ranking != firstRanking {
            return false
        }
        
        return true
    }
    
    deinit {
        if let timer = countdownTimer, timer.isValid {
            timer.invalidate()
        }
    }
}

extension BattleInfoView {
    @objc private func tick() {
        if remainingTime > 0 {
            remainingTime -= 1
            updateTime(remainingTime)
        } else {
            countdownTimer?.invalidate()
            countdownTimer = nil
            onTimeEnd?()
            battleClockButton.setTitle(.battleEndText, for: .normal)
        }
    }
    
    private func updateTime(_ time: Int){
        battleClockButton.setTitle(String(format: "%d:%02d", time / 60 ,time % 60), for: .normal)
    }
}

private extension String {
    static let battleEndText = localized("live.battle.end")
}
