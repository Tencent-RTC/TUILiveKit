//
//  AnchorBattleCountDownView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/9/2.
//

import UIKit
import Combine
import RTCCommon
import AtomicXCore
import RTCRoomEngine

class AnchorBattleCountDownView: UIView {
    private weak var coreView: LiveCoreView?
    var countdownTime: TimeInterval = 5
    var timeEndClosure: (()->Void)?
    var cancelClosure: (()->Void)?
    private var dotsTimer: Timer = Timer()
    private let manager: AnchorBattleManager
    
    init(countdownTime: TimeInterval, manager: AnchorBattleManager, coreView: LiveCoreView) {
        self.manager = manager
        self.countdownTime = countdownTime
        self.coreView = coreView
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    lazy var countdownTimer = AnchorCountdownTimer(seconds: countdownTime, onTick: { [weak self] remainingSeconds in
        guard let self = self else { return }
        self.timeLabel.text = "\(convertSecondsToMinutes(seconds: remainingSeconds))"
    }, onComplete: { [weak self] in
        guard let self = self else { return }
        self.backgroundView.stopAnimations()
        self.backgroundView.isHidden = true
        self.timeEndClosure?()
    })
    
    private lazy var backgroundView: AnchorBattleCountDownBackgroundView = {
        let view = AnchorBattleCountDownBackgroundView()
        view.countDownTimeInSeconds = Int(countdownTime)
        view.backgroundColor = .white.withAlphaComponent(0.8)
        view.layer.cornerRadius = 80.scale375()
        return view
    }()
    
    private let startBattleImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = internalImage("live_battle_start")
        imageView.isHidden = true
        return imageView
    }()
    
    private lazy var timeLabel: UILabel = {
        let label = UILabel()
        label.textColor = .g2
        label.text = "\(convertSecondsToMinutes(seconds: countdownTime))"
        label.textAlignment = .center
        label.font = .customFont(ofSize: 40, weight: .bold)
        return label
    }()
    
    private let tipsLabel: UILabel = {
        let label = UILabel()
        label.text = .localizedReplace(.waitForBattleText, replace: "")
        label.textColor = .g3
        label.textAlignment = .center
        label.font = .customFont(ofSize: 12, weight: .medium)
        return label
    }()
    
    private let cancelButton: UIButton = {
        let button = UIButton()
        button.setTitle(.cancelText, for: .normal)
        button.setTitleColor(.redColor, for: .normal)
        button.titleLabel?.textAlignment = .center
        button.titleLabel?.font = .customFont(ofSize: 12, weight: .medium)
        return button
    }()
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        addSubview(backgroundView)
        backgroundView.addSubview(timeLabel)
        backgroundView.addSubview(tipsLabel)
        backgroundView.addSubview(cancelButton)
    }
    
    private func activateConstraints() {
        snp.makeConstraints { make in
            make.edges.equalToSuperview()
            make.height.equalTo(812.scale375Height())
            make.width.equalTo(375.scale375())
        }
        backgroundView.snp.makeConstraints { make in
            make.center.equalToSuperview()
            make.width.height.equalTo(160.scale375())
        }
        timeLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview().offset(40.scale375Height())
            make.height.equalTo(35.scale375Height())
            make.width.equalTo(117.scale375())
        }
        
        tipsLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalTo(timeLabel.snp.bottom).offset(12.scale375Height())
            make.height.equalTo(17.scale375Height())
        }
        cancelButton.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalTo(tipsLabel.snp.bottom).offset(12.scale375Height())
            make.height.equalTo(20.scale375Height())
        }
    }
    
    private func bindInteraction() {
        cancelButton.addTarget(self, action: #selector(cancelButtonClick), for: .touchUpInside)
        countdownTimer.start()
        backgroundView.startAnimations()
        updateTipsLabelText()
    }
    
    @objc private func cancelButtonClick() {
        guard let coreView = coreView else { return }
        let coHostState: CoHostState = coreView.getState()
        let userState: UserState = coreView.getState()
        let selfUserId = userState.selfInfo.userId
        let inviteeIdList = coHostState.connectedUserList.map { $0.userId }.filter({ $0 != selfUserId })
        coreView.cancelBattle(battleId: manager.state.battleId, userIdList: inviteeIdList, onSuccess: { [weak self] in
            guard let self = self else { return }
            self.manager.onCanceledBattle()
        }, onError: { _, _ in
            
        })
        cancelClosure?()
        countdownTimer.stop()
        if dotsTimer.isValid {
            dotsTimer.invalidate()
        }
    }
    
    private func convertSecondsToMinutes(seconds time: TimeInterval) -> String{
        let minutes = Int(time) / 60
        let seconds = Int(time) % 60
        return String(format: "%02d:%02d", minutes,seconds)
    }
    
    private func updateTipsLabelText() {
        var dots = ""
        dotsTimer = Timer(timeInterval: 0.5, repeats: true) { [weak self] _ in
            guard let self = self else { return }
            if dots.count == 3 {
                dots.removeAll()
            }else {
                dots.append(".")
            }
            self.tipsLabel.text? = .localizedReplace(.waitForBattleText, replace: dots)
        }
        RunLoop.current.add(dotsTimer, forMode: .default)
    }
    
    deinit {
        if dotsTimer.isValid {
            dotsTimer.invalidate()
        }
    }
}

class AnchorCountdownTimer {
    private var timer: Timer?
    private let totalSeconds: TimeInterval
    private var remainingSeconds: TimeInterval
    private let onTick: (TimeInterval) -> Void
    private let onComplete: () -> Void
    
    init(seconds: TimeInterval, onTick: @escaping (TimeInterval) -> Void, onComplete: @escaping () -> Void) {
        self.totalSeconds = seconds
        self.remainingSeconds = seconds
        self.onTick = onTick
        self.onComplete = onComplete
    }
    
    func start() {
        timer?.invalidate()
        remainingSeconds = totalSeconds
        timer = Timer.scheduledTimer(timeInterval: 1.0, target: self, selector: #selector(tick), userInfo: nil, repeats: true)
    }
    
    func stop() {
        timer?.invalidate()
        timer = nil
    }
    
    @objc private func tick() {
        if remainingSeconds > 0 {
            remainingSeconds -= 1
            onTick(remainingSeconds)
        } else {
            timer?.invalidate()
            timer = nil
            onComplete()
        }
    }
}

private extension String {
    static let waitForBattleText: String = internalLocalized("Waiting for battlexxx")
    static let cancelText: String = internalLocalized("Cancel")
}
