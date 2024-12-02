//
//  LSBattleCountDownView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/9/2.
//

import UIKit
import Combine
import RTCCommon

class LSBattleCountDownView: UIView {
    var countdownTime: TimeInterval = 5
    var timeEndClosure: (()->Void)?
    var cancelClosure: (()->Void)?
    private var dotsTimer: Timer = Timer()
    private let manager: LSBattleManager
    
    init(countdownTime: TimeInterval, manager: LSBattleManager) {
        self.manager = manager
        self.countdownTime = countdownTime
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    lazy var countdownTimer = CountdownTimer(seconds: countdownTime, onTick: { [weak self] remainingSeconds in
        guard let self = self else { return }
        self.timeLabel.text = "\(convertSecondsToMinutes(seconds: remainingSeconds))"
    }, onComplete: { [weak self] in
        guard let self = self else { return }
        self.backgroundView.stopAnimations()
        self.backgroundView.isHidden = true
        self.timeEndClosure?()
    })
    
    private lazy var backgroundView: LSBattleCountDownBackgroundView = {
        let view = LSBattleCountDownBackgroundView()
        view.countDownTimeInSeconds = Int(countdownTime)
        view.backgroundColor = .white.withAlphaComponent(0.8)
        view.layer.cornerRadius = 80.scale375()
        return view
    }()
    
    private let startBattleImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = .liveBundleImage("live_battle_start")
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
    override func layoutSubviews() {
        super.layoutSubviews()
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
            make.top.equalToSuperview().offset(204.scale375Height())
            make.centerX.equalToSuperview()
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
        manager.cancelBattleRequest()
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

class CountdownTimer {
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
    static let waitForBattleText: String = localized("live.battle.request.wait.xxx")
    static let cancelText: String = localized("live.battle.request.cancel")
}
