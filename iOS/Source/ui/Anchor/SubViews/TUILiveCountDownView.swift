//
//  TUILiveCountDownView.swift
//  TUILiveRoom
//
//  Created by jack on 2022/5/26.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation
import UIKit
import TUICore

class TUILiveCountDownView: UIView {
    
    typealias TUILiveCountDownBlock = () -> Void
    
    private(set) var isInCountdown: Bool = false
    
    private var currentValue: Int = 0
    
    private lazy var bgView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = UIColor(hex: "29CC85")
        view.layer.cornerRadius = 100
        view.layer.masksToBounds = true
        return view
    }()
    
    private lazy var countdownLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .white
        label.font = UIFont.systemFont(ofSize: 140)
        return label
    }()
    
    private var timer: DispatchSourceTimer? = nil
    
    var willDismiss: TUILiveCountDownBlock? = nil
    var didDismiss: TUILiveCountDownBlock?  = nil
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        self.alpha = 0
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        timer?.cancel()
        timer = nil
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
    }
    
    func start() {
        isInCountdown = true
        currentValue = 3
        updateUI()
        UIView.animate(withDuration: 1.0) { [weak self] in
            guard let self = self else { return }
            self.alpha = 1.0
        } completion: { [weak self] _ in
            guard let self = self else { return }
            self.startTimer()
        }
    }
    
    func stop() {
        willDismiss?()
        stopTimer()
        UIView.animate(withDuration: 0.3) { [weak self] in
            guard let self = self else { return }
            self.alpha = 0
        } completion: { [weak self] _ in
            guard let self = self else { return }
            self.isInCountdown = false
            self.didDismiss?()
        }
    }
}

// MARK: - UI Layout
extension TUILiveCountDownView {
    
    private func constructViewHierarchy() {
        addSubview(bgView)
        bgView.addSubview(countdownLabel)
    }
    
    private func activateConstraints() {
        bgView.snp.makeConstraints { make in
            make.center.equalToSuperview()
            make.width.height.equalTo(200)
        }
        countdownLabel.snp.makeConstraints { make in
            make.center.equalToSuperview()
        }
    }
}

// MARK: - Private
extension TUILiveCountDownView {
    
    private func updateUI() {
        countdownLabel.text = "\(currentValue)"
    }
    
    private func startTimer() {
        if timer != nil {
            stopTimer()
        }
        timer = DispatchSource.makeTimerSource(queue: .global())
        timer?.schedule(deadline: .now(), repeating: .seconds(1))
        timer?.setEventHandler(handler: { [weak self] in
            guard let self = self else { return }
            DispatchQueue.main.async { [weak self] in
                guard let self = self else { return }
                self.currentValue -= 1
                if self.currentValue > 0 {
                    self.updateUI()
                } else {
                    self.stop()
                }
            }
        })
        timer?.resume()
    }
    
    private func stopTimer() {
        timer?.cancel()
        timer = nil
    }
}
