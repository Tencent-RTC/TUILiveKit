//
//  NetWorkInfoButton.swift
//  Pods
//
//  Created by ssc on 2025/5/12.
//
import AtomicXCore
import Combine
import RTCCommon
import RTCRoomEngine
import SnapKit
import UIKit

class NetworkInfoButton: UIView {
    var onNetWorkInfoButtonClicked: (() -> Void)?

    private weak var manager: NetWorkInfoManager?
    private var cancellableSet = Set<AnyCancellable>()
    private var durationTimestamp: TimeInterval = 0
    private var durationTimer: Timer?
    private let backgroundView: UIView = {
        let view = UIView()
        view.backgroundColor = UIColor.black.withAlphaComponent(0.25)
        view.layer.cornerRadius = 11
        return view
    }()

    private let wifiImageView: UIImageView = {
        let view = UIImageView()
        view.image = internalImage("live_networkinfo_wifi")
        return view
    }()

    private let timeLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "PingFangSC-Regular", size: 10)
        label.textColor = UIColor.white.withAlphaComponent(0.9)
        label.text = "00:00:00"
        return label
    }()

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }

    private let liveId: String

    init(liveId: String, manager: NetWorkInfoManager) {
        self.liveId = liveId
        self.manager = manager
        super.init(frame: .zero)
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func constructViewHierarchy() {
        addSubview(backgroundView)
        backgroundView.addSubview(wifiImageView)
        backgroundView.addSubview(timeLabel)
    }

    private func activateConstraints() {
        backgroundView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
            make.height.equalTo(20.scale375())
        }

        wifiImageView.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(4.scale375())
            make.centerY.equalToSuperview()
            make.width.height.equalTo(14.scale375())
        }

        timeLabel.snp.makeConstraints { make in
            make.left.equalTo(wifiImageView.snp.right).offset(4.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(16.scale375())
            make.right.equalToSuperview().inset(2.scale375())
        }
    }

    private func bindInteraction() {
        addGestureRecognizer(UITapGestureRecognizer(target: self, action: #selector(handleTap)))

        manager?.subscribe(StateSelector(keyPath: \NetWorkInfoState.netWorkQuality))
            .receive(on: RunLoop.main)
            .sink { [weak self] netWorkQuality in
                guard let self = self else { return }
                onNetWorkQualityChanged(netWorkQuality)
            }
            .store(in: &cancellableSet)

        subscribeState()
    }

    @objc private func handleTap() {
        onNetWorkInfoButtonClicked?()
    }

    private func onNetWorkQualityChanged(_ netWorkQuality: TUINetworkQuality) {
        switch netWorkQuality {
            case .excellent:
                wifiImageView.image = internalImage("live_networkinfo_wifi")
            case .good:
                wifiImageView.image = internalImage("live_networkinfo_wifi")
            case .poor:
                wifiImageView.image = internalImage("live_networkinfo_wifi_poor")
            case .bad:
                wifiImageView.image = internalImage("live_networkinfo_wifi_bad")
            case .veryBad:
                wifiImageView.image = internalImage("live_networkinfo_wifi_error")
            case .down:
                wifiImageView.image = internalImage("live_networkinfo_wifi_error")
            default:
                return
        }
    }

    private func startTimer() {
        stopTimer()
        durationTimer = Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { [weak self] _ in
            guard let self = self else { return }
            updateElapsedTime()
        }
    }

    private func stopTimer() {
        durationTimer?.invalidate()
        durationTimer = nil
    }

    private func updateElapsedTime() {
        let hours = Int(durationTimestamp) / 3600
        let minutes = (Int(durationTimestamp) % 3600) / 60
        let seconds = Int(durationTimestamp) % 60
        timeLabel.text = String(format: "%02d:%02d:%02d", hours, minutes, seconds)
        durationTimestamp += 1
    }

    private func subscribeState() {
        LiveListStore.shared.state.subscribe(StatePublisherSelector(keyPath: \LiveListState.currentLive))
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] currentLive in
                guard let self = self else { return }
                if currentLive.liveID == liveId {
                    durationTimestamp = Date().timeIntervalSince1970 - TimeInterval(currentLive.createTime) / 1000
                    if currentLive.createTime == 0 || durationTimestamp < 0 {
                        durationTimestamp = 0
                    }
                    startTimer()
                } else {
                    stopTimer()
                }
            }
            .store(in: &cancellableSet)
    }

    deinit {
        cancellableSet.forEach { $0.cancel() }
        cancellableSet.removeAll()
        gestureRecognizers?.forEach { removeGestureRecognizer($0) }
        stopTimer()
        print("deinit \(type(of: self))")
    }
}
