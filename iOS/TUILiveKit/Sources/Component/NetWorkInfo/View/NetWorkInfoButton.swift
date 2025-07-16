//
//  NetWorkInfoButton.swift
//  Pods
//
//  Created by ssc on 2025/5/12.
//
import UIKit
import SnapKit
import RTCCommon
import Combine
import RTCRoomEngine

class NetworkInfoButton: UIView {
    var onNetWorkInfoButtonClicked: (() -> Void)?

    private weak var manager: NetWorkInfoManager?
    private var cancellables = Set<AnyCancellable>()
    private var startTimestamp: TimeInterval = 0
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

    init(manager: NetWorkInfoManager) {
        self.manager = manager
        super.init(frame: .zero)
    }

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
            make.width.equalTo(74.scale375())
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
            make.width.equalTo(42.scale375())
            make.height.equalTo(16.scale375())
        }
    }

    private func bindInteraction() {
        addGestureRecognizer(UITapGestureRecognizer(target: self,action: #selector(handleTap)))

        manager?.subscribe(StateSelector(keyPath: \NetWorkInfoState.netWorkQuality))
            .sink { [weak self] netWorkQualit in
                self?.onNetWorkQualitChanged(netWorkQualit)
            }
            .store(in: &cancellables)
    }

    @objc private func handleTap() {
        onNetWorkInfoButtonClicked?()
    }

    private func onNetWorkQualitChanged(_ netWorkQualit: TUINetworkQuality) {
            DispatchQueue.main.async { [weak self] in
                switch netWorkQualit {
                    case .excellent:
                        self?.wifiImageView.image = internalImage("live_networkinfo_wifi")
                    case .good:
                        self?.wifiImageView.image = internalImage("live_networkinfo_wifi")
                    case .poor:
                        self?.wifiImageView.image = internalImage("live_networkinfo_wifi_poor")
                    case .bad:
                        self?.wifiImageView.image = internalImage("live_networkinfo_wifi_bad")
                    case .veryBad:
                        self?.wifiImageView.image = internalImage("live_networkinfo_wifi_error")
                    case .down:
                        self?.wifiImageView.image = internalImage("live_networkinfo_wifi_error")
                    default:
                        return
                }
            }
    }

    private func startTimer() {
        durationTimer = Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { [weak self] _ in
            self?.updateElapsedTime()
        }
    }
    
    private func updateElapsedTime() {
        let hours = Int(startTimestamp) / 3600
        let minutes = (Int(startTimestamp) % 3600) / 60
        let seconds = Int(startTimestamp) % 60

        DispatchQueue.main.async { [weak self] in
            self?.timeLabel.text = String(format: "%02d:%02d:%02d", hours, minutes, seconds)
            self?.startTimestamp = (self?.startTimestamp ?? 0) + 1
        }
    }

    func onRecivedCreateTime(timer: TimeInterval) {
        self.startTimestamp = Date().timeIntervalSince1970 - timer / 1000
        startTimer()
    }

    deinit {
        cancellables.forEach { $0.cancel() }
        cancellables.removeAll()
        gestureRecognizers?.forEach { removeGestureRecognizer($0) }
        durationTimer?.invalidate()
        durationTimer = nil
        print("deinit \(type(of: self))")
    }
}
