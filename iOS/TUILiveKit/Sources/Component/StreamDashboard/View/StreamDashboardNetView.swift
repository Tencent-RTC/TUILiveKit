//
//  StreamDashboardNetView.swift
//  TUILiveKit
//
//  Created by jack on 2024/11/21.
//

import Foundation
import Combine
import RTCCommon

class StreamDashboardNetView: UIView {
    
    private lazy var netInfoStackView: UIStackView = {
        let view = UIStackView(frame: .zero)
        view.backgroundColor = .clear
        view.axis = .horizontal
        view.alignment = .fill
        view.distribution = .fillEqually
        view.contentMode = .scaleToFill
        return view
    }()
    
    private lazy var rttInfoView: StreamDashboardNetItemView = {
        let info = StreamDashboardNetItemView(name:.rttText)
        info.backgroundColor = .clear
        info.setValue(value: "0ms", status: .normal)
        return info
    }()
    
    private lazy var downLossInfoView: StreamDashboardNetItemView = {
        let info = StreamDashboardNetItemView(name:.downLossText)
        info.backgroundColor = .clear
        info.setValue(value: "0%", status: .normal)
        return info
    }()
    
    private lazy var upLossInfoView: StreamDashboardNetItemView = {
        let info = StreamDashboardNetItemView(name:.upLossText)
        info.backgroundColor = .clear
        info.setValue(value: "0%", status: .normal)
        return info
    }()
    
    private var cancellableSet: Set<AnyCancellable> = []
    private weak var manager: StreamDashboardManager?
    init(manager: StreamDashboardManager) {
        self.manager = manager
        super.init(frame: .zero)
        
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        subscribeState()
    }
}

extension StreamDashboardNetView {
    
    private func constructViewHierarchy() {
        addSubview(netInfoStackView)
        netInfoStackView.addArrangedSubview(rttInfoView)
        netInfoStackView.addArrangedSubview(downLossInfoView)
        netInfoStackView.addArrangedSubview(upLossInfoView)
    }
    
    private func activateConstraints() {
        netInfoStackView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview().inset(20)
            make.top.bottom.equalToSuperview()
        }
    }
    
    private func subscribeState() {
        manager?.subscribe(StateSelector(keyPath: \StreamDashboardState.rtt))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] rtt in
                guard let self = self else { return }
                self.rttInfoView.setValue(value: "\(rtt)ms", status: getRttStatus(rtt: rtt))
            }
            .store(in: &cancellableSet)
        manager?.subscribe(StateSelector(keyPath: \StreamDashboardState.downLoss))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] downLoss in
                guard let self = self else { return }
                self.downLossInfoView.setValue(value: "\(downLoss)%",status: getLossStatus(loss: downLoss))
            }
            .store(in: &cancellableSet)
        manager?.subscribe(StateSelector(keyPath: \StreamDashboardState.upLoss))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] upLoss in
                guard let self = self else { return }
                self.upLossInfoView.setValue(value: "\(upLoss)%", status: getLossStatus(loss: upLoss))
            }
            .store(in: &cancellableSet)
    }
    
    private func getRttStatus(rtt: UInt32) -> StreamDashboardNetItemView.StatusType {
        if (rtt > 100) {
            return .warning
        }
        return .normal
    }
    
    private func getLossStatus(loss: UInt32) -> StreamDashboardNetItemView.StatusType {
        if (loss > 10) {
            return .warning
        }
        return .normal
    }
    
}


fileprivate class StreamDashboardNetItemView: UIView {
    
    enum StatusType {
        case normal
        case warning
    }
    
    private lazy var valueLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 24, weight: .semibold)
        label.textAlignment = .center
        label.numberOfLines = 1
        label.adjustsFontSizeToFitWidth = true
        label.minimumScaleFactor = 0.5
        return label
    }()
    
    private lazy var nameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 12)
        label.textColor = .white
        label.alpha = 0.6
        label.textAlignment = .center
        label.numberOfLines = 1
        label.adjustsFontSizeToFitWidth = true
        label.minimumScaleFactor = 0.5
        return label
    }()
    
    init(name: String, frame: CGRect = .zero) {
        super.init(frame: frame)
        self.nameLabel.text = name
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
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
    
    func setValue(value: String, status: StatusType) {
        valueLabel.text = value
        if status == .normal {
            valueLabel.textColor = UIColor.tui_color(withHex: "47d4ab")
        }
        if status == .warning {
            valueLabel.textColor = UIColor.tui_color(withHex: "f95f91")
        }
    }
    
    private func constructViewHierarchy() {
        addSubview(nameLabel)
        addSubview(valueLabel)
    }
    
    private func activateConstraints() {
        valueLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalToSuperview()
        }
        nameLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.bottom.equalToSuperview()
            make.top.equalTo(valueLabel.snp.bottom).offset(5.scale375Height())
        }
    }
}

fileprivate extension String {
    static let rttText = internalLocalized("RTT")
    static let downLossText = internalLocalized("DownLoss")
    static let upLossText = internalLocalized("UpLoss")
}
