//
//  StreamDashboardPanel.swift
//  TUILiveKit
//
//  Created by jack on 2024/11/21.
//

import Foundation
#if canImport(TXLiteAVSDK_TRTC)
import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
import TXLiteAVSDK_Professional
#endif

class StreamDashboardPanel: UIView {
    
    private let manager: StreamDashboardManager
    
    private lazy var titleLabel: UILabel = {
        let label = UILabel()
        label.text = .dashboardText
        label.font = .customFont(ofSize: 16.0, weight: .medium)
        label.textColor = .g7
        label.textAlignment = .center
        return label
    }()
    
    private lazy var networkInfoView: StreamDashboardNetView = {
        let view = StreamDashboardNetView(manager: manager)
        
        return view
    }()
    
    private lazy var mediaView: StreamDashboardMediaView = {
        let view = StreamDashboardMediaView(manager: manager)
        
        return view
    }()
    
    init(roomId: String, trtcCloud: TRTCCloud) {
        let service = EngineStreamDashboardService(trtcCloud: trtcCloud)
        self.manager = StreamDashboardManager(service: service, roomId: roomId)
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        manager.removeTRTCEvent()
    }
    
    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        setupViewStyle()
    }
    
}

extension StreamDashboardPanel {
    
    private func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(networkInfoView)
        addSubview(mediaView)
    }
    
    private func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(10)
            make.centerX.equalToSuperview()
        }
        networkInfoView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.top.equalTo(titleLabel.snp.bottom).offset(10)
        }
        mediaView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.top.equalTo(networkInfoView.snp.bottom).offset(10)
            make.bottom.equalToSuperview()
        }
    }
    
    private func bindInteraction() {
        manager.addTRTCEvent()
    }
    
    private func setupViewStyle() {
        backgroundColor = .black.withAlphaComponent(0.1)
        layer.cornerRadius = 16
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
    }
}


fileprivate extension String {
    static let dashboardText = localized("live.streamDashboard.title")
}
