//
//  SettingPanel.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/15.
//

import Combine
import Foundation
import RTCCommon

class SettingPanel: UIView {
    private let settingPanelModel: FeatureClickPanelModel
    
    private let titleLabel: UILabel = {
        let view = UILabel()
        view.text = .settingTitleText
        view.textColor = .g7
        view.font = .customFont(ofSize: 16)
        view.textAlignment = .center
        return view
    }()

    private lazy var featureClickPanel: FeatureClickPanel = {
        let featureClickPanel = FeatureClickPanel(model: settingPanelModel)
        return featureClickPanel
    }()
    
    init(settingPanelModel: FeatureClickPanelModel) {
        self.settingPanelModel = settingPanelModel
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()

    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        setupView()
        isViewReady = true
    }

    private func setupView() {
        backgroundColor = .g2
        layer.cornerRadius = 20
        layer.masksToBounds = true
    }
}

// MARK: Layout

private extension SettingPanel {
    func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(featureClickPanel)
    }

    func activateConstraints() {
        snp.remakeConstraints { make in
            if isPortrait {
                make.height.equalTo(350.scale375Height())
            } else {
                make.width.equalTo(375.scale375())
            }
            make.edges.equalToSuperview()
        }

        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(20.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(24.scale375Height())
        }

        featureClickPanel.snp.makeConstraints { make in
            make.top.equalTo(titleLabel.snp.bottom).offset(32.scale375Height())
            make.centerX.equalToSuperview()
        }
    }
}

private extension String {
    static let settingTitleText: String = localized("live.anchor.setting.title")
}
