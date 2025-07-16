//
//  NetWorkInfoLoadingView.swift
//  Pods
//
//  Created by ssc on 2025/5/16.
//


import UIKit
import SnapKit

class NetworkStatusToastView: UIView {
    // MARK: - UI Elements
    var onCloseButtonTapped: (() -> Void)?
    private lazy var containerView: UIView = {
        let view = UIView()
        view.backgroundColor = .blackColor.withAlphaComponent(0.55)
        view.layer.borderColor = UIColor.white.withAlphaComponent(0.14).cgColor
        view.layer.borderWidth = 1
        view.layer.cornerRadius = 6
        view.layer.shadowColor = UIColor.black.cgColor
        view.layer.shadowOpacity = 0.12
        view.layer.shadowOffset = CGSize(width: 0, height: 1)
        view.layer.shadowRadius = 5
        return view
    }()

    private lazy var iconImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = internalImage("live_networkinfo_tips")
        return imageView
    }()

    private lazy var messageLabel: UILabel = {
        let label = UILabel()
        label.text = .netWorkWarning
        label.font = UIFont(name: "PingFangSC-Medium", size: 14)
        label.textColor = UIColor.white.withAlphaComponent(0.9)
        return label
    }()

    private lazy var switchNetWorkButton: UIButton = {
        let button = UIButton()
        button.setTitle(.SwitchNetWork, for: .normal)
        button.setTitleColor(.blueColor, for: .normal)
        button.titleLabel?.font = UIFont.systemFont(ofSize: 14, weight: .medium)
        return button
    }()

    private lazy var closeButton: UIButton = {
        let button = UIButton()
        button.setImage(internalImage("live_leave_icon"), for: .normal)
        button.tintColor = UIColor.white.withAlphaComponent(0.9)
        return button
    }()

    // MARK: - Initialization
    override init(frame: CGRect) {
        super.init(frame: frame)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }

    private func constructViewHierarchy() {
        addSubview(containerView)
        containerView.addSubview(iconImageView)
        containerView.addSubview(messageLabel)
        containerView.addSubview(closeButton)
        containerView.addSubview(switchNetWorkButton)
    }

    private func activateConstraints() {
        containerView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        
        iconImageView.snp.makeConstraints { make in
            make.width.height.equalTo(16.scale375())
            make.leading.equalToSuperview().offset(16.scale375())
            make.centerY.equalToSuperview()
        }

        messageLabel.snp.makeConstraints { make in
            make.leading
                .equalTo(iconImageView.snp.trailing)
                .offset(4.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(22.scale375())
        }

        switchNetWorkButton.snp.makeConstraints { make in
            make.leading.equalTo(messageLabel.snp.trailing)
            make.centerY.equalToSuperview()
            make.height.equalTo(22.scale375())
        }

        closeButton.snp.makeConstraints { make in
            make.width.height.equalTo(16.scale375())
            make.trailing.equalToSuperview().offset(-16.scale375())
            make.centerY.equalToSuperview()
        }
    }

    private func bindInteraction() {
        switchNetWorkButton.addTarget(self, action: #selector(switchNetWorkButtonTapped), for: .touchUpInside)
        closeButton.addTarget(self, action: #selector(closeButtonTapped), for: .touchUpInside)
    }

    @objc private func switchNetWorkButtonTapped() {
        if let url = URL(string: UIApplication.openSettingsURLString) {
            UIApplication.shared.open(url, options: [:], completionHandler: nil)
        }
    }

    @objc private func closeButtonTapped() {
        self.onCloseButtonTapped?()
    }

}

fileprivate extension String {
    static let netWorkWarning = internalLocalized("Network lag detected suggestions")
    static let SwitchNetWork = internalLocalized("Switch Network")
}
