//
//  VRLayoutPanel.swift
//  Pods
//
//  Created by ssc on 2025/8/23.
//

import UIKit
import SnapKit

class VRLayoutPanel: UIView {
    // MARK: - UI Components
    private let manager: VoiceRoomManager
    private let routerManager: VRRouterManager
    private let titleLabel: UILabel = {
        let label = UILabel()
        label.text = .layoutSettings
        label.textColor = .g7
        label.font = .customFont(ofSize: 16)
        label.textAlignment = .center
        return label
    }()

    private lazy var backButton: UIButton = {
        let button = UIButton()
        button.setImage(internalImage("live_back_icon"), for: .normal)
        button.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return button
    }()

    private let contentView = UIView()
    private let chatBackgroundView = UIView()
    private let ktvBackgroundView = UIView()

    private lazy var chatImageView: UIImageView = {
        let view = UIImageView()
        view.image = internalImage("chat_icon")
        return view
    }()

    private lazy var ktvImageView: UIImageView = {
        let view = UIImageView()
        view.image = internalImage("ktv_icon")
        return view
    }()

    private lazy var chatLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "PingFangSC-Regular", size: 14)
        label.text = .chatRoom
        label.textColor = UIColor.white.withAlphaComponent(0.9)
        return label
    }()

    private lazy var ktvLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "PingFangSC-Regular", size: 14)
        label.text = .ktvRoom
        label.textColor = UIColor.white.withAlphaComponent(0.9)
        return label
    }()

    private var isViewReady: Bool = false

    // MARK: - Life Cycle
    init(manager: VoiceRoomManager,routerManager: VRRouterManager) {
        self.manager = manager
        self.routerManager = routerManager
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        setupView()
        updateSelection(selectedView: chatBackgroundView)
        isViewReady = true
    }

    // MARK: - Private Methods
    private func setupView() {
        backgroundColor = .bgOperateColor
        layer.cornerRadius = 20
        layer.masksToBounds = true

        [chatBackgroundView, ktvBackgroundView].forEach {
            $0.backgroundColor = UIColor(hex: "2B2C30")
            $0.layer.cornerRadius = 8
            $0.layer.borderWidth = 0
            $0.layer.borderColor = UIColor.clear.cgColor
            $0.isUserInteractionEnabled = true

            let tap = UITapGestureRecognizer(target: self, action: #selector(handleLayoutTap(_:)))
            $0.addGestureRecognizer(tap)
        }
    }

    private func updateSelection(selectedView: UIView) {
        [chatBackgroundView, ktvBackgroundView].forEach {
            $0.layer.borderWidth = 0
            $0.layer.borderColor = UIColor.clear.cgColor
        }

        selectedView.layer.borderWidth = 2
        selectedView.layer.borderColor = UIColor("2B6AD6").cgColor
    }

    @objc private func handleLayoutTap(_ gesture: UITapGestureRecognizer) {
        guard let tappedView = gesture.view else { return }

        updateSelection(selectedView: tappedView)

        if tappedView == chatBackgroundView {
            manager.onSetlayoutType(layoutType: .chatRoom)
            routerManager.router(action: .dismiss())
        } else if tappedView == ktvBackgroundView {
            manager.onSetlayoutType(layoutType: .KTVRoom)
            routerManager.router(action: .dismiss())
        }
    }
}

// MARK: - Layout
private extension VRLayoutPanel {
    func constructViewHierarchy() {
        addSubview(contentView)
        contentView.addSubview(titleLabel)
        contentView.addSubview(chatBackgroundView)
        contentView.addSubview(ktvBackgroundView)
        contentView.addSubview(backButton)
        chatBackgroundView.addSubview(chatImageView)
        chatBackgroundView.addSubview(chatLabel)

        ktvBackgroundView.addSubview(ktvImageView)
        ktvBackgroundView.addSubview(ktvLabel)
    }

    func activateConstraints() {
        snp.remakeConstraints { make in
            make.height.equalTo(200.scale375Height())
            make.width.equalTo(375.scale375())
        }

        backButton.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(24.scale375())
            make.width.height.equalTo(24.scale375())
            make.top.equalToSuperview().offset(20.scale375())
        }

        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(20.scale375())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(24.scale375())
        }

        contentView.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.width.equalTo(375.scale375())
            make.height.equalTo(200.scale375())
        }

        chatBackgroundView.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(16.scale375())
            make.top.equalTo(titleLabel.snp.bottom).offset(32.scale375())
            make.width.equalTo(165.scale375())
            make.height.equalTo(56.scale375())
        }

        chatImageView.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(12.scale375())
            make.centerY.equalToSuperview()
            make.width.height.equalTo(32.scale375())
        }

        chatLabel.snp.makeConstraints { make in
            make.left.equalTo(chatImageView.snp.right).offset(12.scale375())
            make.centerY.equalTo(chatImageView)
        }

        ktvBackgroundView.snp.makeConstraints { make in
            make.left.equalTo(chatBackgroundView.snp.right).offset(16.scale375())
            make.top.equalTo(chatBackgroundView)
            make.size.equalTo(chatBackgroundView)
        }

        ktvImageView.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(12.scale375())
            make.centerY.equalToSuperview()
            make.size.equalTo(chatImageView)
        }

        ktvLabel.snp.makeConstraints { make in
            make.left.equalTo(ktvImageView.snp.right).offset(12.scale375())
            make.centerY.equalTo(ktvImageView)
        }
    }

    @objc func backButtonClick(sender: UIButton) {
        routerManager.router(action: .dismiss())
    }
}

fileprivate extension String {
    static let ktvRoom = internalLocalized("KTV Room")
    static let chatRoom = internalLocalized("Chat Room")
    static let layoutSettings = internalLocalized("Layout settings")
}
