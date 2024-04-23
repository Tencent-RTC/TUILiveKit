//
//  AlertPanel.swift
//  TUIKitCommon
//
//  Created by krabyu on 2023/11/14.
//

import SnapKit
import UIKit

public class AlertDesignConfig {
    var titleTextColor: UIColor? = UIColor(0x000000)
    var titleTextFont: UIFont? = UIFont(name: "PingFangSC-Regular", size: 18)
    var messageColor: UIColor? = UIColor(0x4F586B)
    var messageFont: UIFont? = UIFont(name: "PingFangSC-Regular", size: 14)
    var buttonTextColor: UIColor? = UIColor(0x1C66E5)
    var buttonTextFont: UIFont? = UIFont(name: "PingFangSC-Regular", size: 16)
    var cornerRadius: CGFloat = 10
    var lineColor: UIColor = UIColor(0xD5E0F2)
    var lineWidth: CGFloat = 0.5
    var backgroundColor: UIColor? = UIColor(0xFFFFFF)

    public init(buttonTextColor: UIColor? = UIColor(0x1C66E5), buttonTextFont: UIFont? = UIFont(name: "PingFangSC-Regular", size: 16)) {
        self.buttonTextColor = buttonTextColor
        self.buttonTextFont = buttonTextFont
    }
}

public class AlertPanel: UIView {
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()

    private var isViewReady = false
    private var popupAction: Observable<PopupPanelAction>?

    private var titleText: String
    private var messageText: String
    private var buttonText: String
    private var designConfig: AlertDesignConfig
    private var alertButtonAction: (() -> Void)?

    public init(titleText: String, messageText: String, buttonText: String,
                designConfig: AlertDesignConfig = AlertDesignConfig(), action: (() -> Void)? = nil) {
        self.titleText = titleText
        self.messageText = messageText
        self.buttonText = buttonText
        self.designConfig = designConfig
        alertButtonAction = action
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override public func didMoveToWindow() {
        guard !isViewReady else { return }
        super.didMoveToWindow()
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }

    private lazy var titleLabel: UILabel = {
        let view = UILabel()
        view.text = self.titleText
        view.textColor = designConfig.titleTextColor
        view.font = designConfig.titleTextFont
        view.textAlignment = .center
        return view
    }()

    private lazy var messageLabel: UILabel = {
        let view = UILabel()
        view.text = self.messageText
        view.frame = CGRect(x: 0, y: 0, width: 275.scale375Width(), height: 0)
        view.textColor = designConfig.messageColor
        view.font = designConfig.messageFont
        view.lineBreakMode = .byWordWrapping
        view.numberOfLines = 0
        view.sizeToFit()
        return view
    }()

    private lazy var lineView: UIView = {
        let view = UIView()
        view.backgroundColor = designConfig.lineColor
        return view
    }()

    private lazy var alertButton: UIButton = {
        let view = UIButton()
        view.setTitle(self.buttonText, for: .normal)
        view.setTitleColor(designConfig.buttonTextColor, for: .normal)
        view.titleLabel?.font = designConfig.buttonTextFont
        view.backgroundColor = .clear
        view.addTarget(self, action: #selector(alertButtonClick), for: .touchUpInside)
        return view
    }()
}

// MARK: Layout

extension AlertPanel {
    private func constructViewHierarchy() {
        backgroundColor = designConfig.backgroundColor
        layer.cornerRadius = designConfig.cornerRadius
        layer.masksToBounds = true

        addSubview(titleLabel)
        addSubview(messageLabel)
        addSubview(lineView)
        addSubview(alertButton)
    }

    private func activateConstraints() {
        let panelWithoutMessageHeight = 132.scale375Height()
        let panelHeight = panelWithoutMessageHeight + messageLabel.frame.size.height

        snp.remakeConstraints { make in
            make.centerX.centerY.equalToSuperview()
            make.width.equalTo(323.scale375Width())
            make.height.equalTo(panelHeight)
        }

        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(24.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(25.scale375Height())
        }

        messageLabel.snp.makeConstraints { make in
            make.top.equalTo(titleLabel.snp.bottom).offset(12.scale375Height())
            make.leading.equalToSuperview().offset(24.scale375Width())
            make.trailing.equalToSuperview().offset(-24.scale375Width())
            make.height.equalTo(messageLabel.frame.size.height)
        }

        lineView.snp.makeConstraints { make in
            make.top.equalTo(messageLabel.snp.bottom).offset(17.scale375Height())
            make.leading.trailing.equalToSuperview()
            make.height.equalTo(designConfig.lineWidth)
        }

        alertButton.snp.makeConstraints { make in
            make.top.equalTo(lineView.snp.bottom)
            make.height.equalTo(54.scale375Height())
            make.leading.trailing.equalToSuperview()
        }
    }
}

// MARK: Action

extension AlertPanel {
    @objc func alertButtonClick() {
        popupAction?.value = .close
        alertButtonAction?()
    }
}

extension AlertPanel: PopupPanelSubViewProtocol {
    public func setAction(_ popupAction: Observable<PopupPanelAction>) {
        self.popupAction = popupAction
    }

    public func updateRootViewOrientation(isPortrait: Bool) {
        activateConstraints()
    }

    public func isSupportTouchToExit() -> Bool {
        return false
    }

    public func isSupportAlertPanel() -> Bool {
        return true
    }
}
