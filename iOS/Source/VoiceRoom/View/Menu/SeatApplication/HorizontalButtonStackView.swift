//
//  HorizontalButtonStackView.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/22.
//

import UIKit

struct HorizontalButtonStackConfig {
    typealias OperateAction = (UIButton) -> Void
    
    let title: String
    let color: UIColor
    let style: HorizontalButtonStackView.ButtonStyle
    let action: OperateAction?
}

class HorizontalButtonStackView: UIView {
    
    static let buttonWidth: CGFloat = 64.0
    static let buttonHeight: CGFloat = 24.0
    static let spacing: CGFloat = 10.0
    
    enum ButtonStyle {
        case fill
        case hollow
    }
    let operateConfigs: [HorizontalButtonStackConfig]
    private var buttons: [UIButton] = []
    init(operateConfigs: [HorizontalButtonStackConfig]) {
        self.operateConfigs = operateConfigs
        super.init(frame: .zero)
        self.buttons = operateConfigs.enumerated().map { createButtonFrom(operate: $0.element, index: $0.offset) }
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
    
    let stackView: UIStackView = {
        let view = UIStackView(frame: .zero)
        view.spacing = spacing
        view.axis = .horizontal
        view.distribution = .fill
        return view
    }()
    
    private func constructViewHierarchy() {
        addSubview(stackView)
        buttons.forEach { button in
            stackView.addArrangedSubview(button)
        }
    }
    
    private func activateConstraints() {
        stackView.snp.makeConstraints { make in
            make.top.leading.bottom.trailing.equalToSuperview()
        }
        buttons.forEach { button in
            button.snp.makeConstraints { make in
                make.height.equalTo(HorizontalButtonStackView.buttonHeight)
                make.width.equalTo(HorizontalButtonStackView.buttonWidth)
            }
        }
    }
    
    private func bindInteraction() {
        buttons.forEach { button in
            button.addTarget(self, action: #selector(clickButton(sender:)), for: .touchUpInside)
        }
    }
    
    private func createButtonFrom(operate: HorizontalButtonStackConfig, index: Int) -> UIButton {
        let button = UIButton(type: .custom)
        button.setTitle(operate.title, for: .normal)
        switch operate.style {
            case .fill:
                button.backgroundColor = operate.color
                button.setTitleColor(.white, for: .normal)
            case .hollow:
                button.layer.borderColor = operate.color.cgColor
                button.layer.borderWidth = 1
                button.backgroundColor = .clear
                button.setTitleColor(operate.color, for: .normal)
        }
        button.titleLabel?.font = .customFont(ofSize: 12)
        button.layer.cornerRadius = HorizontalButtonStackView.buttonHeight / 2.0
        button.tag = index
        return button
    }
    
    @objc
    func clickButton(sender: UIButton) {
        let index = sender.tag
        let operate = self.operateConfigs[index]
        operate.action?(sender)
    }
}
