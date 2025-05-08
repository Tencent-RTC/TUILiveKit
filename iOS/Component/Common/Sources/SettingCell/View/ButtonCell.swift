//
//  ButtonCell.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/26.
//

import UIKit
import SnapKit

public class ButtonCell: UITableViewCell {
    public static let identifier = "ButtonCell"
    
    private var item: ButtonItem?
    
    public var title: String {
        set {
            titleLabel.text = newValue
        }
        get {
            return titleLabel.text ?? ""
        }
    }
    
    let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textAlignment = .center
        label.font = .customFont(ofSize: 16.0, weight: .medium)
        label.textColor = .g7
        return label
    }()
    
    let configButton: UIButton = {
        let button = UIButton(type: .custom)
        button.layer.cornerRadius = 10.0
        button.layer.masksToBounds = true
        button.titleLabel?.font = .customFont(ofSize: 16)
        button.setBackgroundImage(UIColor.lightGreenColor.trans2Image(), for: .normal)
        return button
    }()
    
    private var isViewReady = false
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        setupStyle()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        contentView.addSubview(titleLabel)
        contentView.addSubview(configButton)
    }
    
    private func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(24)
            make.centerY.equalToSuperview()
        }
        configButton.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-24)
            make.centerY.equalToSuperview()
            make.height.equalTo(24)
            make.width.equalTo(42)
        }
    }
    
    private func bindInteraction() {
        configButton.addTarget(self, action: #selector(buttonAction(sender:)), for: .touchUpInside)
    }
    
    func setupStyle() {
        backgroundColor = .clear
    }
    
    
    public func update(item: SettingItem) {
        guard let buttonItem = item as? ButtonItem else { return }
        configButton.setTitle(buttonItem.buttonTitle, for: .normal)
        configButton.setImage(buttonItem.icon, for: .normal)
        self.item = buttonItem
    }
}

extension ButtonCell {
    @objc
    func buttonAction(sender: UIButton) {
        if let item = self.item {
            item(payload: ())
        }
    }
}
