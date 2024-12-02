//
//  SwitchCell.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/21.
//

import UIKit

class SwitchCell: UITableViewCell {
    static let identifier = "SwitchCell"
    
    private var item: SwitchItem?
    
    var title: String {
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
    
    let configSwitch: UISwitch = {
        let view = UISwitch()
        view.onTintColor = .b1
        return view
    }()
    
    private var isViewReady = false
    override func didMoveToWindow() {
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
        contentView.addSubview(configSwitch)
    }
    
    private func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(24)
            make.centerY.equalToSuperview()
        }
        configSwitch.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-24)
            make.centerY.equalToSuperview()
            make.height.equalTo(24)
            make.width.equalTo(42)
        }
    }
    
    private func bindInteraction() {
        configSwitch.addTarget(self, action: #selector(switchAction(sender:)), for: .touchUpInside)
    }
    
    func setupStyle() {
        backgroundColor = .clear
        selectionStyle = .none
    }
    
    func update(item: SettingItem) {
        guard let switchItem = item as? SwitchItem else { return }
        configSwitch.isOn = switchItem.isOn
        self.item = switchItem
    }
}

extension SwitchCell {
    @objc
    func switchAction(sender: UISwitch) {
        if let item = self.item {
            item(payload: sender.isOn)
        }
    }
}
