//
//  SwitchCell.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/21.
//

import UIKit
import SnapKit

class MusicSwitchCell: UITableViewCell {
    public static let identifier = "MusicSwitchCell"

    private var item: MusicSwitchItem?

    public var title: String {
        set {
            titleLabel.text = newValue
        }
        get {
            return titleLabel.text ?? ""
        }
    }
    
    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textAlignment = .center
        label.font = .customFont(ofSize: 16.0, weight: .medium)
        label.textColor = .g7
        return label
    }()
    
    private let configSwitch: UISwitch = {
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
            make.leading.equalToSuperview().offset(24.scale375())
            make.centerY.equalToSuperview()
        }
        configSwitch.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-24.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(24.scale375())
            make.width.equalTo(42.scale375())
        }
    }
    
    private func bindInteraction() {
        configSwitch.addTarget(self, action: #selector(switchAction(sender:)), for: .touchUpInside)
    }
    
    func setupStyle() {
        backgroundColor = .clear
        selectionStyle = .none
    }
    
    public func update(item: MusicSettingItem) {
        guard let switchItem = item as? MusicSwitchItem else { return }
        configSwitch.isOn = switchItem.isOn
        self.item = switchItem
    }
}

extension MusicSwitchCell {
    @objc
    func switchAction(sender: UISwitch) {
        if let item = self.item {
            item(payload: sender.isOn)
        }
    }
}
