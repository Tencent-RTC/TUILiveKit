//
//  VideoSettingSwitchCell.swift
//  TUILiveKit
//
//  Created by jack on 2024/11/25.
//

import Foundation

class VideoSettingSwitchCell: UICollectionViewCell {
    
    typealias SwitchSelectBlock = (Bool) -> ()
    
    static let CellID = "VideoSettingSwitchCell"
    
    private lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 14.0)
        label.textColor = .g7
        label.textAlignment = .left
        return label
    }()
    
    private lazy var switchView: UISwitch = {
        let view = UISwitch()
        view.isOn = false
        view.onTintColor = .b1
        return view
    }()
    
    private lazy var lineView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .g6.withAlphaComponent(0.2)
        return view
    }()
    
    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
    }
    
    private var item: VideoSettingSwitchItem?
    func updateUI(item: VideoSettingItem) {
        guard let itemData = item as? VideoSettingSwitchItem else {
            return
        }
        self.item = itemData
        titleLabel.text = item.title
        switchView.setOn(itemData.isOn, animated: false)
    }
}

// MARK: - UI
extension VideoSettingSwitchCell {
    
    private func constructViewHierarchy() {
        contentView.addSubview(titleLabel)
        contentView.addSubview(switchView)
        contentView.addSubview(lineView)
    }
    
    private func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.leading.equalTo(16)
            make.centerY.equalToSuperview()
        }
        switchView.snp.makeConstraints { make in
            make.trailing.equalTo(-16)
            make.centerY.equalToSuperview()
        }
        lineView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview().inset(16)
            make.bottom.equalToSuperview()
            make.height.equalTo(0.5)
        }
    }
    
    private func bindInteraction() {
        switchView.addTarget(self, action: #selector(switchClick(sender:)), for: .valueChanged)
    }
}

// MARK: - Actions
extension VideoSettingSwitchCell {
    @objc
    private func switchClick(sender: UISwitch) {
        item?.action?(sender.isOn)
    }
}
