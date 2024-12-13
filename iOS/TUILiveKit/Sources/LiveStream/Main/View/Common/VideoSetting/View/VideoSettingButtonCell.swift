//
//  VideoSettingDefaultCell.swift
//  TUILiveKit
//
//  Created by jack on 2024/11/25.
//

import Foundation
import RTCRoomEngine

class VideoSettingButtonCell: UICollectionViewCell {
    
    static let CellID = "VideoSettingButtonCell"

    private lazy var titleLabel: UILabel = {
        let label = UILabel()
        label.font = .customFont(ofSize: 14.0)
        label.textColor = .g7
        label.textAlignment = .left
        return label
    }()

    private lazy var valueContentView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .clear
        return view
    }()
    
    private lazy var valueLabel: UILabel = {
        let view = UILabel(frame: .zero)
        view.font = .customFont(ofSize: 14)
        view.textColor = .g7
        view.sizeToFit()
        view.isUserInteractionEnabled = false
        return view
    }()

    private lazy var arrowImageView: UIImageView = {
        let view = UIImageView()
        view.image = .liveBundleImage("live_selection_arrow_icon")
        view.isUserInteractionEnabled = false
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
    
    private var item: VideoSettingButtonItem?
    
    func updateUI(item: VideoSettingItem) {
        guard let itemData = item as? VideoSettingButtonItem else {
            return
        }
        titleLabel.text = item.title
        valueLabel.text = itemData.value
        self.item = itemData
    }
}

// MARK: - UI
extension VideoSettingButtonCell {
    
    private func constructViewHierarchy() {
        contentView.addSubview(titleLabel)
        contentView.addSubview(valueContentView)
        valueContentView.addSubview(valueLabel)
        valueContentView.addSubview(arrowImageView)
        contentView.addSubview(lineView)
    }
    
    private func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.leading.equalTo(16)
            make.centerY.equalToSuperview()
        }
        valueContentView.snp.makeConstraints { make in
            make.trailing.equalTo(-12)
            make.top.bottom.equalToSuperview()
        }
        valueLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(4)
            make.centerY.equalToSuperview()
        }
        arrowImageView.snp.makeConstraints { make in
            make.leading.equalTo(valueLabel.snp.trailing).offset(4)
            make.trailing.equalToSuperview().offset(-4)
            make.centerY.equalToSuperview()
        }
        lineView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview().inset(16)
            make.bottom.equalToSuperview()
            make.height.equalTo(0.5)
        }
    }
    
    private func bindInteraction() {
        valueContentView.isUserInteractionEnabled = true
        valueContentView.addTapGesture(target: self, action: #selector(valueClick))
    }
}

// MARK: - Actions
extension VideoSettingButtonCell {
    
    @objc
    private func valueClick() {
        item?.action?()
    }
}
