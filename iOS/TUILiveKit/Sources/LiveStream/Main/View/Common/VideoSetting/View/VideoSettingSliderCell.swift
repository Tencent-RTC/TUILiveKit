//
//  VideoSettingTableCell.swift
//  TUILiveKit
//
//  Created by jack on 2024/11/25.
//

import Foundation
import RTCRoomEngine

class VideoSettingSliderCell: UICollectionViewCell {
    
    static let CellID = "VideoSettingSliderCell"
    typealias SliderSelectBlock = (Int) -> ()
    
    private lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 14.0)
        label.textColor = .g7
        return label
    }()
    
    private lazy var valueLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 14.0)
        label.textColor = .g7
        return label
    }()
    
    private lazy var sliderView: UISlider = {
        let slider = UISlider(frame: .zero)
        slider.tintColor = .b1
        slider.setThumbImage(.liveBundleImage("live_slider_icon"), for: .normal)
        slider.minimumValue = 5
        slider.maximumValue = 60
        return slider
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
    
    private var item: VideoSettingSliderItem?
    func updateUI(item: VideoSettingItem) {
        guard let itemData = item as? VideoSettingSliderItem else {
            return
        }
        self.item = itemData
        titleLabel.text = item.title
        if itemData.sliderUnitText.isEmpty {
            valueLabel.text = "\(itemData.value)"
        } else {
            valueLabel.text = "\(itemData.value) \(itemData.sliderUnitText)"
        }
        sliderView.minimumValue = Float(itemData.minValue/itemData.sliderUnit)
        sliderView.maximumValue = Float(itemData.maxValue/itemData.sliderUnit)
        sliderView.setValue(Float(itemData.value/itemData.sliderUnit), animated: false)
    }
}

// MARK: - UI
extension VideoSettingSliderCell {
    
    private func constructViewHierarchy() {
        contentView.addSubview(titleLabel)
        contentView.addSubview(valueLabel)
        contentView.addSubview(sliderView)
        contentView.addSubview(lineView)
    }
    
    private func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.leading.equalTo(16)
            make.top.equalTo(10)
        }
        valueLabel.snp.makeConstraints { make in
            make.trailing.equalTo(-16)
            make.centerY.equalTo(titleLabel)
        }
        sliderView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview().inset(16)
            make.top.equalTo(titleLabel.snp.bottom).offset(10)
        }
        lineView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview().inset(16)
            make.bottom.equalToSuperview()
            make.height.equalTo(0.5)
        }
    }
    
    private func bindInteraction() {
        sliderView.addTarget(self, action: #selector(sliderTouchEnd(sender:)), for: .touchUpInside)
        sliderView.addTarget(self, action: #selector(sliderValueChanged(sender:)), for: .valueChanged)
    }
}

// MARK: - Actions
extension VideoSettingSliderCell {
    
    @objc
    private func sliderValueChanged(sender: UISlider) {
        guard let data = item else { return }
        let value = Int(sender.value) * data.sliderUnit
        if data.sliderUnitText.isEmpty {
            valueLabel.text = "\(value)"
        } else {
            valueLabel.text = "\(value) \(data.sliderUnitText)"
        }
    }
    
    @objc
    private func sliderTouchEnd(sender: UISlider) {
        guard let data = item else { return }
        let value = Int(sender.value) * data.sliderUnit
        data.valueDidChanged?(value)
    }
    
}
