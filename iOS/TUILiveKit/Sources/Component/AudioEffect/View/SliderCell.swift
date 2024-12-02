//
//  SliderCell.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/21.
//

import UIKit
import Combine

class SliderCell: UITableViewCell {
    static let identifier = "SliderCell"
    private var item: SliderItem?
    private var cancellableSet: Set<AnyCancellable> = []
    
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
    
    let valueLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textAlignment = .center
        label.font = .customFont(ofSize: 16.0)
        label.textColor = .whiteColor
        label.textAlignment = .right
        label.backgroundColor = .clear
        return label
    }()
    
    let configSlider: UISlider = {
        let view = UISlider()
        view.tintColor = .b1
        view.setThumbImage(.liveBundleImage("live_slider_icon"), for: .normal)
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
    
    override func prepareForReuse() {
        super.prepareForReuse()
        self.item = nil
        cancellableSet.removeAll()
    }
    
    private func constructViewHierarchy() {
        contentView.addSubview(titleLabel)
        contentView.addSubview(valueLabel)
        contentView.addSubview(configSlider)
    }
    
    private func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(24)
            make.centerY.equalToSuperview()
        }
        valueLabel.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.width.lessThanOrEqualTo(50)
            make.trailing.equalTo(configSlider.snp.leading).offset(-5)
        }
        configSlider.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-24)
            make.centerY.equalToSuperview()
            make.height.equalTo(24)
            make.width.equalTo(110)
        }
    }
    
    private func bindInteraction() {
        configSlider.addTarget(self, action: #selector(valueChanged(sender:)), for: .valueChanged)
        configSlider.addTarget(self, action: #selector(valueDidChanged(sender:)), for: [.touchUpInside, .touchUpOutside])
    }
    
    func setupStyle() {
        backgroundColor = .clear
        selectionStyle = .none
    }
    
    func update(item: SettingItem) {
        guard let sliderItem = item as? SliderItem else { return }
        configSlider.value = sliderItem.currentValue
        configSlider.maximumValue = sliderItem.max
        configSlider.minimumValue = sliderItem.min
        valueLabel.text = String(format: "%.2f", sliderItem.currentValue)
        sliderItem.subscribeState?(self, &cancellableSet)
        self.item = sliderItem
    }
}

extension SliderCell {
    @objc
    func valueChanged(sender: UISlider) {
        valueLabel.text = String(format: "%.f", sender.value)
        if let item = self.item {
            item.valueChanged?(sender.value)
        }
    }
    
    @objc
    func valueDidChanged(sender: UISlider) {
        if let item = self.item {
            item(payload: sender.value)
        }
    }
}
