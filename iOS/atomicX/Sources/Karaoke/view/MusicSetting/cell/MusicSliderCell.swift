//
//  SliderCell.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/21.
//

import UIKit
import Combine
import SnapKit

class MusicSliderCell: UITableViewCell {
    public static let identifier = "MusicSliderCell"
    private var item: MusicSliderItem?
    private var cancellableSet: Set<AnyCancellable> = []
    
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
    
    let valueLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textAlignment = .center
        label.font = .customFont(ofSize: 16.0)
        label.textColor = .white
        label.textAlignment = .right
        label.backgroundColor = .clear
        return label
    }()
    
    let configSlider: UISlider = {
        let view = UISlider()
        view.tintColor = UIColor("1C66E5")
        view.setThumbImage(UIImage.atomicXBundleImage(named: "ktv_slider"), for: .normal)
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
            make.leading.equalToSuperview().offset(24.scale375())
            make.centerY.equalToSuperview()
        }
        valueLabel.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.width.lessThanOrEqualTo(50.scale375())
            make.trailing.equalTo(configSlider.snp.leading).offset(-5.scale375())
        }
        configSlider.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-24.scale375())
            make.centerY.equalToSuperview()
            make.height.equalTo(24.scale375())
            make.width.equalTo(110.scale375())
        }
    }
    
    private func bindInteraction() {
        configSlider.addTarget(self, action: #selector(valueChanged(sender:)), for: .valueChanged)
        configSlider.addTarget(self, action: #selector(valueDidChanged(sender:)), for: [.touchUpInside, .touchUpOutside])
    }
    
    private func setupStyle() {
        backgroundColor = .clear
        selectionStyle = .none
    }
    
    public func update(item: MusicSettingItem) {
        guard let sliderItem = item as? MusicSliderItem else { return }
        configSlider.value = sliderItem.currentValue
        configSlider.maximumValue = sliderItem.max
        configSlider.minimumValue = sliderItem.min
        valueLabel.text = String(format: "%.2f", sliderItem.currentValue)
        sliderItem.subscribeState?(self, &cancellableSet)
        self.item = sliderItem
    }
}

extension MusicSliderCell {
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
