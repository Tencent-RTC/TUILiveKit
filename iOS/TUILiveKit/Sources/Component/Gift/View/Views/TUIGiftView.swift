//
//  TUIGiftView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import UIKit
import Kingfisher
import RTCRoomEngine


typealias TUIActionSendBlock = (TUIGiftInfo) -> Void

class TUIGiftView: UIView {
    var sendBlock: TUIActionSendBlock?
    var giftInfo: TUIGiftInfo = TUIGiftInfo() {
        didSet {
            setGiftInfo(giftInfo: giftInfo)
        }
    }
    var isSelected: Bool = false {
        didSet {
           setSelectedState(isSelected)
        }
    }
    
    private lazy var selectedView: UIView = {
        let view = UIView(frame: CGRect(x: 0, y: 0, width: self.mm_w, height: imageBgView.mm_h + sendButton.mm_h + 4))
        view.layer.masksToBounds = true
        view.layer.cornerRadius = 10
        view.layer.borderColor = UIColor.buttonPrimaryDefaultColor.cgColor
        return view
    }()

    private lazy var imageBgView: UIView = {
        let view = UIView(frame: CGRect(x: 2, y: 2, width: self.mm_w - 4, height: self.mm_w - 4))
        view.backgroundColor = .clear
        view.layer.masksToBounds = true
        view.layer.cornerRadius = 8
        return view
    }()

    private let normalImageView: UIImageView = {
        let view = UIImageView(frame: CGRect(x: 10, y: 10, width: 54, height: 54))
        view.backgroundColor = .clear
        return view
    }()

    private let selectedImageView: UIImageView = {
        let view = UIImageView(frame: CGRect(x: 10, y: 10, width: 54, height: 54))
        view.backgroundColor = .clear
        view.isHidden = true
        return view
    }()

    private lazy var giftNameLabel: UILabel = {
        let label = UILabel(frame: CGRect(x: 0, y: self.imageBgView.mm_maxY + 2, width: self.mm_w, height: 20))
        label.font = .customFont(ofSize: 12)
        label.textAlignment = .center
        label.lineBreakMode = .byTruncatingMiddle
        label.textColor = .textPrimaryColor
        return label
    }()

    private lazy var pointLabel: UILabel = {
        let label = UILabel(frame: CGRect(x: 0, y: self.giftNameLabel.mm_maxY + 2, width: self.mm_w, height: 14))
        label.font = .customFont(ofSize: 10)
        label.textAlignment = .center
        label.lineBreakMode = .byTruncatingMiddle
        label.textColor = .textSecondaryColor
        return label
    }()

    private lazy var sendButton: UIButton = {
        let button = UIButton(frame: CGRect(x: 0, y: self.imageBgView.mm_maxY + 2, width: self.mm_w, height: 20))
        button.setTitle(.sendOut, for: .normal)
        button.setTitleColor(.textPrimaryColor, for: .normal)
        button.titleLabel?.font = .customFont(ofSize: 12, weight: .medium)
        button.addTarget(self, action: #selector(sendButtonClick), for: .touchUpInside)
        button.adjustsImageWhenHighlighted = false
        button.layer.masksToBounds = true
        button.layer.cornerRadius = button.mm_h * 0.5
        button.isHidden = false
        button.backgroundColor = .buttonPrimaryDefaultColor
        button.isHidden = true
        return button
    }()

    override init(frame: CGRect) {
        super.init(frame: frame)
        mm_h = 53 + 74
        mm_w = 74
        setupUI()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private func setGiftInfo(giftInfo: TUIGiftInfo) {
        normalImageView.kf.setImage(with: URL(string: giftInfo.iconUrl))
        selectedImageView.kf.setImage(with: URL(string: giftInfo.iconUrl))
        giftNameLabel.text = giftInfo.name
        pointLabel.text = "\(giftInfo.coins)"
    }
    
    private func setSelectedState(_ isSelected: Bool) {
        selectedView.backgroundColor = isSelected ? .buttonPrimaryDefaultColor : .clear
        selectedView.layer.borderWidth = isSelected ? 2 : 0
        imageBgView.backgroundColor = isSelected ? .bgEntrycardColor : .bgOperateColor
        normalImageView.isHidden = isSelected
        giftNameLabel.isHidden = isSelected
        selectedImageView.isHidden = !isSelected
        sendButton.isHidden = !isSelected
    }
}

// MARK: Layout

extension TUIGiftView {
    func setupUI() {
        clipsToBounds = true
        addSubview(selectedView)
        addSubview(imageBgView)
        addSubview(normalImageView)
        addSubview(selectedImageView)
        addSubview(giftNameLabel)
        addSubview(pointLabel)
        addSubview(sendButton)
    }
}

// MARK: Action

extension TUIGiftView {
    @objc func sendButtonClick() {
        sendBlock?(giftInfo)
    }
}

// MARK: localized String

private extension String {
    static var sendOut = internalLocalized("Send Out")
}
