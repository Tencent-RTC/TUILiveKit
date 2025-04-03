//
//  TUIGiftView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import Kingfisher
import UIKit

typealias TUIActionSendBlock = (TUIGift) -> Void

class TUIGiftView: UIView {
    var sendBlock: TUIActionSendBlock?
    var giftModel: TUIGift = TUIGift() {
        didSet {
            setGiftModel(giftModel)
        }
    }
    var isSelected: Bool = false {
        didSet {
           setSelectedState(isSelected)
        }
    }

    private lazy var imageBgView: UIView = {
        let view = UIView(frame: CGRect(x: 0, y: 0, width: self.mm_w, height: self.mm_w))
        view.layer.masksToBounds = true
        view.layer.cornerRadius = 10
        view.layer.borderColor = UIColor.darkBlueColor.cgColor
        view.backgroundColor = .darkGrayColor
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
        let label = UILabel(frame: CGRect(x: 0, y: self.imageBgView.mm_maxY + 8, width: self.mm_w, height: 20))
        label.font = UIFont.systemFont(ofSize: 14)
        label.textAlignment = .center
        label.lineBreakMode = .byTruncatingMiddle
        label.textColor = .lightGrayColor
        return label
    }()

    private lazy var pointLabel: UILabel = {
        let label = UILabel(frame: CGRect(x: 0, y: self.giftNameLabel.mm_maxY + 2, width: self.mm_w, height: 14))
        label.font = UIFont.systemFont(ofSize: 10)
        label.textAlignment = .center
        label.lineBreakMode = .byTruncatingMiddle
        label.textColor = .darkGrayColor
        return label
    }()

    private lazy var sendButton: UIButton = {
        let button = UIButton(frame: CGRect(x: 0, y: self.imageBgView.mm_maxY + 11, width: self.mm_w, height: 30))
        button.setTitle(.sendOut, for: .normal)
        button.setTitleColor(.white, for: .normal)
        button.titleLabel?.font = UIFont.boldSystemFont(ofSize: 14)
        button.addTarget(self, action: #selector(sendButtonClick), for: .touchUpInside)
        button.adjustsImageWhenHighlighted = false
        button.layer.masksToBounds = true
        button.layer.cornerRadius = button.mm_h * 0.5
        button.isHidden = false
        button.backgroundColor = .b1
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
    
    private func setGiftModel(_ giftModel: TUIGift) {
        normalImageView.kf.setImage(with: URL(string: giftModel.imageUrl))
        selectedImageView.kf.setImage(with: URL(string: giftModel.imageUrl))
        giftNameLabel.text = giftModel.giftName
        pointLabel.text = "\(giftModel.price)"
    }
    
    private func setSelectedState(_ isSelected: Bool) {
        imageBgView.layer.borderWidth = isSelected ? 2 : 0
        normalImageView.isHidden = isSelected
        giftNameLabel.isHidden = isSelected
        pointLabel.isHidden = isSelected
        selectedImageView.isHidden = !isSelected
        sendButton.isHidden = !isSelected
    }
}

// MARK: Layout

extension TUIGiftView {
    func setupUI() {
        clipsToBounds = true
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
        sendBlock?(giftModel)
    }
}

// MARK: localized String

private extension String {
    static var sendOut = {
        localized("Send Out")
    }()
}
