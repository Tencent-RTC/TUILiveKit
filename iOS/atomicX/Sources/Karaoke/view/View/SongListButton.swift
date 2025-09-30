//
//  SongListButton.swift
//  Pods
//
//  Created by ssc on 2025/8/28.
//

import UIKit
import SnapKit

class SongListButton: UIControl {

    private let gradient : CAGradientLayer = {
        let gradient = CAGradientLayer()
        gradient.colors = [
            UIColor("8157FF").cgColor,
            UIColor("00ABD6").cgColor
        ]
        gradient.startPoint = CGPoint(x: 0, y: 0.5)
        gradient.endPoint = CGPoint(x: 1, y: 0.5)
        gradient.cornerRadius = 16
        return gradient
    }()
    
    private let iconView : UIImageView = {
        let iconView = UIImageView()
        iconView.image = UIImage.atomicXBundleImage(named: "ktv_songList_note")
        iconView.contentMode = .scaleAspectFit
        return iconView
    }()

    private let titleLbl : UILabel = {
        let titleLbl = UILabel()
        titleLbl.text = .songText
        titleLbl.font = UIFont(name: "PingFangSC-Medium", size: 14)
        titleLbl.textColor = UIColor.white.withAlphaComponent(0.9)
        titleLbl.numberOfLines = 1
        titleLbl.lineBreakMode = .byClipping
        titleLbl.setContentHuggingPriority(.required, for: .horizontal)
        return titleLbl
    }()

    override init(frame: CGRect) {
        super.init(frame: frame)
        self.isHidden = true
    }
    required init?(coder: NSCoder) {
        super.init(coder: coder)
    }

    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }

    private func constructViewHierarchy() {
        layer.cornerRadius = 16
        clipsToBounds = true
        layer.insertSublayer(gradient, at: 0)
        addSubview(iconView)
        addSubview(titleLbl)
    }

    private func activateConstraints() {
        iconView.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(16)
            make.centerY.equalToSuperview()
            make.size.equalTo(CGSize(width: 20, height: 20))
        }
        titleLbl.snp.makeConstraints { make in
            make.left.equalTo(iconView.snp.right).offset(8)
            make.centerY.equalToSuperview()
            make.right.equalToSuperview().inset(16)
        }
    }

    override func layoutSubviews() {
        super.layoutSubviews()
        gradient.frame = bounds
    }
}

fileprivate extension String {
    static var songText = ("Song").localized
}
