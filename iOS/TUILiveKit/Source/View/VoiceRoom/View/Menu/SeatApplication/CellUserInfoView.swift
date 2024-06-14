//
//  CellUserInfoView.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/22.
//

import UIKit

class CellUserInfoView: UIView {
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
    }
    
    let avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 40 * 0.5
        imageView.layer.masksToBounds = true
        return imageView
    }()
    
    let nameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 16)
        label.textColor = .white
        return label
    }()
    
    private func constructViewHierarchy() {
        addSubview(avatarImageView)
        addSubview(nameLabel)
    }
    
    private func activateConstraints() {
        avatarImageView.snp.makeConstraints { make in
            make.width.height.equalTo(40)
            make.centerY.equalToSuperview()
            make.leading.equalToSuperview()
        }
        nameLabel.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.leading.equalTo(avatarImageView.snp.trailing).offset(12)
            make.trailing.equalToSuperview()
        }
    }
}
