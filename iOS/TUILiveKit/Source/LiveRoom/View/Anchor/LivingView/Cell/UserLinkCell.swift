//
//  UserLinkCell.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/27.
//

import Foundation

class UserLinkCell: UserBaseCell {
    
    private lazy var hangUpButton: UIButton = {
        let view = UIButton(type: .system)
        view.showsTouchWhenHighlighted = false
        view.backgroundColor = .clear
        view.setTitleColor(.redColor, for: .normal)
        view.titleLabel?.font = .customFont(ofSize: 12)
        view.setTitle(.anchorHangUpTitle, for: .normal)
        view.mm_w = 64.scale375()
        view.mm_h = 24.scale375()
        view.layer.masksToBounds = true
        view.layer.cornerRadius = view.mm_h*0.5
        view.layer.borderColor = UIColor.redColor.cgColor
        view.layer.borderWidth = 1
        view.addTarget(self, action: #selector(hangUpButtonClick), for: .touchUpInside)
        return view
    }()
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
    }
    
    func constructViewHierarchy() {
        contentView.addSubview(avatarImageView)
        contentView.addSubview(nameLabel)
        contentView.addSubview(hangUpButton)
        contentView.addSubview(lineView)
    }
    
    func activateConstraints() {
        avatarImageView.snp.makeConstraints { (make) in
            make.centerY.equalToSuperview()
            make.leading.equalToSuperview().inset(24)
            make.width.equalTo(40.scale375())
            make.height.equalTo(40.scale375())
        }
        
        nameLabel.snp.makeConstraints { (make) in
            make.centerY.equalToSuperview()
            make.height.equalToSuperview()
            make.leading.equalTo(avatarImageView.snp.trailing).offset(14.scale375())
            make.trailing.equalTo(hangUpButton.snp.leading).offset(-14.scale375())
        }
        
        hangUpButton.snp.makeConstraints { (make) in
            make.centerY.equalToSuperview()
            make.trailing.equalToSuperview().inset(24)
            make.width.equalTo(hangUpButton.mm_w)
            make.height.equalTo(hangUpButton.mm_h)
        }
        
        lineView.snp.makeConstraints { (make) in
            make.bottom.equalToSuperview()
            make.leading.equalTo(nameLabel)
            make.trailing.equalToSuperview().inset(24)
            make.height.equalTo(1)
        }
        
    }
    
}

// MARK: Action

extension UserLinkCell {
    @objc func hangUpButtonClick() {
        guard let userInfo = userInfo else{ return}
        action.value = .hangUp(userInfo: userInfo)
    }
}


private extension String {
    static var anchorHangUpTitle: String {
        localized("live.anchor.link.hang.up.title")
    }
}
