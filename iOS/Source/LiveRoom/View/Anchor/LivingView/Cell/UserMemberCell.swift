//
//  File.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/24.
//

import Foundation

class UserMemberCell: UserBaseCell {
    
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
            make.trailing.trailing.equalToSuperview().inset(24)
        }
       
        lineView.snp.makeConstraints { (make) in
            make.bottom.equalToSuperview()
            make.leading.equalTo(nameLabel)
            make.trailing.equalToSuperview().inset(24)
            make.height.equalTo(1)
        }
        
    }
    
}
