//
//  UserRequestLinkCell.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/24.
//

import Foundation
import RTCRoomEngine

class UserRequestLinkCell: LinkMicBaseCell {
    var respondEventClosure: ((TUIUserInfo, Bool, @escaping () -> Void) -> Void)?
    private var isPending = false
    
    private lazy var acceptButton: UIButton = {
        let view = UIButton(type: .system)
        view.showsTouchWhenHighlighted = false
        view.backgroundColor = .b1
        view.setTitleColor(.white, for: .normal)
        view.titleLabel?.font = .customFont(ofSize: 12)
        view.setTitle(.anchorLinkAgreeTitle, for: .normal)
        view.mm_w = 64.scale375()
        view.mm_h = 24.scale375()
        view.layer.masksToBounds = true
        view.layer.cornerRadius = view.mm_h*0.5
        view.addTarget(self, action: #selector(acceptButtonClick), for: .touchUpInside)
        return view
    }()
    
    private lazy var rejectButton: UIButton = {
        let view = UIButton(type: .system)
        
        view.showsTouchWhenHighlighted = false
        view.backgroundColor = .clear
        view.setTitleColor(.b1, for: .normal)
        view.titleLabel?.font = .customFont(ofSize: 12)
        view.setTitle(.anchorLinkRejectTitle, for: .normal)
        view.mm_w = 64.scale375()
        view.mm_h = 24.scale375()
        view.layer.masksToBounds = true
        view.layer.cornerRadius = view.mm_h*0.5
        view.layer.borderColor = UIColor.b1.cgColor
        view.layer.borderWidth = 1
        view.addTarget(self, action: #selector(rejectButtonClick), for: .touchUpInside)
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
        contentView.addSubview(acceptButton)
        contentView.addSubview(rejectButton)
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
            make.trailing.equalTo(acceptButton.snp.leading).offset(-14.scale375())
        }
        
        rejectButton.snp.makeConstraints { (make) in
            make.centerY.equalToSuperview()
            make.trailing.equalToSuperview().inset(24)
            make.width.equalTo(rejectButton.mm_w)
            make.height.equalTo(rejectButton.mm_h)
        }
        
        acceptButton.snp.makeConstraints { (make) in
            make.centerY.equalToSuperview()
            make.trailing.equalTo(rejectButton.snp.leading).offset(-14.scale375())
            make.width.equalTo(acceptButton.mm_w)
            make.height.equalTo(acceptButton.mm_h)
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

extension UserRequestLinkCell {
    @objc func acceptButtonClick() {
        guard let seatApplication = seatApplication, let respondEventClosure = respondEventClosure, !isPending else { return }
        isPending = true
        respondEventClosure(seatApplication, true) { [weak self] in
            guard let self = self else { return }
            isPending = false
        }
    }
    
    @objc func rejectButtonClick() {
        guard let seatApplication = seatApplication, let respondEventClosure = respondEventClosure, !isPending else { return }
        isPending = true
        respondEventClosure(seatApplication, false) { [weak self] in
            guard let self = self else { return }
            isPending = false
        }
    }
}

private extension String {
    static var anchorLinkAgreeTitle: String {
        internalLocalized("Agree")
    }
    
    static var anchorLinkRejectTitle: String {
        internalLocalized("Reject")
    }

}
