//
//  LSCoHostUserCell.swift
//  TUILiveKit
//
//  Created by jack on 2024/8/7.
//

import Foundation
import RTCRoomEngine

class LSCoHostUserCell: UITableViewCell {
    static let identifier = "LSCoHostUserCell"

    private var connectionUser:TUIConnectionUser?
    var inviteEventClosure: ((TUIConnectionUser) -> Void)?
    
    let avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        return imageView
    }()
    
    let userNameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = UIFont.customFont(ofSize: 16)
        label.textColor = .grayColor
        return label
    }()
    
    let inviteButton: UIButton = {
        let button = UIButton()
        button.layer.cornerRadius = 12.scale375()
        button.titleLabel?.font = UIFont.customFont(ofSize: 12)
        button.setTitleColor(.white, for: .normal)
        button.setTitle(.inviteText, for: .normal)
        button.setTitle(.invitingTest, for: .disabled)
        button.backgroundColor = .b1
        return button
    }()
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        backgroundColor = .clear
        contentView.backgroundColor = .clear
        selectionStyle = .none
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func draw(_ rect: CGRect) {
        super.draw(rect)
        avatarImageView.roundedRect(.allCorners, withCornerRatio: 20.scale375())
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }
    
    func constructViewHierarchy() {
        contentView.addSubview(avatarImageView)
        contentView.addSubview(userNameLabel)
        contentView.addSubview(inviteButton)
    }
    
    func activateConstraints() {
        avatarImageView.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.leading.equalToSuperview().offset(24.scale375())
            make.size.equalTo(CGSize(width: 40.scale375(), height: 40.scale375()))
        }
        
        userNameLabel.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.leading.equalTo(avatarImageView.snp.trailing).offset(12.scale375())
            make.width.lessThanOrEqualTo(120.scale375())
        }
        
        inviteButton.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-24.scale375())
            make.centerY.equalToSuperview()
            make.size.equalTo(CGSize(width: 72.scale375(), height: 24.scale375()))
        }
    }
    
    func bindInteraction() {
        inviteButton.addTarget(self, action: #selector(inviteButtonClick(sender:)), for: .touchUpInside)
    }
    
    func updateUser(_ user: TUIConnectionUser) {
        self.connectionUser = user
        avatarImageView.kf.setImage(with: URL(string: user.avatarUrl), placeholder: UIImage.avatarPlaceholderImage)
        userNameLabel.text = user.userName.isEmpty ? user.userId : user.userName
        
        inviteButton.isHidden = user.connectionStatus == .connected
        updateButtonView(isEnabled: user.connectionStatus == .none)
    }
    
    func updateButtonView(isEnabled: Bool) {
        inviteButton.isEnabled = isEnabled
        inviteButton.backgroundColor = isEnabled ? .b1 : .b1.withAlphaComponent(0.5)
    }
}

// MARK: - Action
extension LSCoHostUserCell {
    @objc
    private func inviteButtonClick(sender: UIButton) {
        if let user = connectionUser {
            inviteEventClosure?(user)
        }
    }
    
}

fileprivate extension String {
    static let inviteText = internalLocalized("Invite")
    static let invitingTest = internalLocalized("Waiting")
}
