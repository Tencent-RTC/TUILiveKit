//
//  SingleColumnWidgetView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2025/4/18.
//

import RTCCommon

class SingleColumnWidgetView: RTCBaseView {
    private var liveInfo: LiveInfo
    
    init(liveInfo: LiveInfo) {
        self.liveInfo = liveInfo
        super.init(frame: .zero)
    }
    
    private lazy var roomNameLabel: UILabel = {
        let label = UILabel()
        label.textColor = .textPrimaryColor
        label.font = .customFont(ofSize: 20, weight: .semibold)
        label.textAlignment = .left
        label.numberOfLines = 1
        return label
    }()
    
    private lazy var ownerAvatarView: UIImageView = {
        let view = UIImageView(frame: .zero)
        view.layer.cornerRadius = 10.scale375()
        view.layer.masksToBounds = true
        return view
    }()
    
    private lazy var ownerNameLabel: UILabel = {
        let label = UILabel()
        label.textColor = .textSecondaryColor
        label.font = .customFont(ofSize: 16)
        label.textAlignment = .left
        label.numberOfLines = 1
        return label
    }()
    
    private lazy var joinLiveButton: UIButton = {
        let button = UIButton(type: .custom)
        button.backgroundColor = .white.withAlphaComponent(0.14)
        button.layer.cornerRadius = 20.scale375Height()
        button.layer.masksToBounds = true
        button.layer.borderColor = UIColor.white.withAlphaComponent(0.3).cgColor
        button.layer.borderWidth = 1
        return button
    }()
    
    private lazy var liveStatusView: LiveStatusView = {
        let view = LiveStatusView()
        view.isUserInteractionEnabled = false
        return view
    }()
    
    private lazy var joinLiveButtonLabel: UILabel = {
        let label = UILabel()
        label.text = .joinLiveButtonText
        label.textColor = .white
        label.font = .customFont(ofSize: 16)
        label.textAlignment = .center
        label.isUserInteractionEnabled = false
        return label
    }()
    
    private lazy var joinLiveButtonStack: UIStackView = {
        let stack = UIStackView(arrangedSubviews: [liveStatusView, joinLiveButtonLabel])
        stack.axis = .horizontal
        stack.alignment = .center
        stack.spacing = 8.scale375()
        stack.isUserInteractionEnabled = false
        return stack
    }()
    
    override func constructViewHierarchy() {
        addSubview(roomNameLabel)
        addSubview(ownerAvatarView)
        addSubview(ownerNameLabel)
        addSubview(joinLiveButton)
        joinLiveButton.addSubview(joinLiveButtonStack)
    }
    
    override func activateConstraints() {
        roomNameLabel.snp.makeConstraints { make in
            make.bottom.equalTo(ownerNameLabel.snp.top).offset(-8.scale375Height())
            make.leading.equalTo(ownerAvatarView)
            make.height.equalTo(22.scale375Height())
            make.trailing.lessThanOrEqualToSuperview().inset(20.scale375())
        }

        ownerAvatarView.snp.makeConstraints { make in
            make.leading.equalToSuperview().inset(20.scale375())
            make.centerY.equalTo(ownerNameLabel)
            make.width.height.equalTo(20.scale375())
        }

        ownerNameLabel.snp.makeConstraints { make in
            make.height.equalTo(21.scale375Height())
            make.leading.equalTo(ownerAvatarView.snp.trailing).offset(8.scale375())
            make.bottom.equalToSuperview().inset(43.scale375Height())
            make.trailing.lessThanOrEqualToSuperview().inset(20.scale375())
        }
        
        joinLiveButton.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.bottom.equalToSuperview().inset(234.scale375Height())
            make.width.equalTo(200.scale375())
            make.height.equalTo(40.scale375())
        }
        
        liveStatusView.snp.makeConstraints { make in
            make.width.height.equalTo(16.scale375())
        }
        
        joinLiveButtonStack.snp.makeConstraints { make in
            make.center.equalToSuperview()
        }
    }
    
    override func setupViewStyle() {
        updateView(liveInfo: liveInfo)
    }
    
    func updateView(liveInfo: LiveInfo) {
        self.liveInfo = liveInfo
        roomNameLabel.text = !liveInfo.name.isEmpty ? liveInfo.name : liveInfo.roomId
        ownerNameLabel.text = liveInfo.ownerName.isEmpty ? liveInfo.ownerId : liveInfo.ownerName
        ownerAvatarView.sd_setImage(with: URL(string: liveInfo.ownerAvatarUrl), placeholderImage: .avatarPlaceholderImage)
    }
}

extension String {
    static var joinLiveButtonText = internalLocalized("Click to enter the live room")
}
