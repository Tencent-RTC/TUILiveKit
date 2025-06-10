//
//  DoubleColumnWidgetView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2025/4/16.
//

import RTCCommon

class DoubleColumnWidgetView: RTCBaseView {
    private var liveInfo: LiveInfo
    
    init(liveInfo: LiveInfo) {
        self.liveInfo = liveInfo
        super.init(frame: .zero)
    }
    
    private lazy var watchingLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .textPrimaryColor
        label.font = .customFont(ofSize: 14, weight: .semibold)
        return label
    }()
    
    private lazy var watchingIcon: UIImageView = {
        let icon = UIImageView(frame: .zero)
        icon.image = internalImage("watching")
        return icon
    }()
    
    private lazy var roomNameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .textPrimaryColor
        label.textAlignment = .left
        label.font = .customFont(ofSize: 16, weight: .semibold)
        return label
    }()
    
    private lazy var ownerAvatarView: UIImageView = {
        let view = UIImageView(frame: .zero)
        return view
    }()
    
    private lazy var roomOwnerNameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .textSecondaryColor
        label.textAlignment = .left
        label.font = UIFont.customFont(ofSize: 12)
        return label
    }()
    
    override func constructViewHierarchy() {
        addSubview(watchingIcon)
        addSubview(watchingLabel)
        addSubview(roomNameLabel)
        addSubview(ownerAvatarView)
        addSubview(roomOwnerNameLabel)
    }
    
    override func activateConstraints() {
        watchingIcon.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(8.scale375())
            make.centerY.equalTo(watchingLabel.snp.centerY)
            make.width.height.equalTo(8.scale375())
        }
        
        watchingLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(6.scale375Height())
            make.leading.equalTo(watchingIcon.snp.trailing).offset(5.scale375())
            make.trailing.equalToSuperview().offset(-8.scale375())
        }
        
        roomNameLabel.snp.makeConstraints { make in
            make.bottom.equalTo(roomOwnerNameLabel.snp.top).offset(-4.scale375Height())
            make.leading.equalTo(ownerAvatarView)
            make.trailing.lessThanOrEqualToSuperview().inset(8.scale375())
            make.height.equalTo(22.scale375Height())
        }
        
        ownerAvatarView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(8.scale375())
            make.centerY.equalTo(roomOwnerNameLabel)
            make.height.width.equalTo(16.scale375())
        }
        
        roomOwnerNameLabel.snp.makeConstraints { make in
            make.bottom.equalToSuperview().inset(6.scale375Height())
            make.leading.equalTo(ownerAvatarView.snp.trailing).offset(4.scale375())
            make.height.equalTo(20.scale375Height())
            make.trailing.lessThanOrEqualToSuperview().inset(8.scale375())
        }
    }
    
    override func setupViewStyle() {
        updateView(liveInfo: liveInfo)
        ownerAvatarView.layer.cornerRadius = 8.scale375()
        ownerAvatarView.layer.masksToBounds = true
    }
    
    func updateView(liveInfo: LiveInfo) {
        self.liveInfo = liveInfo
        watchingLabel.text = String.localizedReplace(.watching, replace: "\(liveInfo.viewCount)")
        roomNameLabel.text = liveInfo.name.isEmpty ? liveInfo.roomId : liveInfo.name
        ownerAvatarView.sd_setImage(with: URL(string: liveInfo.ownerAvatarUrl), placeholderImage: .avatarPlaceholderImage)
        roomOwnerNameLabel.text = liveInfo.ownerName.isEmpty ? liveInfo.ownerId : liveInfo.ownerName
    }
}

extension String {
    static let watching = internalLocalized("xxx people viewed")
}
