//
//  LiveListCell.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/5.
//

import UIKit
import Kingfisher
import RTCRoomEngine

class LiveListCell: UICollectionViewCell {
    
    static let reuseIdentify = "LiveListCell"
    
    lazy var roomCoverIcon: UIImageView = {
        let view = UIImageView(frame: .zero)
        return view
    }()
    
    lazy var watchingLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .white
        label.font = UIFont.customFont(ofSize: 12)
        return label
    }()
    
    lazy var watchingIcon: UIImageView = {
        let icon = UIImageView(frame: .zero)
        icon.image = .liveBundleImage("watching")
        return icon
    }()
    
    lazy var roomNameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = UIColor(hex: "22262E")
        label.textAlignment = .left
        label.font = UIFont.customFont(ofSize: 14)
        return label
    }()
    
    lazy var roomOwnerIcon: UIImageView = {
        let view = UIImageView(frame: .zero)
        return view
    }()
    
    lazy var roomOwnerNameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = UIColor(hex: "4E5461")
        label.textAlignment = .left
        label.font = UIFont.customFont(ofSize: 12)
        return label
    }()
    
    override init(frame: CGRect) {
        super.init(frame: frame)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
        
        backgroundColor = .white
    }
    
    override func draw(_ rect: CGRect) {
        super.draw(rect)
        roundedRect(.allCorners, withCornerRatio: 10)
        roomOwnerIcon.roundedRect(.allCorners, withCornerRatio: 15)
    }
}

extension LiveListCell {
    private func constructViewHierarchy() {
        contentView.addSubview(roomCoverIcon)
        contentView.addSubview(watchingIcon)
        contentView.addSubview(watchingLabel)
        contentView.addSubview(roomNameLabel)
        contentView.addSubview(roomOwnerIcon)
        contentView.addSubview(roomOwnerNameLabel)
    }
    
    private func activateConstraints() {
        roomCoverIcon.snp.makeConstraints { make in
            make.left.top.right.equalToSuperview()
            make.height.equalTo(contentView.snp.width).multipliedBy(1.2)
        }
        
        watchingIcon.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(8)
            make.centerY.equalTo(watchingLabel.snp.centerY)
            make.size.equalTo(CGSize(width: 10, height: 10))
        }
        
        watchingLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(8)
            make.left.equalTo(watchingIcon.snp.right).offset(4)
            make.right.equalToSuperview().offset(-8)
        }
        
        roomNameLabel.snp.makeConstraints { make in
            make.top.equalTo(roomCoverIcon.snp.bottom).offset(5)
            make.left.equalToSuperview().offset(8)
            make.right.equalToSuperview().offset(-8)
            make.height.equalTo(25)
        }
        
        roomOwnerIcon.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(8)
            make.top.equalTo(roomNameLabel.snp.bottom).offset(5)
            make.size.equalTo(CGSizeMake(30, 30))
        }
        
        roomOwnerNameLabel.snp.makeConstraints { make in
            make.centerY.equalTo(roomOwnerIcon.snp.centerY)
            make.left.equalTo(roomOwnerIcon.snp.right).offset(4)
            make.right.equalToSuperview().offset(-8)
        }
    }
}

extension LiveListCell {
    func updateView(liveInfo: LiveInfo) {
        let placeholderImage = UIImage.liveBundleImage("live_edit_info_default_cover_image")
        roomCoverIcon.sd_setImage(with: URL(string: liveInfo.coverUrl), placeholderImage: placeholderImage)
        watchingLabel.text = String.localizedReplace(.watching, replace: "\(liveInfo.viewCount)")
        roomNameLabel.text = liveInfo.name.count > 0 ?  liveInfo.name : liveInfo.roomId
        roomOwnerIcon.sd_setImage(with: URL(string: liveInfo.ownerAvatarUrl), placeholderImage: placeholderImage)
        roomOwnerNameLabel.text = liveInfo.ownerName
    }
}

extension String {
    static let watching = localized("xxx people viewed")
}
