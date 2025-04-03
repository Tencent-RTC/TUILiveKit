//
//  AudienceEndView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/12/12.
//

import UIKit
import RTCCommon

class AudienceEndView: UIView {
    private let roomId: String
    private let avatarUrl: String
    private let userName: String
    
    private let titleLabel: UILabel = {
        let label = UILabel()
        label.textAlignment = .center
        label.font = .customFont(ofSize: 20)
        label.textColor = .flowKitWhite
        label.text = .titleText
        return label
    }()
    
    private lazy var closeButton: UIButton = {
        let button = UIButton()
        button.setImage(.liveBundleImage("live_leave_icon"), for: .normal)
        button.addTarget(self, action: #selector(closeButtonClick), for: .touchUpInside)
        return button
    }()
    
    private lazy var imageView: UIImageView = {
        let view = UIImageView()
        view.layer.cornerRadius = 40
        view.layer.masksToBounds = true
        view.kf.setImage(with: URL(string: avatarUrl), placeholder: UIImage.avatarPlaceholderImage)
        return view
    }()
    
    private lazy var nameLabel: UILabel = {
        let label = UILabel()
        label.textAlignment = .center
        label.font = .customFont(ofSize: 14)
        label.textColor = .flowKitWhite
        label.text = userName
        return label
    }()
    
    weak var delegate: LiveEndViewDelegate?
    
    init(roomId: String, avatarUrl: String, userName: String) {
        self.roomId = roomId
        self.avatarUrl = avatarUrl
        self.userName = userName
        super.init(frame: .zero)
        self.backgroundColor = .g2
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(closeButton)
        addSubview(imageView)
        addSubview(nameLabel)
    }
    
    func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(120.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(30.scale375Height())
        }
        
        closeButton.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(60.scale375Height())
            make.trailing.equalToSuperview().offset(-30.scale375())
            make.width.height.equalTo(30.scale375())
        }
        
        imageView.snp.makeConstraints { make in
            make.top.equalTo(titleLabel.snp.bottom).offset(50.scale375Height())
            make.centerX.equalToSuperview()
            make.width.height.equalTo(80.scale375())
        }
        
        nameLabel.snp.makeConstraints { make in
            make.top.equalTo(imageView.snp.bottom).offset(5.scale375())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(25.scale375Height())
        }
    }
    
    @objc func closeButtonClick() {
        delegate?.onCloseButtonClick()
    }
    
    func update(avatarUrl: String) {
        imageView.kf.setImage(with: URL(string: avatarUrl))
    }
    
    func update(userName: String) {
        nameLabel.text = userName
    }
}

private extension String {
    static var titleText: String {
        localized("Live broadcast has ended")
    }
}
