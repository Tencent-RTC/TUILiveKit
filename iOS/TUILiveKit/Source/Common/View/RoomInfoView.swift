//
//  RoomInfoView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2024/5/8.
//

import Foundation

class RoomInfoView: UIControl {
    private let roomNameLabel: UILabel = {
        let view = UILabel()
        view.font = .customFont(ofSize: 14)
        view.textColor = .g7
        view.textAlignment = .left
        return view
    }()

    private lazy var imageView: UIImageView = {
        let view = UIImageView(frame: CGRect(origin: .zero, size: CGSize(width: 24.scale375(), height: 24.scale375())))
        view.layer.cornerRadius = view.frame.width * 0.5
        view.layer.masksToBounds = true
        view.image = UIImage.placeholderImage
        return view
    }()

    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }

    private func constructViewHierarchy() {
        addSubview(roomNameLabel)
        addSubview(imageView)
    }

    private func activateConstraints() {
        imageView.snp.makeConstraints { make in
            make.size.equalTo(imageView.frame.size)
            make.leading.equalToSuperview().inset(4.scale375())
            make.centerY.equalToSuperview()
        }

        roomNameLabel.snp.makeConstraints { make in
            make.leading.equalTo(imageView.snp.trailing).offset(8.scale375())
            make.trailing.equalToSuperview().inset(8.scale375())
            make.centerY.equalToSuperview()
            make.height.equalToSuperview()
        }
    }

    var roomName: String = "" {
        didSet {
            roomNameLabel.text = roomName
        }
    }

    var roomCoverUrl: URL? {
        didSet {
            guard let roomCoverUrl = roomCoverUrl else { return }
            imageView.kf.setImage(with: roomCoverUrl, placeholder: UIImage.placeholderImage)
        }
    }
}
