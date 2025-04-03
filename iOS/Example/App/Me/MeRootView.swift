//
//  MeRootView.swift
//  TUILiveKitApp
//
//  Created by krabyu on 2024/6/19.
//

import UIKit
import RTCCommon

class MeRootView: UIView {
    weak var rootVC: MeViewController?
    
    let imageView: UIImageView = {
        let imageView = UIImageView()
        imageView.layer.cornerRadius = 50.scale375()
        imageView.layer.masksToBounds = true
        return imageView
    }()
    
    lazy var nameLabel: UILabel = {
        let label = UILabel()
        label.textColor = UIColor(hex: "0F1014")
        label.font = UIFont(name: "PingFangSC-Semibold", size: 22)
        label.textAlignment = .center
        let tap = UITapGestureRecognizer(target: self, action: #selector(nameLabelTapped))
        label.addGestureRecognizer(tap)
        label.isUserInteractionEnabled = true
        return label
    }()
    
    let likesLabel: UILabel = {
        let label = UILabel()
        label.text = .likesText
        label.textColor = UIColor(hex: "8F9AB2")
        label.font = UIFont(name: "PingFangSC-Regular", size: 14)
        label.textAlignment = .center
        return label
    }()
    
    let likesCountLabel: UILabel = {
        let label = UILabel()
        label.text = "0"
        label.textColor = UIColor(hex: "0F1014")
        label.font = UIFont(name: "PingFangSC-Semibold", size: 14)
        label.textAlignment = .center
        return label
    }()
    
    let fansLabel: UILabel = {
        let label = UILabel()
        label.text = .fansText
        label.textColor = UIColor(hex: "8F9AB2")
        label.font = UIFont(name: "PingFangSC-Regular", size: 14)
        label.textAlignment = .center
        return label
    }()
    
    let fansCountLabel: UILabel = {
        let label = UILabel()
        label.text = "0"
        label.textColor = UIColor(hex: "0F1014")
        label.font = UIFont(name: "PingFangSC-Semibold", size: 14)
        label.textAlignment = .center
        return label
    }()
    
    var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        backgroundColor = .white
        isViewReady = true
    }
}

// MARK: Layout

extension MeRootView {
    private func constructViewHierarchy() {
        addSubview(imageView)
        addSubview(nameLabel)
        addSubview(likesLabel)
        addSubview(likesCountLabel)
        addSubview(fansLabel)
        addSubview(fansCountLabel)
    }
    
    private func activateConstraints() {
        imageView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(navigationFullHeight() + 16.scale375Height())
            make.leading.equalToSuperview().offset(138.scale375())
            make.height.width.equalTo(100.scale375())
        }
        
        nameLabel.snp.makeConstraints { make in
            make.top.equalTo(imageView.snp.bottom).offset(8.scale375Height())
            make.centerX.equalTo(imageView)
            make.width.equalTo(200.scale375())
            make.height.equalTo(28.scale375Height())
        }
        
        likesLabel.snp.makeConstraints { make in
            make.top.equalTo(nameLabel.snp.bottom).offset(26.scale375Height())
            make.leading.equalToSuperview().offset(104.scale375())
            make.width.equalTo(40.scale375())
            make.height.equalTo(22.scale375Height())
        }
        
        likesCountLabel.snp.makeConstraints { make in
            make.top.height.equalTo(likesLabel)
            make.leading.equalToSuperview().offset(148.scale375())
            make.width.equalTo(35.scale375())
        }
        
        fansLabel.snp.makeConstraints { make in
            make.top.equalTo(nameLabel.snp.bottom).offset(26.scale375Height())
            make.leading.equalToSuperview().offset(199.scale375())
            make.width.equalTo(40.scale375())
            make.height.equalTo(22.scale375Height())
        }
        
        fansCountLabel.snp.makeConstraints { make in
            make.top.height.equalTo(fansLabel)
            make.leading.equalToSuperview().offset(243.scale375())
            make.width.equalTo(30.scale375())
        }
    }
}

// MARK: Action

extension MeRootView {
    @objc private func nameLabelTapped() {
        rootVC?.nameLabelClick()
    }
}

// MARK: Localized String

private extension String {
    static let likesText = TUILiveKitAppLocalize("Likes")
    static let fansText = TUILiveKitAppLocalize("Fans")
}
