//
//  AlertPanel.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/7/25.
//

import UIKit
import RTCCommon

typealias VRAlertButtonClickClosure = (VRAlertPanel) -> Void

struct VRAlertInfo {
    let description: String
    let imagePath: String?
    
    let cancelButtonInfo: (title: String, titleColor: UIColor)?
    let defaultButtonInfo: (title:String, titleColor: UIColor)
    
    let cancelClosure: VRAlertButtonClickClosure?
    let defaultClosure: VRAlertButtonClickClosure
    
    init(description: String, 
         imagePath: String?,
         cancelButtonInfo: (title: String, titleColor: UIColor)? = nil,
         defaultButtonInfo: (title: String, titleColor: UIColor) = (.confirmText, .b1),
         cancelClosure: VRAlertButtonClickClosure?,
         defaultClosure: @escaping VRAlertButtonClickClosure) {
        self.description = description
        self.imagePath = imagePath
        self.cancelButtonInfo = cancelButtonInfo
        self.defaultButtonInfo = defaultButtonInfo
        self.cancelClosure = cancelClosure
        self.defaultClosure = defaultClosure
    }
    
}

class VRAlertPanel: UIView {
    private let alertInfo: VRAlertInfo
    
    let alertContentView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .white
        view.layer.cornerRadius = 10
        return view
    }()
    
    let descriptionContentView: UIView = {
        let view = UIView(frame: .zero)
        return view
    }()
    
    let avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 12.scale375()
        imageView.layer.masksToBounds = true
        return imageView
    }()
    
    let descriptionLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .g1
        label.textAlignment = .center
        label.numberOfLines = 0
        label.font = .customFont(ofSize: 16, weight: .semibold)
        return label
    }()
    
    let horizontalSeparator: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .g7
        return view
    }()
    
    let verticalSeparator: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .g7
        return view
    }()
    
    let cancelButton: UIButton = {
        let button = UIButton(frame: .zero)
        button.titleLabel?.font = .customFont(ofSize: 16, weight: .regular)
        return button
    }()
    
    let defaultButton: UIButton = {
        let button = UIButton(frame: .zero)
        button.titleLabel?.font = .customFont(ofSize: 16, weight: .medium)
        return button
    }()
    
    init(alertInfo: VRAlertInfo) {
        self.alertInfo = alertInfo
        super.init(frame: UIScreen.main.bounds)
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        setupStyle()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    func show() {
        WindowUtils.getCurrentWindowViewController()?.view.addSubview(self)
    }
    
    func dismiss() {
        removeFromSuperview()
    }
}

extension VRAlertPanel {
    private func constructViewHierarchy() {
        addSubview(alertContentView)
        alertContentView.addSubview(descriptionContentView)
        if alertInfo.imagePath != nil {
            descriptionContentView.addSubview(avatarImageView)
        }
        descriptionContentView.addSubview(descriptionLabel)
        alertContentView.addSubview(horizontalSeparator)
        alertContentView.addSubview(defaultButton)
        
        if alertInfo.cancelButtonInfo != nil {
            alertContentView.addSubview(cancelButton)
            alertContentView.addSubview(verticalSeparator)
        }
    }
    
    private func activateConstraints() {
        
        alertContentView.snp.makeConstraints { make in
            make.center.equalToSuperview()
            make.width.equalTo(323.scale375())
        }
        
        descriptionContentView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(24.scale375Height())
            make.centerX.equalToSuperview()
            make.bottom.equalTo(horizontalSeparator.snp.top).offset(-24.scale375Height())
            make.width.lessThanOrEqualToSuperview().multipliedBy(0.8)
        }
        
        if alertInfo.imagePath != nil {
            avatarImageView.snp.makeConstraints { make in
                make.leading.equalToSuperview()
                make.trailing.equalTo(descriptionLabel.snp.leading).offset(-4.scale375())
                make.centerY.equalTo(descriptionLabel.snp.centerY)
                make.size.equalTo(CGSize(width: 24.scale375(), height: 24.scale375()))
            }
            
            descriptionLabel.snp.makeConstraints { make in
                make.leading.equalTo(avatarImageView.snp.trailing).offset(4.scale375())
                make.top.trailing.bottom.equalToSuperview()
            }
        } else {
            descriptionLabel.snp.makeConstraints { make in
                make.leading.trailing.top.bottom.equalToSuperview()
            }
        }
        
        horizontalSeparator.snp.makeConstraints { make in
            make.height.equalTo(1)
            make.leading.trailing.equalToSuperview()
            make.top.equalTo(descriptionContentView.snp.bottom)
        }
        
        if alertInfo.cancelButtonInfo != nil {
            verticalSeparator.snp.makeConstraints { make in
                make.centerX.equalToSuperview()
                make.width.equalTo(1)
                make.bottom.equalToSuperview()
                make.top.equalTo(horizontalSeparator.snp.bottom)
            }
            
            cancelButton.snp.makeConstraints { make in
                make.bottom.leading.equalToSuperview()
                make.height.equalTo(54.scale375Height())
                make.top.equalTo(horizontalSeparator.snp.bottom)
                make.trailing.equalTo(verticalSeparator.snp.leading)
            }
            
            defaultButton.snp.makeConstraints { make in
                make.bottom.trailing.equalToSuperview()
                make.height.equalTo(54.scale375Height())
                make.top.equalTo(horizontalSeparator.snp.bottom)
                make.leading.equalTo(verticalSeparator.snp.trailing)
            }
        } else {
            defaultButton.snp.makeConstraints { make in
                make.leading.bottom.trailing.equalToSuperview()
                make.height.equalTo(54.scale375Height())
                make.top.equalTo(horizontalSeparator.snp.bottom)
            }
        }
    }
    
    private func bindInteraction() {
        cancelButton.addTarget(self, action: #selector(cancelButtonClick(sender:)), for: .touchUpInside)
        defaultButton.addTarget(self, action: #selector(defaultButtonClick(sender:)), for: .touchUpInside)
    }
    
    private func setupStyle() {
        if let imagePath = alertInfo.imagePath {
            avatarImageView.kf.setImage(with: URL(string:imagePath), placeholder: UIImage.avatarPlaceholderImage)
        }
        
        descriptionLabel.text = alertInfo.description
        
        if let cancelButtonInfo = alertInfo.cancelButtonInfo {
            cancelButton.setTitle(cancelButtonInfo.title, for: .normal)
            cancelButton.setTitleColor(cancelButtonInfo.titleColor, for: .normal)
        }
        
        defaultButton.setTitle(alertInfo.defaultButtonInfo.title, for: .normal)
        defaultButton.setTitleColor(alertInfo.defaultButtonInfo.titleColor, for: .normal)
    }
}

extension VRAlertPanel {
    @objc
    private func cancelButtonClick(sender: UIButton) {
        if let cancelClosure = alertInfo.cancelClosure {
            cancelClosure(self)
        }
    }
    
    @objc
    private func defaultButtonClick(sender: UIButton) {
        alertInfo.defaultClosure(self)
    }
}


fileprivate extension String {
    static let confirmText = localized("Confirm")
}
