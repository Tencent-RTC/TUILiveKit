//
//  MainRootView.swift
//  TUILiveKitApp
//
//  Created by adams on 2021/6/4.
//

import UIKit
import SnapKit

class MainRootView: UIView {
    let loading = UIActivityIndicatorView(style: .medium)
    
    lazy var textContainerView: UIView = {
        let containerView = UIView(frame: .zero)
        containerView.backgroundColor = UIColor(0xF4F5F9)
        containerView.layer.cornerRadius = 8
        containerView.layer.masksToBounds = true
        return containerView
    }()
    
    lazy var streamIdTextField: TUITextField = {
        let textField = TUITextField(frame: .zero)
        textField.backgroundColor = .clear
        let leftLabel = UILabel(frame: CGRect(x: 0, y: 0, width: 120, height: 40))
        leftLabel.text = .streamIdText
        leftLabel.font = UIFont.systemFont(ofSize: 15)
        leftLabel.textColor = .black
        textField.leftView = leftLabel
        textField.leftViewMode = .always
        let attributeString = NSMutableAttributedString(string: .joinStreamIdText)
        attributeString.addAttributes([.foregroundColor : UIColor.gray, .font :
         UIFont.systemFont(ofSize: 15),], range: NSRange(location: 0, length:
         attributeString.length))
        textField.attributedPlaceholder = attributeString
        textField.addTarget(self, action: #selector(streamIdTextFieldValueChange(sender:)), for: .editingChanged)
        textField.textColor = .black
        return textField
    }()
    
    let roomSegment: UISegmentedControl = {
        let segment = UISegmentedControl(items: [String.videoLiveText, String.audioLiveText])
        segment.selectedSegmentTintColor = .systemBlue
        segment.selectedSegmentIndex = 0
        segment.backgroundColor = UIColor(0xd8d8d8)
        segment.setTitleTextAttributes([.foregroundColor: UIColor.white], for: .normal)
        segment.setTitleTextAttributes([.foregroundColor: UIColor.white], for: .selected)
        return segment
    }()
    
    lazy var joinBtn: UIButton = {
        let btn = UIButton(frame: .zero)
        btn.setTitle(.joinLiveStreamText, for: .normal)
        btn.setTitleColor(.white, for: .normal)
        btn.setBackgroundImage(UIColor(0xd8d8d8).trans2Image(), for: .normal)
        btn.setBackgroundImage(UIColor.systemBlue.trans2Image(), for: .selected)
        btn.layer.cornerRadius = 8
        btn.layer.masksToBounds = true
        return btn
    }()

    lazy var startBtn: UIButton = {
        let btn = UIButton()
        btn.setTitle(.startLiveStreamText, for: .normal)
        btn.setTitleColor(.white, for: .normal)
        btn.backgroundColor = .systemBlue
        btn.layer.cornerRadius = 24
        btn.titleLabel?.font = UIFont.systemFont(ofSize: 18)
        btn.titleLabel?.adjustsFontSizeToFitWidth = true
        return btn
    }()
    
    weak var rootVC: MainViewController?
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
    }
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesBegan(touches, with: event)
        streamIdTextField.resignFirstResponder()
    }
    
}

extension MainRootView {
    private func constructViewHierarchy() {
        addSubview(textContainerView)
        textContainerView.addSubview(streamIdTextField)
        addSubview(roomSegment)
        addSubview(joinBtn)
        addSubview(loading)
        addSubview(startBtn)
    }
    
    private func activateConstraints() {
        textContainerView.snp.makeConstraints { make in
            make.top.equalTo(safeAreaLayoutGuide.snp.top).offset(10)
            make.left.equalTo(20)
            make.right.equalTo(-20)
            make.height.equalTo(54)
        }
        
        streamIdTextField.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        roomSegment.snp.makeConstraints { make in
            make.top.equalTo(streamIdTextField.snp.bottom).offset(15)
            make.trailing.equalTo(streamIdTextField.snp.trailing)
            make.leading.equalTo(streamIdTextField.snp.leading)
            make.height.equalTo(30)
        }
        joinBtn.snp.makeConstraints { make in
            make.leading.equalTo(textContainerView.snp.leading)
            make.trailing.equalTo(textContainerView.snp.trailing)
            make.height.equalTo(textContainerView.snp.height)
            make.top.equalTo(roomSegment.snp.bottom).offset(40)
        }
        
        loading.snp.makeConstraints { make in
            make.width.height.equalTo(40)
            make.centerX.centerY.equalToSuperview()
        }
        
        startBtn.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.bottom.equalTo(safeAreaLayoutGuide.snp.bottom).offset(-60)
            make.size.equalTo(CGSize(width: 163, height: 60))
        }
    }
    
    private func bindInteraction() {
        startBtn.addTarget(self, action: #selector(startBtnClick), for:.touchUpInside)
        joinBtn.addTarget(self, action: #selector(joinBtnTouchEvent), for: .touchUpInside)
    }
    @objc
    private func startBtnClick(sender: UIButton) {
        rootVC?.startBtnClick(sender: sender, view: self)
    }
    @objc
    private func joinBtnTouchEvent(sender: UIButton) {
        guard let roomId = streamIdTextField.text else { return }
        rootVC?.joinBtnClick(sender: sender, view: self, roomId: roomId)
    }

    @objc
    private func streamIdTextFieldValueChange(sender: UITextField) {
        if let text = sender.text, text.count > 0 {
            joinBtn.isSelected = true
            joinBtn.isUserInteractionEnabled = true
        } else {
            joinBtn.isSelected = false
            joinBtn.isUserInteractionEnabled = false
        }
    }
}

class TUITextField: UITextField {
    override func leftViewRect(forBounds bounds: CGRect) -> CGRect {
        var rect = super.leftViewRect(forBounds: bounds)
        rect.origin.x += 10
        return rect
    }
}

private extension String {
    static let streamIdText = TUILiveKitAppLocalize("TUILiveKitApp.Main.streamId")
    static let joinStreamIdText = TUILiveKitAppLocalize("TUILiveKitApp.Main.joinStreamId")
    static let joinLiveStreamText = TUILiveKitAppLocalize("TUILiveKitApp.Main.joinLiveStream")
    static let startLiveStreamText = TUILiveKitAppLocalize("TUILiveKitApp.Main.startLiveStream")
    static let enterLiveStreamListText = TUILiveKitAppLocalize("TUILiveKitApp.Main.enterLiveStreamList")
    static let videoLiveText = TUILiveKitAppLocalize("TUILiveKitApp.Main.video")
    static let audioLiveText = TUILiveKitAppLocalize("TUILiveKitApp.Main.audio")
}
