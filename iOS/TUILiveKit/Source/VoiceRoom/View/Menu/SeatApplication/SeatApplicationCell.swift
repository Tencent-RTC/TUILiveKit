//
//  SeatApplicationCell.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/21.
//

import UIKit
import Combine

class SeatApplicationCell: UITableViewCell {
    static let identifier = "SeatApplicationCell"
    var cancellableSet = Set<AnyCancellable>()
    
    var acceptAction: (() -> Void)?
    var rejectAction: (() -> Void)?
    
    var userName: String = "" {
        didSet {
            userInfoView.nameLabel.text = userName
        }
    }
    
    var userAvatar: String = "" {
        didSet {
            let url = URL(string: userAvatar)
            userInfoView.avatarImageView.kf.setImage(with: url)
        }
    }
    
    var buttonConfigs: [HorizontalButtonStackConfig] {
        return [
            HorizontalButtonStackConfig(title: .agree, color: .b1, style: .fill, action: { [weak self] sender in
                guard let self = self else { return }
                self.acceptAction?()
            }),
            HorizontalButtonStackConfig(title: .reject, color: .b1, style: .hollow, action: { [weak self] sender in
                guard let self = self else { return }
                self.rejectAction?()
            }),
        ]
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        setupStyle()
        isViewReady = true
    }
    
    let userInfoView: CellUserInfoView = {
        let view = CellUserInfoView(frame: .zero)
        return view
    }()
    
    lazy var operateView: HorizontalButtonStackView = {
        let view = HorizontalButtonStackView(operateConfigs: self.buttonConfigs)
        return view
    }()
    
    let lineView: UIView = {
        let view = UIView()
        view.backgroundColor = .g3.withAlphaComponent(0.3)
        return view
    }()
    
    func constructViewHierarchy() {
        contentView.addSubview(userInfoView)
        contentView.addSubview(lineView)
        contentView.addSubview(operateView)
    }
    
    func activateConstraints() {
        let operateViewWidth: CGFloat = HorizontalButtonStackView.buttonWidth * CGFloat(buttonConfigs.count) + 
        (HorizontalButtonStackView.spacing * CGFloat(buttonConfigs.count - 1))
        userInfoView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(24)
            make.centerY.equalToSuperview()
            make.height.equalToSuperview()
            make.trailing.equalTo(operateView.snp.leading).offset(20)
        }
        operateView.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-24)
            make.centerY.equalToSuperview()
            make.height.equalTo(HorizontalButtonStackView.buttonHeight)
            make.width.equalTo(operateViewWidth)
        }
        lineView.snp.makeConstraints { make in
            make.height.equalTo(1)
            make.bottom.equalToSuperview()
            make.trailing.equalTo(operateView.snp.trailing)
            make.leading.equalTo(userInfoView.nameLabel.snp.leading)
        }
    }
    
    func setupStyle() {
        backgroundColor = .clear
    }
}

fileprivate extension String {
    static let agree = localized("live.anchor.link.agree.title")
    static let reject = localized("live.anchor.link.reject.title")
}


