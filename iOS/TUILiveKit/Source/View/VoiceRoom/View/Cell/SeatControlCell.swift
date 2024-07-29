//
//  SeatControlCell.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/7/16.
//

import UIKit

class SeatControlCell: UITableViewCell {
    let avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        return imageView
    }()
    
    let userNameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = UIFont.customFont(ofSize: 16)
        label.textColor = .grayColor
        label.adjustsFontSizeToFitWidth = true
        return label
    }()
    
    let levelButton: UIButton = {
        let button = UIButton()
        button.layer.cornerRadius = 7.scale375Height()
        button.titleLabel?.textColor = .flowKitWhite
        button.isEnabled = false
        let spacing: CGFloat = 2.scale375()
        button.imageEdgeInsets = UIEdgeInsets(top: 0, left: -spacing / 2, bottom: 0, right: spacing / 2)
        button.titleEdgeInsets = UIEdgeInsets(top: 0, left: spacing / 2, bottom: 0, right: -spacing / 2)
        button.titleLabel?.font = UIFont(name: "PingFangSC-Regular", size: 12)
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
        contentView.addSubview(levelButton)
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
        
        levelButton.snp.makeConstraints { make in
            make.centerY.equalToSuperview()
            make.leading.equalTo(userNameLabel.snp.trailing).offset(4.scale375())
            make.width.equalTo(35.scale375())
            make.height.equalTo(14.scale375Height())
        }
    }
    
    func bindInteraction() {}
    
    func getLevel() -> Int {
        return 0
    }
    
    func getLevelImage(level: Int) -> UIImage? {
        if level <= 30 {
            return UIImage(named: "barrage_level1", in: Bundle.liveBundle, compatibleWith: nil)
        } else if level <= 60 {
            return UIImage(named: "barrage_level2", in: Bundle.liveBundle, compatibleWith: nil)
        } else if level <= 90 {
            return UIImage(named: "barrage_level3", in: Bundle.liveBundle, compatibleWith: nil)
        } else {
            return UIImage(named: "barrage_level4", in: Bundle.liveBundle, compatibleWith: nil)
        }
    }
    
    func getLevelBackground(level: Int) -> UIColor {
        if level <= 30 {
            return UIColor.horizontalGradientColor(colors: [UIColor(hex: "#6CFFE5"), UIColor(hex: "#82FFE1")],
                                                   frame: CGRect(x: 0, y: 0, width: 35.scale375(), height: 14.scale375Height()))
        } else if level <= 60 {
            return UIColor.horizontalGradientColor(colors: [UIColor(hex: "#6CA7FF"), UIColor(hex: "#82B4FF")],
                                                   frame: CGRect(x: 0, y: 0, width: 35.scale375(), height: 14.scale375Height()))
        } else if level <= 90 {
            return UIColor.horizontalGradientColor(colors: [UIColor(hex: "#9B6CFF"), UIColor(hex: "#AA82FF")],
                                                   frame: CGRect(x: 0, y: 0, width: 35.scale375(), height: 14.scale375Height()))
        } else {
            return UIColor.horizontalGradientColor(colors: [UIColor(hex: "#FF6C87"), UIColor(hex: "#FF82CD")],
                                                   frame: CGRect(x: 0, y: 0, width: 35.scale375(), height: 14.scale375Height()))
        }
    }
}

class OnTheSeatCell: SeatControlCell {
    static let identifier = "OnTheSeatCell"
    var kickoffEventClosure: ((SeatInfo) -> Void)?
    var seatInfo: SeatInfo?
    
    let seatIndexLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .white
        label.font = UIFont.customFont(ofSize: 12)
        label.backgroundColor = .g1
        label.alpha = 0.8
        label.textAlignment = .center
        return label
    }()
    
    let kickoffSeatButton: UIButton = {
        let button = UIButton()
        button.layer.cornerRadius = 12
        button.layer.borderWidth = 1
        button.layer.borderColor = UIColor.redColor.cgColor
        button.titleLabel?.font = UIFont.customFont(ofSize: 12)
        button.setTitleColor(UIColor.redColor, for: .normal)
        button.backgroundColor = .clear
        button.setTitle(.endTitleText, for: .normal)
        return button
    }()
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func draw(_ rect: CGRect) {
        super.draw(rect)
        seatIndexLabel.roundedRect(.allCorners, withCornerRatio: 8.scale375())
    }
    
    override func constructViewHierarchy() {
        super.constructViewHierarchy()
        contentView.addSubview(seatIndexLabel)
        contentView.addSubview(kickoffSeatButton)
    }
    
    override func activateConstraints() {
        super.activateConstraints()
        seatIndexLabel.snp.makeConstraints { make in
            make.trailing.bottom.equalTo(avatarImageView)
            make.size.equalTo(CGSize(width: 16.scale375(), height: 16.scale375()))
        }
        
        kickoffSeatButton.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-24.scale375())
            make.centerY.equalToSuperview()
            make.size.equalTo(CGSize(width: 60.scale375(), height: 24.scale375()))
        }
    }
    
    override func bindInteraction() {
        super.bindInteraction()
        kickoffSeatButton.addTarget(self, action: #selector(kickoffSeatButtonClick(sender:)), for: .touchUpInside)
    }
    
    func updateSeatInfo(seatInfo: SeatInfo) {
        self.seatInfo = seatInfo
        avatarImageView.kf.setImage(with: URL(string: seatInfo.avatarUrl))
        userNameLabel.text = seatInfo.userName
        seatIndexLabel.text = "\(seatInfo.index + 1)"
        let level = getLevel()
        levelButton.backgroundColor = getLevelBackground(level: level)
        levelButton.setImage(getLevelImage(level: level), for: .normal)
        levelButton.setTitle("\(level)", for: .normal)
    }
    
    @objc
    private func kickoffSeatButtonClick(sender: UIButton) {
        if let kickoffEventClosure = kickoffEventClosure, let seatInfo = seatInfo {
            kickoffEventClosure(seatInfo)
        }
    }
}

class ApplyTakeSeatCell: SeatControlCell {
    static let identifier = "ApplyTakeSeatCell"
    var approveEventClosure: (( SeatApplication) -> Void)?
    var rejectEventClosure: ((SeatApplication) -> Void)?
    var seatApplication: SeatApplication?
    
    let approveButton: UIButton = {
        let button = UIButton()
        button.layer.cornerRadius = 12
        button.titleLabel?.font = UIFont.customFont(ofSize: 12)
        button.setTitleColor(.white, for: .normal)
        button.backgroundColor = .b1
        button.setTitle(.approveText, for: .normal)
        return button
    }()
    
    let rejectButton: UIButton = {
        let button = UIButton()
        button.layer.cornerRadius = 12
        button.layer.borderWidth = 1
        button.layer.borderColor = UIColor.b1.cgColor
        button.titleLabel?.font = UIFont.customFont(ofSize: 12)
        button.setTitleColor(UIColor.b1, for: .normal)
        button.backgroundColor = .clear
        button.setTitle(.rejectText, for: .normal)
        return button
    }()
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func constructViewHierarchy() {
        super.constructViewHierarchy()
        contentView.addSubview(approveButton)
        contentView.addSubview(rejectButton)
    }
    
    override func activateConstraints() {
        super.activateConstraints()
        approveButton.snp.makeConstraints { make in
            make.trailing.equalTo(rejectButton.snp.leading).offset(-10.scale375())
            make.centerY.equalToSuperview()
            make.size.equalTo(CGSize(width: 60.scale375(), height: 24.scale375()))
        }
        
        rejectButton.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-24.scale375())
            make.centerY.equalToSuperview()
            make.size.equalTo(CGSize(width: 60.scale375(), height: 24.scale375()))
        }
    }
    
    override func bindInteraction() {
        super.bindInteraction()
        approveButton.addTarget(self, action: #selector(approveButtonClick(sender:)), for: .touchUpInside)
        rejectButton.addTarget(self, action: #selector(rejectButtonClick(sender:)), for: .touchUpInside)
    }
    
    func updateSeatApplication(seatApplication: SeatApplication) {
        self.seatApplication = seatApplication
        avatarImageView.kf.setImage(with: URL(string: seatApplication.avatarUrl))
        userNameLabel.text = seatApplication.userName
        let level = getLevel()
        levelButton.backgroundColor = getLevelBackground(level: level)
        levelButton.setImage(getLevelImage(level: level), for: .normal)
        levelButton.setTitle("\(level)", for: .normal)
    }
    
    @objc
    private func approveButtonClick(sender: UIButton) {
        if let approveEventClosure = approveEventClosure, let seatApplication = seatApplication {
            approveEventClosure(seatApplication)
        }
    }
    
    @objc
    private func rejectButtonClick(sender: UIButton) {
        if let rejectEventClosure = rejectEventClosure, let seatApplication = seatApplication {
            rejectEventClosure(seatApplication)
        }
    }
}

extension String {
    fileprivate static let endTitleText = localized("live.anchor.link.hang.up.title")
    fileprivate static let approveText = localized("live.anchor.link.agree.title")
    fileprivate static let rejectText = localized("live.anchor.link.reject.title")
}
