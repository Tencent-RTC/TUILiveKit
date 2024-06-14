//
// TUIBarrageCell.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

import RTCCommon
import SnapKit
import UIKit

class TUIBarrageCell: UITableViewCell {
    static let identifier: String = "BarrageCell"
    private var isCustomCell = false
    private var defaultCell = UIView()
    private var customCell = UIView()

    override func prepareForReuse() {
        super.prepareForReuse()
        contentView.subviews.forEach { $0.removeFromSuperview()}
    }

    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        backgroundColor = .clear
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    func useDefaultCell(barrage: TUIBarrage) {
        defaultCell = TUIBarrageCellFactory.createCell(type: .default, param: ["barrage": barrage])
        contentView.addSubview(defaultCell)
        isCustomCell = false
    }

    func useCustomCell(_ cell: UIView) {
        customCell = TUIBarrageCellFactory.createCell(type: .custom, param: ["customView": cell])
        contentView.addSubview(customCell)
        isCustomCell = true
    }

    func getCellHeight() -> CGFloat {
        return isCustomCell ? (customCell.mm_h + 4.scale375Height()) :
            (defaultCell.mm_h + 4.scale375Height())
    }
}

class TUIBarrageCellFactory {
    enum CellType {
        case `default`
        case custom
    }

    static func createCell(type: CellType, param: [String: Any]) -> UIView {
        switch type {
        case .default:
            let barrage = param["barrage"] as? TUIBarrage ?? TUIBarrage()
            return TUIBarrageDefaultCell(barrage: barrage)
        case .custom:
            let customView = param["customView"] as? UIView ?? UIView()
            return TUIBarrageCustomCell(customView: customView)
        }
    }
}

class TUIBarrageDefaultCell: UIView {
    let barrage: TUIBarrage
    var isOwner: Bool {
        barrage.user.userId == TUIBarrageStore.shared.ownerId
    }

    private let levelButton: UIButton = {
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

    private let anchorButton: UIButton = {
        let button = UIButton()
        button.backgroundColor = UIColor(hex: "#4D8EFF")
        button.layer.cornerRadius = 7
        button.setTitle(.anchorText, for: .normal)
        button.setTitleColor(.white, for: .normal)
        button.titleLabel?.font = .customFont(ofSize: 8, weight: .semibold)
        return button
    }()

    private lazy var barrageLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "PingFangSC-Regular", size: 14)
        label.numberOfLines = 5
        label.textAlignment = .left
        label.lineBreakMode = .byTruncatingTail
        label.textColor = .white
        return label
    }()

    init(barrage: TUIBarrage) {
        self.barrage = barrage
        super.init(frame: .zero)
        backgroundColor = .g1.withAlphaComponent(0.4)
        setupDefaultCell(barrage)
    }

    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    func constructViewHierarchy() {
        addSubview(barrageLabel)
        addSubview(levelButton)
        if isOwner {
            addSubview(anchorButton)
        }
    }

    func activateConstraints() {
        snp.makeConstraints { [weak self] make in
            guard let self = self else { return }
            make.top.leading.equalToSuperview()
            make.width.equalTo(self.mm_w)
            make.height.equalTo(self.mm_h)
        }
        levelButton.snp.makeConstraints { make in
            make.leading.equalToSuperview().inset(8.scale375())
            make.top.equalToSuperview().offset(8.scale375Height())
            make.width.equalTo(35.scale375())
            make.height.equalTo(14.scale375Height())
        }
        if isOwner {
            anchorButton.snp.makeConstraints { make in
                make.leading.equalTo(levelButton.snp.trailing).offset(5.scale375())
                make.centerY.equalTo(levelButton)
                make.height.equalTo(14.scale375Height())
                make.width.equalTo(42.scale375())
            }
        }
        barrageLabel.snp.makeConstraints { [weak self] make in
            guard let self = self else { return }
            make.leading.equalToSuperview().inset(8.scale375())
            make.top.equalToSuperview().inset(6.scale375())
            make.width.equalTo(self.barrageLabel.mm_w)
            make.height.equalTo(self.barrageLabel.mm_h)
        }
    }

    func setupDefaultCell(_ barrage: TUIBarrage) {
        barrageLabel.mm_w = 231.scale375()
        let level = getLevel(barrage: barrage)
        levelButton.backgroundColor = getLevelBackground(level: level)
        levelButton.setImage(getLevelImage(level: level), for: .normal)
        levelButton.setTitle("\(level)", for: .normal)
        barrageLabel.attributedText = getBarrageLabelAttributedText(barrage: barrage)
        barrageLabel.sizeToFit()
        mm_h = barrageLabel.mm_h + 2.scale375Height() + 10.scale375Height()
        mm_w = barrageLabel.mm_w + 16.scale375()
        if mm_h < 40 {
            layer.cornerRadius = mm_h * 0.5
        } else {
            layer.cornerRadius = 12
        }
    }

    func getBarrageLabelAttributedText(barrage: TUIBarrage)
        -> NSMutableAttributedString {
        let placeholderString = String(repeating: " ", count: isOwner ? 28 : 13)
        let isNormal = isNormalMessage(barrage: barrage)
        let userName = barrage.user.userName + (isNormal ? "：" : "")
        let userNameAttributes: [NSAttributedString.Key: Any] =
            [.foregroundColor: UIColor.lightBlueColor, .font: UIFont.systemFont(ofSize: 12)]
        let userNameAttributedText = NSMutableAttributedString(string: "\(placeholderString)\(userName)",
                                                               attributes: userNameAttributes)

        let contentFont = UIFont(name: "PingFangSC-Regular", size: 12) ?? UIFont.systemFont(ofSize: 12)
        let contentAttributes: [NSAttributedString.Key: Any] =
            [.font: contentFont]
        let contentAttributedText: NSMutableAttributedString = isNormal ? getBarrageContentAttributedText(content: barrage.content) :
            NSMutableAttributedString(string: barrage.content, attributes: contentAttributes)
        userNameAttributedText.append(contentAttributedText)
        return userNameAttributedText
    }

    func isNormalMessage(barrage: TUIBarrage) -> Bool {
        return barrage.user.level != ""
    }

    func getBarrageContentAttributedText(content: String)
        -> NSMutableAttributedString {
        return EmotionHelper.shared.obtainImagesAttributedString(byText: content,
                                                                 font: UIFont(name: "PingFangSC-Regular", size: 12) ??
                                                                     UIFont.systemFont(ofSize: 12))
    }

    private func getLevel(barrage: TUIBarrage) -> Int {
        return Int.random(in: 0...120)
    }
    
    private func getLevelImage(level: Int) -> UIImage? {
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

    private func getLevelBackground(level: Int) -> UIColor {
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

class TUIBarrageCustomCell: UIView {
    private var customView: UIView
    init(customView: UIView) {
        self.customView = customView
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isViewReady = false

    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        setupCustomCell()
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }

    func setupCustomCell() {
        mm_h = customView.mm_h + 12.scale375Height()
        mm_w = customView.mm_w + 16.scale375()
        layer.cornerRadius = mm_h * 0.5
        if mm_h < 40 {
            layer.cornerRadius = mm_h * 0.5
        } else {
            layer.cornerRadius = 12
        }
    }

    func constructViewHierarchy() {
        addSubview(customView)
    }

    func activateConstraints() {
        snp.makeConstraints { [weak self] make in
            guard let self = self else { return }
            make.top.leading.equalToSuperview()
            make.width.equalTo(self.mm_w)
            make.height.equalTo(self.mm_h)
        }

        customView.snp.makeConstraints { make in
            make.leading.equalToSuperview().inset(8.scale375())
            make.centerY.equalToSuperview()
            make.width.equalTo(self.customView.mm_w)
            make.height.equalTo(self.customView.mm_h)
        }
    }
}

private extension String {
    static let anchorText = localized("live.barrage.anchor")
}
