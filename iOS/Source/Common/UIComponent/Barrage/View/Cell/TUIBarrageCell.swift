//
// TUIBarrageCell.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

import SnapKit
import UIKit

class TUIBarrageCell: UITableViewCell {
    static let identifier: String = "BarrageCell"
    private var isCustomCell = false
    private var defaultCell = UIView()

    private var customCell = UIView()

    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        backgroundColor = .clear
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    func useDefaultCell(barrage: TUIBarrage) {
        for subView in contentView.subviews {
            subView.removeFromSuperview()
        }
        defaultCell = TUIBarrageCellFactory.createCell(type: .default, param: ["barrage": barrage])
        for subView in defaultCell.subviews {
            subView.removeFromSuperview()
        }
        contentView.addSubview(defaultCell)
        isCustomCell = false
    }

    func useCustomCell(_ cell: UIView) {
        for subView in contentView.subviews {
            subView.removeFromSuperview()
        }
        customCell = TUIBarrageCellFactory.createCell(type: .custom, param: ["customView": cell])
        contentView.addSubview(customCell)
        isCustomCell = true
    }

    func getCellHeight() -> CGFloat {
        return isCustomCell ? (customCell.mm_h + 16.scale375Height()) :
            (defaultCell.mm_h + 16.scale375Height())
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
            return TUIBarrageDefaultCell(TUIBarrage: barrage)
        case .custom:
            let customView = param["customView"] as? UIView ?? UIView()
            return TUIBarrageCustomCell(customView: customView)
        }
    }
}

class TUIBarrageDefaultCell: UIView {
    let TUIBarrage: TUIBarrage

    private let levelButton: UIButton = {
        let button = UIButton()
        button.backgroundColor = .b1
        button.layer.borderColor = UIColor.blueColor.cgColor
        button.layer.borderWidth = 0.5
        button.layer.cornerRadius = 7
        button.titleLabel?.textColor = .flowKitWhite
        button.isEnabled = false
        button.titleLabel?.font = UIFont(name: "PingFangSC-Regular", size: 12)
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

    init(TUIBarrage: TUIBarrage) {
        self.TUIBarrage = TUIBarrage
        super.init(frame: .zero)
        backgroundColor = .g2.withAlphaComponent(0.4)
        setupDefaultCell(TUIBarrage)
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
            make.width.equalTo(33.scale375())
            make.height.equalTo(14.scale375Height())
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
        levelButton.setAttributedTitle(getLevelAttributedText(level: barrage.user.level), for: .normal)
        barrageLabel.attributedText = getBarrageLabelAttributedText(barrage: barrage)
        barrageLabel.sizeToFit()
        mm_h = barrageLabel.mm_h + 2.scale375Height() + 10.scale375Height()
        mm_w = barrageLabel.mm_w + 16.scale375()
        if mm_h < 50 {
            layer.cornerRadius = mm_h * 0.5
        } else {
            layer.cornerRadius = 18
        }
    }

    func getLevelAttributedText(level: String) -> NSMutableAttributedString {
        let levelText = "LV."
        let levelFont =
            UIFont(name: "PingFangSC-Regular", size: 7) ?? UIFont.systemFont(ofSize: 6)
        let levelAttributes: [NSAttributedString.Key: Any] =
            [.font: levelFont]
        let levelAttributedString =
            NSMutableAttributedString(string: levelText, attributes: levelAttributes)

        let userLevelText = level.count == 0 ? "0" : level
        let userLevelTextFont =
            UIFont(name: "PingFangSC-Regular", size: 12) ?? UIFont.systemFont(ofSize: 12)
        let userLevelAttributes: [NSAttributedString.Key: Any] =
            [.font: userLevelTextFont]
        let userLevelAttributedString =
            NSMutableAttributedString(string: userLevelText
                                      , attributes: userLevelAttributes)
        levelAttributedString.append(userLevelAttributedString)
        return levelAttributedString
    }

    func getBarrageLabelAttributedText(barrage: TUIBarrage)
        -> NSMutableAttributedString {
        let placeholderString = String(repeating: " ", count: 11)
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
        if mm_h < 50 {
            layer.cornerRadius = mm_h * 0.5
        } else {
            layer.cornerRadius = 18
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
            make.top.equalToSuperview().inset(6.scale375())
            make.width.equalTo(self.customView.mm_w)
            make.height.equalTo(self.customView.mm_h)
        }
    }
}
