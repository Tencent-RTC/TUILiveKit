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
        if isCustomCell {
            return customCell.mm_h + 6
        }
        guard let defaultCell = defaultCell as? TUIBarrageDefaultCell else {
            return defaultCell.mm_h + 4.scale375Height()
        }
        var barrageContentHeight = defaultCell.barrageContentSize.height
        if barrageContentHeight < 18 {
            barrageContentHeight = 18
        }
        return barrageContentHeight + 2*4 + 6
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
    let barrageContentMaxWidth: CGFloat = 240.scale375()
    lazy var barrageContentSize: CGSize = {
        let barrageSize = CGSize(width: barrageContentMaxWidth, height: CGFloat(integerLiteral: Int.max))
        return barrageLabel.sizeThatFits(barrageSize)
    }()

    private let levelButton: UIButton = {
        let button = UIButton()
        button.layer.cornerRadius = 7
        button.titleLabel?.textColor = .flowKitWhite
        button.isEnabled = false
        let spacing: CGFloat = 2
        button.imageEdgeInsets = UIEdgeInsets(top: 0, left: -spacing / 2, bottom: 0, right: spacing / 2)
        button.titleEdgeInsets = UIEdgeInsets(top: 0, left: spacing / 2, bottom: 0, right: -spacing / 2)
        button.titleLabel?.font = .customFont(ofSize: 10, weight: .semibold)
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
        label.font = .customFont(ofSize: 12, weight: .semibold)
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
        snp.makeConstraints { make in
            make.top.leading.equalToSuperview()
        }
        levelButton.snp.makeConstraints { make in
            make.leading.equalToSuperview().inset(8)
            make.top.equalToSuperview().offset(6)
            make.width.equalTo(35)
            make.height.equalTo(14)
        }
        if isOwner {
            anchorButton.snp.makeConstraints { make in
                make.leading.equalTo(levelButton.snp.trailing).offset(5)
                make.centerY.equalTo(levelButton)
                make.height.equalTo(14)
                make.width.equalTo(42)
            }
        }
        barrageLabel.snp.makeConstraints { [weak self] make in
            guard let self = self else { return }
            let contentSize = self.barrageContentSize
            make.leading.equalTo(levelButton.snp.trailing).offset(5)
            make.trailing.equalToSuperview().inset(8)
            
            make.top.bottom.equalToSuperview().inset(4)

            make.width.equalTo(contentSize.width)
            if contentSize.height > 18 {
                make.height.equalTo(contentSize.height)
            } else {
                make.height.equalTo(18)
            }
        }
    }

    func setupDefaultCell(_ barrage: TUIBarrage) {
        if isOwner {
            barrage.user.level = "65"
        } else {
            barrage.user.level = "32"
        }
        let level = getLevel(barrage: barrage)
        levelButton.backgroundColor = getLevelBackground(level: level)
        levelButton.setImage(getLevelImage(level: level), for: .normal)
        levelButton.setTitle("\(level)", for: .normal)
        barrageLabel.attributedText = getBarrageLabelAttributedText(barrage: barrage)
        layer.cornerRadius = 13
    }

    func getBarrageLabelAttributedText(barrage: TUIBarrage)
        -> NSMutableAttributedString {
            let placeholderString = String(repeating: " ", count: isOwner ? 12 : 0)
        let isNormal = isNormalMessage(barrage: barrage)
        let userName = barrage.user.userName + (isNormal ? "：" : "")
        let userNameAttributes: [NSAttributedString.Key: Any] =
            [.foregroundColor: UIColor.lightBlueColor, .font: UIFont.customFont(ofSize: 12, weight: .semibold)]
        let userNameAttributedText = NSMutableAttributedString(string: "\(placeholderString)\(userName)",
                                                               attributes: userNameAttributes)
                 
        let contentFont = UIFont.customFont(ofSize: 12, weight: .semibold)
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
                                                                 font: UIFont.customFont(ofSize: 12, weight: .semibold))
    }

    private func getLevel(barrage: TUIBarrage) -> Int {
        if let level = Int(barrage.user.level){
            return max(level, 0)
        } else {
            return 0
        }
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
        mm_h = customView.mm_h
        mm_w = customView.mm_w + 16.scale375()
        layer.cornerRadius = mm_h * 0.5
        if mm_h < 40 {
            layer.cornerRadius = mm_h * 0.5
        } else {
            layer.cornerRadius = 13
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

        customView.snp.remakeConstraints { make in
            make.leading.equalToSuperview()
            make.centerY.equalToSuperview()
            make.width.equalTo(self.customView.mm_w + 8)
            make.height.equalTo(self.customView.mm_h)
        }
    }
}

private extension String {
    static let anchorText = localized("live.barrage.anchor")
}
