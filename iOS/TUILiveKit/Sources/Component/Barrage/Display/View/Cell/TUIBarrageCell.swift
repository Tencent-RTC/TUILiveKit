//
// TUIBarrageCell.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

import RTCCommon
import SnapKit
import UIKit

fileprivate let cellMargin: CGFloat = 6.scale375Height()
fileprivate let barrageContentMaxWidth: CGFloat = 240.scale375Width()

class TUIBarrageCell: UITableViewCell {
    static let identifier: String = "BarrageCell"
    private var contentCell: UIView?
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        backgroundColor = .clear
        selectionStyle = .none
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    func setContent(_ barrage: TUIBarrage, ownerId: String) {
        if let cell = contentCell {
            cell.removeFromSuperview()
        }
        let cell = TUIBarrageDefaultCell(barrage: barrage, ownerId: ownerId)
        contentView.addSubview(cell)
        contentCell = cell
        cell.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }

    func setContent(_ view: UIView) {
        if let cell = contentCell {
            cell.removeFromSuperview()
        }
        let cell = TUIBarrageCustomCell(customView: view)
        contentView.addSubview(cell)
        contentCell = cell
        cell.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
}

class TUIBarrageDefaultCell: UIView {
    private let barrage: TUIBarrage
    private let ownerId: String
    private var isOwner: Bool {
        barrage.user.userId == ownerId
    }

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
    
    private lazy var backgroundView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .g1.withAlphaComponent(0.4)
        view.layer.cornerRadius = 13
        view.clipsToBounds = true
        return view
    }()

    init(barrage: TUIBarrage, ownerId: String) {
        self.barrage = barrage
        self.ownerId = ownerId
        super.init(frame: .zero)
        backgroundColor = .clear
        setupDefaultCell(barrage, ownerId: ownerId)
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
        addSubview(backgroundView)
        backgroundView.addSubview(barrageLabel)
        backgroundView.addSubview(levelButton)
        if isOwner {
            backgroundView.addSubview(anchorButton)
        }
    }

    func activateConstraints() {
        backgroundView.snp.makeConstraints { make in
            make.leading.bottom.equalToSuperview()
            make.trailing.lessThanOrEqualToSuperview()
            make.top.equalToSuperview().offset(cellMargin)
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
        
        let height = barrageLabel.getHeight(maxWidth: barrageContentMaxWidth)
        barrageLabel.snp.makeConstraints { make in
            make.leading.equalTo(levelButton.snp.trailing).offset(5)
            make.trailing.equalToSuperview().inset(8)
            make.top.bottom.equalToSuperview().inset(4)
            make.height.equalTo(height)
        }
    }

    func setupDefaultCell(_ barrage: TUIBarrage, ownerId: String) {
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
        backgroundColor = .clear
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isViewReady = false

    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }

    func constructViewHierarchy() {
        addSubview(customView)
    }
    
    override func draw(_ rect: CGRect) {
        super.draw(rect)
        let height = customView.bounds.height
        layer.cornerRadius = height < 40 ? height * 0.5 : 13
    }

    func activateConstraints() {
        customView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(cellMargin)
            make.bottom.equalToSuperview()
            make.leading.equalToSuperview().offset(8)
            make.trailing.equalToSuperview().offset(-8)
        }
    }
}

private extension String {
    static let anchorText = localized("live.barrage.anchor")
}

private extension UILabel {
    func getHeight(maxWidth: CGFloat) -> CGFloat {
        let size = CGSize(width: maxWidth, height: CGFloat.greatestFiniteMagnitude)
        return sizeThatFits(size).height
    }
}
