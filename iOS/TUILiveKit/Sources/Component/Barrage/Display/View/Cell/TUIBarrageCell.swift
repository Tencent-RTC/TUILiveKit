//
// TUIBarrageCell.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

import RTCCommon
import SnapKit
import UIKit
import AtomicXCore

fileprivate let cellMargin: CGFloat = 6.scale375Height()
fileprivate let barrageContentMaxWidth: CGFloat = 240.scale375Width()

class BarrageCell: UITableViewCell {
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

    func setContent(_ barrage: Barrage, ownerId: String) {
        if let cell = contentCell {
            cell.safeRemoveFromSuperview()
        }
        let cell = BarrageDefaultCell(barrage: barrage, ownerId: ownerId)
        contentView.addSubview(cell)
        contentCell = cell
        cell.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }

    func setContent(_ view: UIView) {
        if let cell = contentCell {
            cell.safeRemoveFromSuperview()
        }
        let cell = BarrageCustomCell(customView: view)
        contentView.addSubview(cell)
        contentCell = cell
        cell.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
}

class BarrageDefaultCell: UIView {
    private let barrage: Barrage
    private let ownerId: String
    private var isOwner: Bool {
        barrage.sender.userId == ownerId
    }

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
        label.numberOfLines = 0
        label.textAlignment = .left
        label.lineBreakMode = .byCharWrapping
        label.textColor = .white
        return label
    }()
    
    private lazy var backgroundView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .black.withAlphaComponent(0.25)
        view.layer.cornerRadius = 13
        view.clipsToBounds = true
        return view
    }()

    init(barrage: Barrage, ownerId: String) {
        self.barrage = barrage
        self.ownerId = ownerId
        super.init(frame: .zero)
        backgroundColor = .clear
        barrageLabel.attributedText = getBarrageLabelAttributedText(barrage: barrage)
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
        if isOwner {
            anchorButton.snp.makeConstraints { make in
                make.leading.equalToSuperview().offset(5)
                make.top.equalToSuperview().offset(6)
                make.height.equalTo(14)
                make.width.equalTo(42)
            }
        }
        
        barrageLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(5)
            make.trailing.equalToSuperview().inset(8)
            make.top.bottom.equalToSuperview().inset(4)
            make.width.lessThanOrEqualTo(barrageContentMaxWidth)
        }
    }

    func getBarrageLabelAttributedText(barrage: Barrage) -> NSMutableAttributedString {
        let placeholderString = String(repeating: " ", count: isOwner ? 12 : 0)
        let userName = barrage.sender.userName ?? ""
        let displayName = (userName.isEmpty ? barrage.sender.userId : userName) + "："
        let userNameAttributes: [NSAttributedString.Key: Any] =
        [.foregroundColor: UIColor.lightBlueColor, .font: UIFont.customFont(ofSize: 12, weight: .semibold)]
        let userNameAttributedText = NSMutableAttributedString(string: "\(placeholderString)\(displayName)",
                                                               attributes: userNameAttributes)
        
        userNameAttributedText.append(getBarrageContentAttributedText(content: barrage.textContent))
        return userNameAttributedText
    }
    
    func getBarrageContentAttributedText(content: String)
        -> NSMutableAttributedString {
        return EmotionHelper.shared.obtainImagesAttributedString(byText: content,
                                                                 font: UIFont.customFont(ofSize: 12, weight: .semibold))
    }
}

class BarrageCustomCell: UIView {
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
            make.leading.equalToSuperview()
            make.trailing.equalToSuperview()
        }
    }
}

private extension String {
    static let anchorText = internalLocalized("Anchor")
}
