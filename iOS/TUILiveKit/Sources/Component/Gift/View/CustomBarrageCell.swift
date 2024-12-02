//
//  CustomBarrageCell.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/8.
//

import UIKit
import SnapKit
import TUICore
import Kingfisher

class CustomBarrageCell: UIView {
    
    private lazy var containerView: UIView = {
        let view = UIView()
        view.backgroundColor = .clear
        return view
    }()
    
    private let barrageLabel: UILabel = {
        let label = UILabel()
        label.bounds.size.height = 20.scale375Height()
        return label
    }()
    
    private let giftImageView: UIImageView = {
        let view = UIImageView()
        view.contentMode = .scaleAspectFit
        return view
    }()
    
    private let countLabel: UILabel = {
        let label = UILabel()
        label.bounds.size.height = 20.scale375Height()
        return label
    }()
    
    private var cellHeight = 22.scale375Height()
    
    init(barrage: TUIBarrage) {
        super.init(frame: .zero)
        backgroundColor = UIColor(hex: "#0F1014")?.withAlphaComponent(0.4)
        layer.cornerRadius = 13.scale375Height()
        layer.masksToBounds = true
        updateViewContent(barrage: barrage)
        updateCellSize()
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
    
    private func updateViewContent(barrage: TUIBarrage) {
        let barrageAttributedText = getBarrageAttributedText(barrage: barrage)
        barrageLabel.attributedText = barrageAttributedText
        
        
        if let giftIconUrlString = barrage.extInfo["gift_icon_url"]?.value as? String,
           let giftIconUrl = URL(string: giftIconUrlString) {
            giftImageView.kf.setImage(with: giftIconUrl)
        }
        
        let countAttributedText = getCountAttributedText(barrage: barrage)
        countLabel.attributedText = countAttributedText
    }
    
    private func getBarrageAttributedText(barrage: TUIBarrage) -> NSMutableAttributedString {
        let userName = barrage.user.userName
        let userNameAttributes: [NSAttributedString.Key: Any] =
            [.foregroundColor: UIColor.lightBlueColor, .font: UIFont.systemFont(ofSize: 12)]
        let mutableAttributedString = NSMutableAttributedString(string: userName, attributes: userNameAttributes)
        
        let sendAttributedText = NSAttributedString(string: " " + .sendText, attributes: [.foregroundColor: UIColor.white])
        mutableAttributedString.append(sendAttributedText)
        
        let colors: [UIColor] = [.red, .blue, .yellow]
        guard let giftName = barrage.extInfo["gift_name"]?.value as? String,
              let receiver = barrage.extInfo["gift_receiver_username"]?.value as? String else {
            return NSMutableAttributedString()
        }
        
        let receiverAttributedText = NSAttributedString(string: receiver, attributes: [.foregroundColor: UIColor.lightBlueColor])
        mutableAttributedString.append(receiverAttributedText)
        
        let random = Int(arc4random_uniform(UInt32(colors.count)))
        let giftNameAttributedString = NSAttributedString(string: " " + giftName, attributes: [.foregroundColor: colors[random]])
        mutableAttributedString.append(giftNameAttributedString)

        mutableAttributedString.addAttribute(.font,
                                             value: UIFont.systemFont(ofSize: 12),
                                             range: NSRange(location: 0, length: mutableAttributedString.length))
        return mutableAttributedString
    }
    
    private func getCountAttributedText(barrage: TUIBarrage ) -> NSAttributedString {
        let giftCount = barrage.extInfo["gift_count"]?.value as? Int ?? 0
        return NSAttributedString(string: " x\(giftCount)", attributes: [.foregroundColor: UIColor.white])
    }
    
    private func updateCellSize() {
        barrageLabel.sizeToFit()
        countLabel.sizeToFit()
        let barrageLabelWidth = barrageLabel.bounds.width
        let countLabelWidth = countLabel.bounds.width
        
        let margin = 28.0
        mm_w = barrageLabelWidth + countLabelWidth + giftImageView.bounds.width + margin
        mm_h = cellHeight
    }
}


// MARK: - Layout
extension CustomBarrageCell {
    private func constructViewHierarchy() {
        addSubview(containerView)
        containerView.addSubview(barrageLabel)
        containerView.addSubview(giftImageView)
        containerView.addSubview(countLabel)
    }
    
    private func activateConstraints() {
        if self.superview != nil {
            snp.makeConstraints { make in
                make.leading.equalToSuperview()
            }
        }
        containerView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        barrageLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(4)
            make.centerY.equalToSuperview()
        }
        giftImageView.snp.makeConstraints { make in
            make.leading.equalTo(barrageLabel.snp.trailing).offset(8)
            make.centerY.equalToSuperview()
            make.width.height.equalTo(12)
        }
        countLabel.snp.makeConstraints { make in
            make.leading.equalTo(giftImageView.snp.trailing)
            make.centerY.equalToSuperview()
        }
    }
}


private extension String {
    static let sendText = localized("live.customBarrageCell.send")
}
