//
//  LiveRoomBeautyAlertView.swift
//  TXLiteAVDemo
//
//  Created by gg on 2021/5/7.
//  Copyright © 2021 Tencent. All rights reserved.
//

import Foundation
import TUIKitCommon

// MARK: Base
public class LiveRoomAlertContentView: UIView {
    public lazy var bgView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .black
        view.alpha = 0.6
        return view
    }()
    
    public lazy var contentView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .white
        return view
    }()
    
    public lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .black
        label.font = UIFont(name: "PingFangSC-Medium", size: 24)
        return label
    }()
    
    public var willDismiss: (()->())?
    public var didDismiss: (()->())?
    
    public override init(frame: CGRect = .zero) {
        super.init(frame: frame)
        contentView.transform = CGAffineTransform(translationX: 0, y: ScreenHeight)
        alpha = 0
    }
    
    public required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady = false
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
    }
    
    public func show() {
        UIView.animate(withDuration: 0.3) {
            self.alpha = 1
            self.contentView.transform = .identity
        }
    }
    
    public func dismiss() {
        if let action = willDismiss {
            action()
        }
        UIView.animate(withDuration: 0.3) {
            self.alpha = 0
            self.contentView.transform = CGAffineTransform(translationX: 0, y: ScreenHeight)
        } completion: { (finish) in
            if let action = self.didDismiss {
                action()
            }
            self.removeFromSuperview()
        }
    }
    
    public override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        guard let point = touches.first?.location(in: self) else {
            return
        }
        if !contentView.frame.contains(point) {
            dismiss()
        }
    }
    
    public override func draw(_ rect: CGRect) {
        super.draw(rect)
        contentView.roundedRect(rect: contentView.bounds, byRoundingCorners: [.topLeft, .topRight], cornerRadii: CGSize(width: 12, height: 12))
    }
    
    public func constructViewHierarchy() {
        addSubview(bgView)
        addSubview(contentView)
        contentView.addSubview(titleLabel)
    }
    
    public func activateConstraints() {
        bgView.snp.makeConstraints { (make) in
            make.edges.equalToSuperview()
        }
        contentView.snp.makeConstraints { (make) in
            make.leading.trailing.bottom.equalToSuperview()
        }
        titleLabel.snp.makeConstraints { (make) in
            make.leading.equalToSuperview().offset(20)
            make.top.equalToSuperview().offset(32)
        }
    }
    
    public func bindInteraction() {
        
    }
}

// MARK: Resolution
class TRTCLiveRoomResolutionAlert: LiveRoomAlertContentView {
    
    var dataSource: [TRTCLiveRoomBitrateTable] = []
    var selectIndex = 3
    
    var didSelectItem: ((_ index: Int)->())?
    
    lazy var tableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .plain)
        tableView.separatorStyle = .none
        tableView.backgroundColor = .clear
        return tableView
    }()
    
    override init(frame: CGRect = .zero) {
        super.init(frame: frame)
        titleLabel.text = .resolutionTitleText
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func constructViewHierarchy() {
        super.constructViewHierarchy()
        contentView.addSubview(tableView)
    }
    
    override func activateConstraints() {
        super.activateConstraints()
        tableView.snp.makeConstraints { (make) in
            make.top.equalTo(titleLabel.snp.bottom).offset(20)
            make.leading.trailing.bottom.equalToSuperview()
            make.height.equalTo(248)
        }
    }
    
    override func bindInteraction() {
        super.bindInteraction()
        tableView.dataSource = self
        tableView.delegate = self
        tableView.register(TRTCMeetingResolutionTableViewCell.self, forCellReuseIdentifier: "TRTCMeetingResolutionTableViewCell")
    }
}

extension TRTCLiveRoomResolutionAlert: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return dataSource.count
    }
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "TRTCMeetingResolutionTableViewCell", for: indexPath)
        if let scell = cell as? TRTCMeetingResolutionTableViewCell {
            let model = dataSource[indexPath.row]
            scell.titleLabel.text = model.resolutionName
            scell.isSelected = indexPath.row == selectIndex
        }
        return cell
    }
}
extension TRTCLiveRoomResolutionAlert: UITableViewDelegate {
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        selectIndex = indexPath.row
        tableView.reloadSections(IndexSet(integer: 0), with: .none)
        if let action = didSelectItem {
            action(selectIndex)
        }
    }
}

class TRTCMeetingResolutionTableViewCell: UITableViewCell {
    
    lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = UIFont(name: "PingFangSC-Medium", size: 16)
        label.textColor = UIColor(hex: "666666")
        return label
    }()
    
    lazy var checkboxImageView: UIImageView = {
        let imageView = UIImageView(image: norImage)
        return imageView
    }()
    
    let selImage = UIImage.init(named: "checkbox_sel", in: liveRoomBundle(), compatibleWith: nil)
    let norImage = UIImage.init(named: "checkbox_nor", in: liveRoomBundle(), compatibleWith: nil)
    
    override var isSelected: Bool {
        didSet {
            checkboxImageView.image = isSelected ? selImage : norImage
        }
    }
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        
        backgroundColor = .clear
        selectionStyle = .none
        
        contentView.addSubview(titleLabel)
        titleLabel.snp.makeConstraints { (make) in
            make.centerY.equalToSuperview()
            make.leading.equalToSuperview().offset(20)
        }
        
        contentView.addSubview(checkboxImageView)
        checkboxImageView.snp.makeConstraints { (make) in
            make.centerY.equalToSuperview()
            make.trailing.equalToSuperview().offset(-20)
            make.size.equalTo(CGSize(width: 24, height: 24))
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
}

extension String {
    public func width(fromFont: UIFont) -> CGFloat {
        if count == 0 {
            return 0
        }
        let str = self as NSString
        return str.boundingRect(with: CGSize(width: 0, height: fromFont.lineHeight), options:
         [.usesLineFragmentOrigin, .usesFontLeading, .usesDeviceMetrics], attributes:
         [NSAttributedString.Key.font : fromFont], context: nil).width
    }
}

/// MARK: - internationalization string
fileprivate extension String {
    static var titleText: String {
        liveRoomLocalize("TC.BeautySettingPanel.Setup")
    }
    static var strengthText: String {
        liveRoomLocalize("TC.BeautySettingPanel.Strength")
    }
    static var resolutionTitleText: String {
        liveRoomLocalize("Demo.TRTC.LiveRoom.resolution")
    }
}
