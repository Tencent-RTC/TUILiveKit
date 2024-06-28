//
//  AudioEffectView.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/26.
//

import UIKit
import Combine
import RTCCommon
import RTCRoomEngine

class AudioEffectView: UIView {
    private let menuGenerator: AudioEffectMenuDateGenerator
    
    var backButtonClickClosure: ((UIButton)->Void)?
    private var isViewReady: Bool = false
    private let backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        return view
    }()
    
    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.contentMode = .center
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.textColor = .g7
        label.text = .audioEffectTitleText
        label.sizeToFit()
        return label
    }()
    
    private let tableView: UITableView = {
        let view = UITableView(frame: .zero, style: .grouped)
        view.register(SwitchCell.self, forCellReuseIdentifier: SwitchCell.identifier)
        view.register(ButtonCell.self, forCellReuseIdentifier: ButtonCell.identifier)
        view.register(SliderCell.self, forCellReuseIdentifier: SliderCell.identifier)
        view.register(CollectionViewCell.self, forCellReuseIdentifier: CollectionViewCell.identifier)
        view.separatorStyle = .none
        view.backgroundColor = .clear
        view.sectionFooterHeight = 0
        view.sectionHeaderHeight = 0
        view.showsVerticalScrollIndicator = false
        return view
    }()
    
    lazy var menus: [Int : [SettingItem]] = self.menuGenerator.audioEffectMenus
    lazy var titles: [Int: String] = self.menuGenerator.audioEffectSectionTitles
    
    init(roomEngine: TUIRoomEngine? = nil) {
        self.menuGenerator = AudioEffectStoreProvider(roomEngine: roomEngine)
        super.init(frame: .zero)
        backgroundColor = .clear
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        setupViewStyle()
        isViewReady = true
    }
    
    func constructViewHierarchy() {
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(tableView)
    }
    
    func activateConstraints() {
        backButton.snp.remakeConstraints { make in
            make.leading.equalToSuperview().inset(20)
            make.top.equalToSuperview().inset(20)
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
        }
        titleLabel.snp.remakeConstraints { make in
            make.centerY.equalTo(backButton)
            make.centerX.equalToSuperview()
            make.height.equalTo(24.scale375())
            make.width.equalTo(titleLabel.mm_w)
        }
        tableView.snp.remakeConstraints { make in
            make.bottom.equalToSuperview()
            make.trailing.equalToSuperview().offset(-16)
            make.leading.equalToSuperview().offset(16)
            make.top.equalTo(titleLabel.snp.bottom).offset(20)
            make.height.equalTo(height)
        }
    }
    
    func bindInteraction() {
        tableView.delegate = self
        tableView.dataSource = self
        backButton.addTarget(self, action: #selector(clickBack(sender:)), for: .touchUpInside)
    }
    
    func setupViewStyle() {
        backgroundColor = .g2
    }
    
    var height: CGFloat {
        return (self.window?.windowScene?.screen.bounds.height ?? 812)  * 0.75
    }
}

extension AudioEffectView: UITableViewDelegate {
    func tableView(_ tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat {
        return section == 0 ? 0 : 56
    }
    
    func tableView(_ tableView: UITableView, viewForHeaderInSection section: Int) -> UIView? {
        let label = UILabel(frame: .zero)
        label.textAlignment = .center
        label.font = .customFont(ofSize: 16)
        label.textColor = .whiteColor
        label.textAlignment = .left
        label.text = titles[section]
        return label
    }
    
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return indexPath.section < 2 ? 56.0 : 80.0
    }
    
    func numberOfSections(in tableView: UITableView) -> Int {
        return menus.count
    }
    
    func tableView(_ tableView: UITableView, heightForFooterInSection section: Int) -> CGFloat {
        return 0
    }
    
    func tableView(_ tableView: UITableView, viewForFooterInSection section: Int) -> UIView? {
        return nil
    }
}

extension AudioEffectView: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        if section < 2 {
            let values = menus[section]
            return values?.count ?? 0
        } else {
            return 1
        }
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let items = menus[indexPath.section] ?? []
        if indexPath.section < 2 {
            let item = items[indexPath.row]
            let identifier = item.cellType.cellIdentifier
            let cell = tableView.dequeueReusableCell(withIdentifier:identifier, for: indexPath)
            if let switchCell = cell as? SwitchCell {
                switchCell.title = item.title
                switchCell.update(item: item)
            } else if let sliderCell = cell as? SliderCell {
                sliderCell.title = item.title
                sliderCell.update(item: item)
            } else if let buttonCell = cell as? ButtonCell {
                buttonCell.title = item.title
                buttonCell.update(item: item)
            }
            return cell
        } else {
            let cell = tableView.dequeueReusableCell(withIdentifier: CollectionViewCell.identifier, for: indexPath)
            if let collectionCell = cell as? CollectionViewCell {
                collectionCell.update(items: items)
            }
            return cell
        }
    }
    
    func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
        guard indexPath.section < 2 else { return }
        let cornerRadius = CGSize(width: 12.0, height: 12.0)
        let bounds = cell.bounds
        let numberOfRows = tableView.numberOfRows(inSection: indexPath.section)
        var bezierPath: UIBezierPath = UIBezierPath(rect: bounds)
        if indexPath.row == 0 && numberOfRows == 1 {
            bezierPath = UIBezierPath(roundedRect: bounds, byRoundingCorners: .allCorners, cornerRadii: cornerRadius)
        } else if indexPath.row == 0 {
            bezierPath = UIBezierPath(roundedRect: bounds, byRoundingCorners: [.topLeft, .topRight], cornerRadii: cornerRadius)
        } else if indexPath.row == numberOfRows - 1 {
            bezierPath = UIBezierPath(roundedRect: bounds, byRoundingCorners: [.bottomLeft, .bottomRight], cornerRadii: cornerRadius)
        }
        cell.backgroundColor = .clear
        let layer = CAShapeLayer()
        layer.path = bezierPath.cgPath
        layer.fillColor = UIColor.g3.withAlphaComponent(0.3).cgColor
        
        let lineLayer = CALayer()
        let lineHeight = 1.0 / (window?.screen.scale ?? 1.0)
        lineLayer.frame = CGRect(x: 16.0, y: bounds.height - lineHeight, width: bounds.width - 32.0, height: lineHeight)
        lineLayer.backgroundColor = UIColor.g6.withAlphaComponent(0.2).cgColor
        if indexPath.row != numberOfRows - 1 {
            layer.addSublayer(lineLayer)
        }
        
        cell.layer.insertSublayer(layer, at: 0)
    }
    
}

extension AudioEffectView {
    @objc
    func clickBack(sender: UIButton) {
        self.backButtonClickClosure?(sender)
    }
}
