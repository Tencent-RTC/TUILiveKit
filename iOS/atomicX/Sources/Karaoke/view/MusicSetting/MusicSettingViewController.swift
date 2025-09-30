//
//  MusicSettingView.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/26.
//

import UIKit
import Combine
import RTCCommon
#if canImport(TXLiteAVSDK_TRTC)
    import TXLiteAVSDK_TRTC
#elseif canImport(TXLiteAVSDK_Professional)
    import TXLiteAVSDK_Professional
#endif

public class MusicSettingViewController: UIViewController {
    private lazy var manager : MusicSettingManager = {
        return MusicSettingManager(karaokeManager: self.karaokeManager ?? KaraokeManager(roomId: ""))
    }()
    private var cancellables = Set<AnyCancellable>()
    private weak var karaokeManager: KaraokeManager?
    private var isViewReady: Bool = false

    private let contentView =  UIView()

    private let doneButton: UIButton = {
        let button = UIButton()
        button.setTitle(.doneText, for: .normal)
        button.setTitleColor(.b1, for: .normal)
        button.titleLabel?.font = UIFont(name: "PingFangSC-Semibold", size: 16)
        return button
    }()
    
    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.contentMode = .center
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.textColor = .g7
        label.text = .MusicSettingTitle
        label.sizeToFit()
        return label
    }()
    
    private lazy var tableView: UITableView = {
        let view = UITableView(frame: .zero, style: .grouped)
        view.register(MusicSwitchCell.self, forCellReuseIdentifier: MusicSwitchCell.identifier)
        view.register(MusicButtonCell.self, forCellReuseIdentifier: MusicButtonCell.identifier)
        view.register(MusicSliderCell.self, forCellReuseIdentifier: MusicSliderCell.identifier)
        view.register(MusicCollectionViewCell.self, forCellReuseIdentifier: MusicCollectionViewCell.identifier)
        view.separatorStyle = .none
        view.backgroundColor = .clear
        view.sectionFooterHeight = 0
        view.sectionHeaderHeight = 0
        view.showsVerticalScrollIndicator = false
        return view
    }()
    
    private lazy var menus: [Int : [MusicSettingItem]] = manager.MusicSettingMenus
    private lazy var titles: [Int: String] = manager.MusicSettingSectionTitles
    
    init(karaokeManager: KaraokeManager) {
        self.karaokeManager = karaokeManager
        super.init(nibName: nil, bundle: nil)
        view.backgroundColor = .clear
        view.layer.cornerRadius = 16
        view.layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        setupViewStyle()
        setupStateSubscriptions()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
}

extension MusicSettingViewController {

    func constructViewHierarchy() {
        view.addSubview(contentView)
        contentView.addSubview(doneButton)
        contentView.addSubview(titleLabel)
        contentView.addSubview(tableView)
    }
    
    func activateConstraints() {
        contentView.snp.remakeConstraints { make in
            make.bottom.equalToSuperview()
            make.height.equalTo(500.scale375())
            make.width.equalTo(375.scale375())
        }
        doneButton.snp.remakeConstraints { make in
            make.trailing.equalToSuperview().inset(20)
            make.top.equalToSuperview().inset(20)
            make.height.equalTo(24.scale375())
        }
        titleLabel.snp.remakeConstraints { make in
            make.centerY.equalTo(doneButton)
            make.centerX.equalToSuperview()
            make.height.equalTo(24.scale375())
            make.width.equalTo(titleLabel.mm_w)
        }
        tableView.snp.remakeConstraints { make in
            make.bottom.equalToSuperview()
            make.trailing.equalToSuperview().offset(-16)
            make.leading.equalToSuperview().offset(16)
            make.top.equalTo(titleLabel.snp.bottom).offset(10)
        }
    }
    
    func bindInteraction() {
        tableView.delegate = self
        tableView.dataSource = self
        doneButton.addTarget(self, action: #selector(clickBack(sender:)), for: .touchUpInside)

        let tap = UITapGestureRecognizer(target: self, action: #selector(onBackButtonClickedClosure(_:)))
        tap.delegate = self
        self.view.addGestureRecognizer(tap)
    }

    func setupStateSubscriptions() {
        karaokeManager?.kickedOutSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] in
                guard let self = self else { return }
                guard let vc = WindowUtils.getCurrentWindowViewController() else {return }
                vc.dismiss(animated: true)
            }
            .store(in: &cancellables)
    }

    func setupViewStyle() {
        view.backgroundColor = .clear
        contentView.backgroundColor = .g2
    }
}

extension MusicSettingViewController: UITableViewDelegate {
    public func tableView(_ tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat {
        return section == 0 ? 0 : 56
    }
    
    public func tableView(_ tableView: UITableView, viewForHeaderInSection section: Int) -> UIView? {
        let label = UILabel(frame: .zero)
        label.textAlignment = .center
        label.font = .customFont(ofSize: 16)
        label.textColor = .whiteColor
        label.textAlignment = .left
        label.text = titles[section]
        return label
    }
    
    public func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return indexPath.section < 2 ? 56.0 : 80.0
    }
    
    public func numberOfSections(in tableView: UITableView) -> Int {
        return menus.count
    }
    
    public func tableView(_ tableView: UITableView, heightForFooterInSection section: Int) -> CGFloat {
        return 0
    }
    
    public func tableView(_ tableView: UITableView, viewForFooterInSection section: Int) -> UIView? {
        return nil
    }
}

extension MusicSettingViewController: UITableViewDataSource {
    public func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        if section < 2 {
            let values = menus[section]
            return values?.count ?? 0
        } else {
            return 1
        }
    }
    
    public func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let items = menus[indexPath.section] ?? []
        if indexPath.section < 2 {
            let item = items[indexPath.row]
            let identifier = item.cellType.cellIdentifier
            let cell = tableView.dequeueReusableCell(withIdentifier:identifier, for: indexPath)
            if let switchCell = cell as? MusicSwitchCell {
                switchCell.title = item.title
                switchCell.update(item: item)
            } else if let sliderCell = cell as? MusicSliderCell {
                sliderCell.title = item.title
                sliderCell.update(item: item)
            } else if let buttonCell = cell as? MusicButtonCell {
                buttonCell.title = item.title
                buttonCell.update(item: item)
            }
            return cell
        } else {
            let cell = tableView.dequeueReusableCell(withIdentifier: MusicCollectionViewCell.identifier, for: indexPath)
            if let collectionCell = cell as? MusicCollectionViewCell {
                collectionCell.update(items: items)
            }
            return cell
        }
    }
    
    public func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
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
        let lineHeight = 1.0
        lineLayer.frame = CGRect(x: 16.0, y: bounds.height - lineHeight, width: bounds.width - 32.0, height: lineHeight)
        lineLayer.backgroundColor = UIColor.g6.withAlphaComponent(0.2).cgColor
        if indexPath.row != numberOfRows - 1 {
            layer.addSublayer(lineLayer)
        }
        
        cell.layer.insertSublayer(layer, at: 0)
    }
    
}

extension MusicSettingViewController {
    @objc func clickBack(sender: UIButton) {
        guard let vc = WindowUtils.getCurrentWindowViewController() else { return }
        vc.dismiss(animated: true)
    }

    @objc func onBackButtonClickedClosure(_ sender: UITapGestureRecognizer) {
        let point = sender.location(in: view)
        guard let vc = WindowUtils.getCurrentWindowViewController() , !contentView.frame.contains(point) else {return }
        vc.dismiss(animated: true)
    }

}

extension MusicSettingViewController: UIGestureRecognizerDelegate {
    public func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer, shouldReceive touch: UITouch) -> Bool {
        let point = touch.location(in: self.view)
        return !contentView.frame.contains(point)
    }
}

fileprivate extension String {
    static let MusicSettingTitle: String = ("Tune").localized
    static let doneText: String = ("Done").localized
}
