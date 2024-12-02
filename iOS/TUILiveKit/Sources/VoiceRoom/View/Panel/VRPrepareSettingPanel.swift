//
//  VRPrepareSettingPanel.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/7/11.
//

import UIKit
import Combine
import RTCCommon

class VRPrepareSettingPanel: UIView {
    private let manager: VoiceRoomManager
    private let routerManager: VRRouterManager
    private var cancellableSet = Set<AnyCancellable>()
    private lazy var menus: [SwitchItem] = {
        var item = SwitchItem(title: .needRequestText, isOn: manager.roomState.seatMode == .applyToTake)
        item.action = { [weak self] isNeedToApply in
            guard let self = self else { return }
            manager.update(seatMode: isNeedToApply ? .applyToTake : .freeToTake)
        }
        let menus = [item]
        return menus
    }()
    
    private let isPortrait: Bool = {
        WindowUtils.isPortrait
    }()
    
    private lazy var backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        view.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return view
    }()
    
    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.contentMode = .center
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.textColor = .g7
        label.text = .settingText
        label.sizeToFit()
        return label
    }()
    
    private lazy var settingTableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .grouped)
        tableView.separatorStyle = .none
        tableView.delegate = self
        tableView.dataSource = self
        tableView.backgroundColor = .clear
        tableView.register(SwitchCell.self, forCellReuseIdentifier: SwitchCell.identifier)
        return tableView
    }()
    
    init(manager: VoiceRoomManager, routerManager: VRRouterManager) {
        self.manager = manager
        self.routerManager = routerManager
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
    }
    
}

extension VRPrepareSettingPanel {
    func constructViewHierarchy() {
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.masksToBounds = true
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(settingTableView)
    }
    
    func activateConstraints() {
        snp.remakeConstraints { make in
            if isPortrait {
                make.height.equalTo(352.scale375Height())
            } else {
                make.width.equalTo(375.scale375())
            }
            make.edges.equalToSuperview()
        }
        
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
        
        settingTableView.snp.remakeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.top.equalTo(titleLabel.snp.bottom).offset(20)
        }
    }
}

extension VRPrepareSettingPanel {
    @objc func backButtonClick(sender: UIButton) {
        routerManager.router(action: .dismiss())
    }
}

extension VRPrepareSettingPanel: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return menus.count
    }
}

extension VRPrepareSettingPanel: UITableViewDelegate {
    func tableView(_ tableView: UITableView,
                   cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: SwitchCell.identifier, for: indexPath)
        let item = menus[indexPath.row]
        if let switchCell = cell as? SwitchCell {
            switchCell.title = item.title
            switchCell.update(item: item)
        }
        return cell
    }
}

fileprivate extension String {
    static let settingText: String = localized("live.anchor.setting.title")
    static let needRequestText: String = localized("live.anchor.setting.need.request")
}
