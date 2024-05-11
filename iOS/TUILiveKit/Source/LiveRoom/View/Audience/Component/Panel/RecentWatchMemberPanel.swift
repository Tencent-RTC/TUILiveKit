//
//  RecentWatchMemberPanel.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/25.
//

import Foundation

class RecentWatchMemberPanel: UIView {
    private var isPortrait: Bool = {
        return WindowUtils.isPortrait
    }()

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        updateView()
    }
  
    private var popupAction: Observable<PopupPanelAction>?
    private var engineService: RoomEngineService
    private var liveRoomInfo:LiveRoomInfo {
          engineService.liveRoomInfo
    }
    private var listUser:[UserInfo] = []
    init(engineService: RoomEngineService) {
        self.engineService = engineService
        super.init(frame: .zero)
        self.engineService.getUserList()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private lazy var backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        view.addTarget(self, action: #selector(backButtonClick), for: .touchUpInside)
        return view
    }()

    private lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textAlignment = .center
        label.font = .customFont(ofSize: 16,weight: .medium)
        label.textColor = .g7
        label.text = .localized(.onlineAudience)
        liveRoomInfo.audienceCount.addObserver(self) { [weak self] _, _ in
            guard let self = self else{ return}
            self.titleLabel.text = .localized(.onlineAudience)
        }
        return label
    }()
    
    private lazy var userListTableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .plain)
        tableView.backgroundColor = .clear
        tableView.separatorStyle = .none
        tableView.delegate = self
        tableView.dataSource = self
        tableView.register(UserMemberCell.self, forCellReuseIdentifier: UserMemberCell.cellReuseIdentifier)
        liveRoomInfo.audienceList.addObserver(self) { [weak self] _, _ in
            self?.updateView()
        }
        return tableView
    }()

    private func updateView() {
        listUser = liveRoomInfo.audienceList.value.filter({ $0.userId != self.liveRoomInfo.anchorInfo.value.userId })
        if engineService.liveRoomInfo.selfInfo.status.value == .none
            && engineService.liveRoomInfo.selfInfo.role.value == .anchor {
            listUser = listUser.filter({ $0.userId != engineService.liveRoomInfo.selfInfo.userId })
        }
        userListTableView.reloadData()
    }
}

// MARK: Layout

extension RecentWatchMemberPanel {
    func constructViewHierarchy() {
        backgroundColor = .g2
        self.layer.cornerRadius = 16
        self.layer.masksToBounds = true
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(userListTableView)
    }

    func activateConstraints() {
        self.snp.remakeConstraints { make in
            if isPortrait {
                make.width.equalToSuperview()
                make.height.equalTo(718.scale375Height())
                make.centerX.equalToSuperview()
            } else {
                make.width.equalTo(375)
                make.height.equalToSuperview()
                make.trailing.equalToSuperview()
            }
            make.bottom.equalToSuperview()
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
        }
        
        userListTableView.snp.remakeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.top.equalTo(backButton.snp.bottom).offset(32)
        }
    }
}

// MARK: Action

extension RecentWatchMemberPanel {
    @objc func backButtonClick() {
        self.popupAction?.value = .close
    }

}

extension RecentWatchMemberPanel: UITableViewDataSource {
    internal func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return listUser.count
    }
}

extension RecentWatchMemberPanel: UITableViewDelegate {
    internal func tableView(_ tableView: UITableView,
                            cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        
        let cell = tableView.dequeueReusableCell(withIdentifier: UserMemberCell.cellReuseIdentifier, for: indexPath)
        if let cell = cell as? UserMemberCell,
            indexPath.row < listUser.count {
            cell.userInfo = listUser[indexPath.row]
        }
        return cell
    }
    
    internal func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        
    }
    
    internal func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 50.scale375()
    }
}

extension RecentWatchMemberPanel:PopupPanelSubViewProtocol {
    
    func setAction(_ popupAction: Observable<PopupPanelAction>) {
        self.popupAction = popupAction
    }
    
    func updateRootViewOrientation(isPortrait: Bool) {
        self.isPortrait = isPortrait
        activateConstraints()
    }
}

fileprivate extension String {
    static let onlineAudience = localized("live.recent.online.audience")
}

