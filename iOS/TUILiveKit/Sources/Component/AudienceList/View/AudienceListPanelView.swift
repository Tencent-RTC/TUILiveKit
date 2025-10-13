//
//  AudienceListPanelView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/25.
//

import Foundation
import Combine
import RTCCommon
import AtomicXCore

class AudienceListPanelView: UIView {
    var onBackButtonClickedClosure: (() -> Void)?
    var onUserManageButtonClicked: ((LiveUserInfo) -> Void)?
    
    private let liveId: String
    private var store: LiveAudienceStore {
        LiveAudienceStore.create(liveID: liveId)
    }
    private var liveListStore: LiveListStore {
        LiveListStore.shared
    }
    private var cancellableSet = Set<AnyCancellable>()
    private var isPortrait: Bool = {
        return WindowUtils.isPortrait
    }()
    private var listUser:[LiveUserInfo] = []
    
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
        label.text = .onlineAudience
        return label
    }()
    
    private lazy var userListTableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .plain)
        tableView.backgroundColor = .clear
        tableView.separatorStyle = .none
        tableView.delegate = self
        tableView.dataSource = self
        tableView.register(AudienceMemberCell.self, forCellReuseIdentifier: AudienceMemberCell.cellReuseIdentifier)
        return tableView
    }()
    
    init(liveId: String, frame: CGRect = .zero) {
        self.liveId = liveId
        super.init(frame: frame)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        subscribe()
        isViewReady = true
    }
  
    private func subscribe() {
        store.state.subscribe(StatePublisherSelector(keyPath: \LiveAudienceState.audienceList))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] audienceList in
                guard let self = self else { return }
                let currentLive = liveListStore.state.value.currentLive
                guard !currentLive.isEmpty else { return }
                self.listUser = audienceList.filter {$0.userID != currentLive.liveOwner.userID }
                self.userListTableView.reloadData()
            }
            .store(in: &cancellableSet)
    }
}

// MARK: Layout

extension AudienceListPanelView {
    func constructViewHierarchy() {
        backgroundColor = .g2
        self.layer.cornerRadius = 16
        self.layer.masksToBounds = true
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(userListTableView)
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
        }
        
        userListTableView.snp.remakeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.top.equalTo(backButton.snp.bottom).offset(32)
            make.height.equalTo(screenHeight * 2 / 3)
        }
    }
}

// MARK: Action

extension AudienceListPanelView {
    @objc func backButtonClick(sender: UIButton) {
        onBackButtonClickedClosure?()
    }
}

extension AudienceListPanelView: UITableViewDataSource {
    internal func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return listUser.count
    }
}

extension AudienceListPanelView: UITableViewDelegate {
    internal func tableView(_ tableView: UITableView,
                            cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        
        let cell = tableView.dequeueReusableCell(withIdentifier: AudienceMemberCell.cellReuseIdentifier, for: indexPath)
        if let cell = cell as? AudienceMemberCell,
            indexPath.row < listUser.count {
            cell.user = listUser[indexPath.row]
            cell.onUserManageButtonClicked = onUserManageButtonClicked
        }
        return cell
    }
    
    internal func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 50.scale375()
    }
}

fileprivate extension String {
    static let onlineAudience = internalLocalized("Online audience")
}

