//
//  AudienceListPanelView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/25.
//

import Foundation
import Combine
import RTCCommon
import RTCRoomEngine

class AudienceListPanelView: UIView {
    var onBackButtonClickedClosure: (() -> Void)?
    private let state: AudienceListState
    private var cancellableSet = Set<AnyCancellable>()
    private var isPortrait: Bool = {
        return WindowUtils.isPortrait
    }()
    private var listUser:[TUIUserInfo] = []
    
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
        return label
    }()
    
    private lazy var userListTableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .plain)
        tableView.backgroundColor = .clear
        tableView.separatorStyle = .none
        tableView.delegate = self
        tableView.dataSource = self
        tableView.register(UserMemberCell.self, forCellReuseIdentifier: UserMemberCell.cellReuseIdentifier)
        return tableView
    }()
    
    init(state: AudienceListState) {
        self.state = state
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        subscribe()
        isViewReady = true
    }
  
    private func subscribe() {
        state.$audienceList
            .receive(on: RunLoop.main)
            .sink { [weak self] audienceList in
                guard let self = self else { return }
                self.listUser = audienceList
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
            make.height.equalTo(593.scale375Height())
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
        
        let cell = tableView.dequeueReusableCell(withIdentifier: UserMemberCell.cellReuseIdentifier, for: indexPath)
        if let cell = cell as? UserMemberCell,
            indexPath.row < listUser.count {
            cell.user = listUser[indexPath.row]
        }
        return cell
    }
    
    internal func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 50.scale375()
    }
}

fileprivate extension String {
    static let onlineAudience = localized("live.recent.online.audience")
}

