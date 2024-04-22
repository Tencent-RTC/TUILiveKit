//
//  UserListMenuView.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/21.
//

import UIKit
import Combine

private extension String {
    static var anchorLinkControlTitle: String {
        localized("live.anchor.link.control.title")
    }
    
    static var anchorLinkControlDesc: String {
        localized("live.anchor.link.control.desc")
    }
    
    static var anchorLinkControlSeatCount: String {
        localized("live.anchor.link.control.seat.count.xxx")
    }
    
    static var anchorLinkControlRequestCount: String {
        localized("live.anchor.link.control.request.count.xxx")
    }
    
}

class SeatApplicationListView: UIView {
    
    @Injected var store: VoiceRoomStoreProvider
    
    lazy var applicationCount = self.store.select(SeatSelectors.getSeatApplicationCount)
    lazy var userMap = self.store.select(SeatSelectors.getApplicationUserMap)
    
    private var cancellables = Set<AnyCancellable>()
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
        label.text = .anchorLinkControlTitle
        label.sizeToFit()
        return label
    }()
    
    private lazy var contentTableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .plain)
        tableView.separatorStyle = .none
        tableView.delegate = self
        tableView.dataSource = self
        tableView.backgroundColor = .clear
        tableView.register(SeatApplicationCell.self, forCellReuseIdentifier: SeatApplicationCell.identifier)
        tableView.register(SwitchCell.self, forCellReuseIdentifier: SwitchCell.identifier)
        tableView.contentInsetAdjustmentBehavior = .never
        return tableView
    }()
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInterAction()
        isViewReady = true
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
}

extension SeatApplicationListView {
    private func constructViewHierarchy() {
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(contentTableView)
    }
    
    private func activateConstraints() {
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
        contentTableView.snp.makeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.top.equalTo(titleLabel.snp.bottom).offset(20)
            make.height.equalTo(height)
        }
    }
    
    private func bindInterAction() {
        backButton.addTarget(self, action: #selector(clickBack(sender:)), for: .touchUpInside)
        applicationCount
            .receive(on: RunLoop.main)
            .sink { [weak self] _ in
                guard let self = self else { return }
                self.contentTableView.reloadData()
            }
            .store(in: &cancellables)
    }
    
    private func setupStyle() {
        backgroundColor = .g2
    }
    
    var height: CGFloat {
        return (self.window?.windowScene?.screen.bounds.height ?? 812)  * 0.8
    }
}

extension SeatApplicationListView: UITableViewDelegate {
    func tableView(_ tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat {
        return 0
    }
    
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        if indexPath.section == 0 {
            return 40
        } else {
            return 60
        }
    }
    
    func numberOfSections(in tableView: UITableView) -> Int {
        return 2
    }
}

extension SeatApplicationListView: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        if section == 0 {
            return 0
        } else {
            return store.selectCurrent(SeatSelectors.getSeatApplicationCount)
        }
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let identifier = indexPath.section == 0 ? SwitchCell.identifier : SeatApplicationCell.identifier
        let cell = tableView.dequeueReusableCell(withIdentifier:identifier, for: indexPath)
        if indexPath.section == 0 {
            if let configCell = cell as? SwitchCell {
                // TODO: Add configuration Cell
            }
        } else {
            if let seatApplicationCell = cell as? SeatApplicationCell {
                bindStateTo(seatApplicationCell: seatApplicationCell, at: indexPath.row)
            }
        }
        return cell
    }
}

extension SeatApplicationListView {
    private func bindStateTo(seatApplicationCell: SeatApplicationCell, at index: Int) {
        let application = store.selectCurrent(SeatSelectors.getSeatApplications)[index]
        seatApplicationCell.acceptAction = { [weak self] in
            guard let self = self else { return }
            self.store.dispatch(action: SeatActions.responseSeatApplication(payload: (true, application.id)))
            self.store.dispatch(action: SeatActions.removeSeatApplication(payload: application.id))
        }
        seatApplicationCell.rejectAction = { [weak self] in
            guard let self = self else { return }
            self.store.dispatch(action: SeatActions.responseSeatApplication(payload: (false, application.id)))
            self.store.dispatch(action: SeatActions.removeSeatApplication(payload: application.id))
        }
        
        let publisher = store.select(SeatSelectors.getApplicationUserMap)
        publisher
            .receive(on: RunLoop.main)
            .sink { [weak seatApplicationCell] userMap in
                guard let cell = seatApplicationCell else { return }
                if let user = userMap[application.userId] {
                    cell.userName = user.name
                    cell.userAvatar = user.avatarUrl
                }
            }
            .store(in: &seatApplicationCell.cancellables)
    }
}

extension SeatApplicationListView {
    @objc
    func clickBack(sender: UIButton) {
        store.dispatch(action: NavigatorActions.navigatorTo(payload: .main))
    }
}
