//
//  AnchorLinkControlPanel.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/25.
//

import Foundation
import Combine

enum LinkControlAction {
    case `default`
    case agree(userInfo: UserInfo)
    case reject(userInfo: UserInfo)
    case hangUp(userInfo: UserInfo)
}

class AnchorLinkControlPanel: UIView {
    private var cancellable = Set<AnyCancellable>()
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()
    private var linkingAudienceList: [UserInfo] = []
    private var applyLinkAudienceList: [UserInfo] = []
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
    private var liveRoomInfo:LiveRoomInfo {
        engineService.liveRoomInfo
    }
    private var engineService: RoomEngineService
    init(engineService: RoomEngineService) {
        self.engineService = engineService
        super.init(frame: .zero)
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

    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.contentMode = .center
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.textColor = .g7
        label.text = .anchorLinkControlTitle
        label.sizeToFit()
        return label
    }()

    private lazy var userListTableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .grouped)
        tableView.separatorStyle = .none
        tableView.delegate = self
        tableView.dataSource = self
        tableView.backgroundColor = .clear
        tableView.register(UserBaseCell.self, forCellReuseIdentifier: UserBaseCell.cellReuseIdentifier)
        tableView.register(UserRequestLinkCell.self, forCellReuseIdentifier: UserRequestLinkCell.cellReuseIdentifier)
        tableView.register(UserLinkCell.self, forCellReuseIdentifier: UserLinkCell.cellReuseIdentifier)
        
        engineService.liveRoomInfo.$applyLinkAudienceList
            .receive(on: RunLoop.main)
            .sink { [weak self] _ in
                guard let self = self else{ return}
                self.updateView()
            }.store(in: &cancellable)

        liveRoomInfo.linkingAudienceList.addObserver(self) { [weak self] _, _ in
            self?.updateView()
        }

        engineService.liveRoomInfo.$selfInfo
            .receive(on: RunLoop.main)
            .sink { [weak self] _ in
                guard let self = self else{ return}
                self.updateView()
            }.store(in: &cancellable)
        return tableView
    }()

    private func updateView() {
        linkingAudienceList = getLinkingAudienceList()
        applyLinkAudienceList = Array(engineService.liveRoomInfo.applyLinkAudienceList)
        userListTableView.reloadData()
    }
    
    func getLinkingAudienceList() -> [UserInfo] {
        var list: [UserInfo] = Array(liveRoomInfo.linkingAudienceList.value)
        list = list.filter({ $0.userId != engineService.liveRoomInfo.selfInfo.userId })
        return list
    }
}

// MARK: Layout

extension AnchorLinkControlPanel {
    func constructViewHierarchy() {
        backgroundColor = .g2
        layer.cornerRadius = 16
        layer.masksToBounds = true
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(userListTableView)
    }

    func activateConstraints() {
        snp.remakeConstraints { make in
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
            make.width.equalTo(titleLabel.mm_w)
        }

        userListTableView.snp.remakeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.top.equalTo(titleLabel.snp.bottom).offset(20)
        }
    }
}

// MARK: Action

extension AnchorLinkControlPanel {
    @objc func backButtonClick() {
        popupAction?.value = .close
    }

    func handleLinkAction(action:LinkControlAction) {
        switch action {
            case let .agree(userInfo):
                engineService.responseRemoteRequestUser(userInfo, agree: true)
                return
            case let .reject(userInfo):
                engineService.responseRemoteRequestUser(userInfo, agree: false)
                return
            case let .hangUp(userInfo):
                engineService.kickUserOffSeatByAdmin(userId: userInfo.userId)
                return
            case .default:
                debugPrint("default")
        }
    }
    
}

extension AnchorLinkControlPanel: UITableViewDataSource {
    internal func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        if section == 0 {
            return linkingAudienceList.count
        } else if section == 1 {
            return applyLinkAudienceList.count
        } else {
            return 0
        }
    }

    func numberOfSections(in tableView: UITableView) -> Int {
        return 2
    }
}

extension AnchorLinkControlPanel: UITableViewDelegate {
    internal func tableView(_ tableView: UITableView,
                            cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        var cell:UserBaseCell?
        if indexPath.section == 0 {
            if indexPath.row < linkingAudienceList.count {
                cell = tableView.dequeueReusableCell(withIdentifier: UserLinkCell.cellReuseIdentifier, for: indexPath) as? UserBaseCell
                cell?.userInfo = linkingAudienceList[indexPath.row]
                cell?.lineView.isHidden = (linkingAudienceList.count-1) == indexPath.row
            }
        } else if indexPath.section == 1 {
            if indexPath.row < applyLinkAudienceList.count {
                cell = tableView.dequeueReusableCell(withIdentifier: UserRequestLinkCell.cellReuseIdentifier, for: indexPath) as? UserBaseCell
                cell?.userInfo = applyLinkAudienceList[indexPath.row]
            }
        }
        cell?.action.addObserver(self, closure: {[weak self] action, _ in
            self?.handleLinkAction(action: action)
        })
        return cell ?? tableView.dequeueReusableCell(withIdentifier: UserBaseCell.cellReuseIdentifier, for: indexPath)
    }

    internal func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    }

    internal func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 50.scale375()
    }

    func tableView(_ tableView: UITableView, viewForHeaderInSection section: Int) -> UIView? {
        let headerView = UIView(frame: CGRect(x: 0, y: 0, width: tableView.frame.width, height: 20.scale375Height()))
        headerView.backgroundColor = .g2
        let label = UILabel(frame: CGRect(x: 24, y: 0, width: headerView.frame.width , height: headerView.frame.height))
        if section == 0 {
            label.text = .localizedReplace(.anchorLinkControlSeatCount,
                                          replace: "\(linkingAudienceList.count)/\(max(liveRoomInfo.maxSeatCount,1) - 1)")
        } else if section == 1 {
            label.text = .localizedReplace(.anchorLinkControlRequestCount,
                                          replace: "\(applyLinkAudienceList.count)")
        }
        label.textColor = .greyColor
        headerView.addSubview(label)
        return headerView
    }

    func tableView(_ tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat {
        if section == 0 && linkingAudienceList.count > 0 {
            return 20.scale375Height()
        }
        
        if section == 1 && applyLinkAudienceList.count > 0 {
            return 20.scale375Height()
        }
        
        return 0
    }
    
    func tableView(_ tableView: UITableView, viewForFooterInSection section: Int) -> UIView? {
        if section == 0 , linkingAudienceList.count > 0 {
            let footerView = UIView(frame: CGRect(x: 0, y: 0, width: tableView.frame.width, height: 7.0))
            footerView.backgroundColor = .g3.withAlphaComponent(0.1)
            return footerView
        } else {
            return nil
        }
    }
    
    func tableView(_ tableView: UITableView, heightForFooterInSection section: Int) -> CGFloat {
        if section == 0 , linkingAudienceList.count > 0 {
            return 7.scale375Height()
        } else {
            return 0
        }
    }
}

extension AnchorLinkControlPanel: PopupPanelSubViewProtocol {
    func setAction(_ popupAction: Observable<PopupPanelAction>) {
        self.popupAction = popupAction
    }

    func updateRootViewOrientation(isPortrait: Bool) {
        self.isPortrait = isPortrait
        activateConstraints()
    }
}

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
