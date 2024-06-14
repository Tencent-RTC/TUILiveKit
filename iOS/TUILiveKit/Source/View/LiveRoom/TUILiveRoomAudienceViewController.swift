//
//  TUILiveRoomAudienceViewController.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/12/11.
//
import UIKit
import Combine
import TUICore

public class TUILiveRoomAudienceViewController: UIViewController {
    // MARK: - property: view
    private lazy var roomListTableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .plain)
        tableView.separatorStyle = .none
        tableView.isPagingEnabled = true
        tableView.delegate = self
        tableView.dataSource = self
        tableView.backgroundColor = .clear
        tableView.register(AudienceScrollCell.self, forCellReuseIdentifier: AudienceScrollCell.cellReuseIdentifier)
        if #available(iOS 11.0, *) {
            tableView.contentInsetAdjustmentBehavior = .never
        }
        tableView.isScrollEnabled = false
        return tableView
    }()
    
    // MARK: - private property.
    @Injected private var store: LiveStore
    private var cancellableSet = Set<AnyCancellable>()
    private lazy var liveRouter: LiveRouter = LiveRouter(rootViewController: self, rootRoute: .audience)
    private let giftCloudServer: IGiftCloudServer = GiftCloudServer()
    private lazy var giftPanelView: TUIGiftListView = {
        let roomId = store.selectCurrent(RoomSelectors.getRoomId)
        let view = TUIGiftListView(groupId: roomId)
        view.delegate = self
        weak var weakGiftPanelView = view
        giftCloudServer.queryGiftInfoList { [weak self] error, giftList in
            guard let self = self else { return }
            DispatchQueue.main.async {
                if error == .noError {
                    weakGiftPanelView?.setGiftList(giftList)
                } else {
                    self.view.makeToast("query gift list error, code = \(error)")
                }
            }
        }
        giftCloudServer.queryBalance { [weak self] error, balance in
            guard let self = self else { return }
            DispatchQueue.main.async {
                if error == .noError {
                    weakGiftPanelView?.setBalance(balance)
                } else {
                    self.view.makeToast("query balance error, code = \(error)")
                }
            }
        }
        return view
    }()
    
    public init(roomId:String) {
        super.init(nibName: nil, bundle: nil)
        self.store.dispatch(action: RoomActions.updateRoomId(payload: roomId))
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        navigationController?.setNavigationBarHidden(true, animated: true)
        initializeRoomState()
        constructViewHierarchy()
        activateConstraints()
        updateView()
        subscribeCustomEvent()
    }
    
    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        UIApplication.shared.isIdleTimerDisabled = true
    }

    public override func viewDidDisappear(_ animated: Bool) {
        super.viewDidDisappear(animated)
        UIApplication.shared.isIdleTimerDisabled = false
    }
    
    deinit {
        // Reset audio effect View data.
        AudioEffectView.session.reset()
        print("deinit \(type(of: self))")
    }
    
}

extension TUILiveRoomAudienceViewController {
    private func initializeRoomState() {
        liveRouter.viewProvider = self
        liveRouter.subscribeRouter()
        // prepare user state.
        store.dispatch(action: UserActions.getSelfInfo())
        // prepare room state.
        let roomId = store.selectCurrent(RoomSelectors.getRoomId)
        var state = RoomState()
        state.roomId = roomId
        store.dispatch(action: RoomActions.initializeRoomState(payload: state))
    }
    
    private func constructViewHierarchy() {
        view.backgroundColor = .g1
        view.addSubview(roomListTableView)
    }

    private func activateConstraints() {
        roomListTableView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func updateView() {
        roomListTableView.reloadData()
    }
    
    private func subscribeCustomEvent() {
        store.userActionSubject
            .receive(on: DispatchQueue.main)
            .filter({
                $0.id == UserResponseActions.like.id
            })
            .sink { [weak self] _ in
                guard let self = self else { return }
                self.giftPanelView.sendLike()
            }
            .store(in: &cancellableSet)
    }
}

extension TUILiveRoomAudienceViewController: RouteViewProvider {
    func getRouteView(route: LiveRouter.Route) -> UIView? {
        if route == .giftView {
            return giftPanelView
        } else {
            return nil
        }
    }
}

extension TUILiveRoomAudienceViewController: UITableViewDataSource {
    public func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return 1
    }

    public func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
}

extension TUILiveRoomAudienceViewController: UITableViewDelegate {
    public func tableView(_ tableView: UITableView,
                          cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: AudienceScrollCell.cellReuseIdentifier, for: indexPath) as! AudienceScrollCell
        let roomId = store.selectCurrent(RoomSelectors.getRoomId)
        if cell.audienceView?.roomId != roomId {
            cell.audienceView = AudienceView(roomId: roomId)
        }
        return cell
    }

    public func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return view.mm_h
    }

    public func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
        guard let cell = cell as? AudienceScrollCell else { return }
        cell.audienceView?.startDisplay()
    }

    public func tableView(_ tableView: UITableView, didEndDisplaying cell: UITableViewCell, forRowAt indexPath: IndexPath) {
        guard let cell = cell as? AudienceScrollCell else { return }
        cell.audienceView?.endDisplay()
    }
}

extension TUILiveRoomAudienceViewController: UIScrollViewDelegate {
    public func scrollViewDidEndDecelerating(_ scrollView: UIScrollView) {
        let row = scrollView.contentOffset.y / max(scrollView.mm_h, 1)
        let indexPath = IndexPath(row: Int(row), section: 0)
        guard let cell = roomListTableView.cellForRow(at: indexPath) as? AudienceScrollCell else { return }
        // FIXME: - 检查这里的逻辑，感觉没有必要
        cell.audienceView?.displayComplete()
    }
}

extension TUILiveRoomAudienceViewController: TUIGiftListViewDelegate {
    func onRecharge(giftListView view: TUIGiftListView) {
        giftCloudServer.rechargeBalance { [weak self] error, balance in
            guard let self = self else { return }
            if error == .noError {
                view.setBalance(balance)
            } else {
                self.view.makeToast(.balanceInsufficientText)
            }
        }
    }
    
    func onSendGift(giftListView view: TUIGiftListView, giftModel: TUIGift, giftCount: Int) {
        let receiver = TUIGiftUser()
        let anchorInfo = store.selectCurrent(RoomSelectors.getRoomOwnerInfo)
        receiver.userId = anchorInfo.userId
        receiver.userName = anchorInfo.name
        receiver.avatarUrl = anchorInfo.avatarUrl
        receiver.level = "0"
        giftCloudServer.sendGift(sender: TUILogin.getUserID() ?? "",
                                 receiver: receiver.userId,
                                 giftModel: giftModel,
                                 giftCount: giftCount) { [weak self] error, balance in
            guard let self = self else { return }
            if error == .noError {
                view.sendGift(giftModel: giftModel, giftCount: giftCount, receiver: receiver)
                view.setBalance(balance)
            } else {
                self.view.makeToast(.balanceInsufficientText)
            }
        }
    }
}

private extension String {
    static var balanceInsufficientText =
        localized("live.balanceInsufficient")
}
