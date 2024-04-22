//
//  TUILiveRoomAudienceViewController.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/12/11.
//

import Foundation

public class TUILiveRoomAudienceViewController: UIViewController {
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

    private func updateView() {
        roomListTableView.reloadData()
    }
    
    private var roomInfoMap: [String:LiveRoomInfo] = [:]
    private var roomList: [LiveRoomInfo] = []
    public init(roomId:String) {
        let roomInfo = LiveRoomInfo(roomId: roomId)
        self.roomInfoMap[roomId] = LiveRoomInfo(roomId: roomId)
        self.roomList = [roomInfo]
        super.init(nibName: nil, bundle: nil)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        navigationController?.setNavigationBarHidden(true, animated: true)
        constructViewHierarchy()
        activateConstraints()
        updateView()
    }
    
    private func insertRoomList(roomList:[LiveRoomInfo]) {
        for roomInfo in roomList where (roomInfoMap[roomInfo.roomId.value] == nil) {
            self.roomList.append(roomInfo)
            roomInfoMap[roomInfo.roomId.value] = roomInfo
        }
    }
    
    deinit {
        // Reset audio effect View data.
        AudioEffectView.session.reset()
        print("deinit \(type(of: self))")
    }
    
}

// MARK: Layout

extension TUILiveRoomAudienceViewController {
    func constructViewHierarchy() {
        view.backgroundColor = .g1
        view.addSubview(roomListTableView)
    }

    func activateConstraints() {
        roomListTableView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
}

extension TUILiveRoomAudienceViewController: UITableViewDataSource {
    public func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return roomList.count
    }

    public func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
}

extension TUILiveRoomAudienceViewController: UITableViewDelegate {
    public func tableView(_ tableView: UITableView,
                          cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: AudienceScrollCell.cellReuseIdentifier, for: indexPath) as! AudienceScrollCell
        cell.state = .default
        if indexPath.row < roomList.count {
            let roomInfo = roomList[indexPath.row]
            if cell.audienceView?.engineService.liveRoomInfo.roomId.value != roomInfo.roomId.value {
                cell.audienceView = AudienceView(roomId: roomInfo.roomId.value)
            }
        }
        return cell
    }

    public func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return view.mm_h
    }

    public func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
        guard let cell = cell as? AudienceScrollCell else { return }
        cell.state = .willDisplay
    }

    public func tableView(_ tableView: UITableView, didEndDisplaying cell: UITableViewCell, forRowAt indexPath: IndexPath) {
        guard let cell = cell as? AudienceScrollCell else { return }
        cell.state = .didEndDisplay
    }
}

extension TUILiveRoomAudienceViewController: UIScrollViewDelegate {
    public func scrollViewDidEndDecelerating(_ scrollView: UIScrollView) {
        let row = scrollView.contentOffset.y / max(scrollView.mm_h, 1)
        let indexPath = IndexPath(row: Int(row), section: 0)
        guard let cell = roomListTableView.cellForRow(at: indexPath) as? AudienceScrollCell else { return }
        cell.state = .didDisplay
    }
}
