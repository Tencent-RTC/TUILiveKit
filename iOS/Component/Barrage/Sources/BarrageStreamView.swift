//
//  BarrageStreamView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

import UIKit
import RTCCommon
import RTCRoomEngine
import Combine

public protocol BarrageStreamViewDelegate: AnyObject {
    func barrageDisplayView(_ barrageDisplayView: BarrageStreamView, createCustomCell barrage: TUIBarrage) -> UIView?
    func onBarrageClicked(user: TUIUserInfo)
}

public class BarrageStreamView: UIView {
    public weak var delegate: BarrageStreamViewDelegate?
    
    private let roomId: String
    private var ownerId: String = ""
    private let manager: BarrageDisplayManager
    private var lastReloadDate: Date?
    
    // Max count of dataSource, default is 1000
    private let dataSource = ListManager<TUIBarrage>(maxLength: 1000)
    private var reloadWorkItem: DispatchWorkItem?
    private var cancellableSet = Set<AnyCancellable>()
    
    private var isDraging: Bool = false

    private lazy var barrageTableView: UITableView = {
        let view = UITableView(frame: self.bounds, style: .plain)
        view.dataSource = self
        view.delegate = self
        view.showsVerticalScrollIndicator = false
        view.backgroundColor = .clear
        view.separatorStyle = .none
        view.contentInsetAdjustmentBehavior = .never
        view.estimatedRowHeight = 30.scale375Height()
        view.register(TUIBarrageCell.self, forCellReuseIdentifier: TUIBarrageCell.cellReuseIdentifier)
        view.contentInset = UIEdgeInsets(top: bounds.height - view.estimatedRowHeight, left: 0, bottom: 0, right: 0)
        return view
    }()

    public init(roomId: String) {
        self.roomId = roomId
        self.manager = BarrageDisplayManager(roomId: roomId)
        super.init(frame: .zero)
        manager.delegate = self
        initEmotions()
        
        BarrageManager.shared.toastSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] msg in
                guard let self = self else { return }
                superview?.makeToast(msg)
            }
            .store(in: &cancellableSet)
    }
    
    public func setOwnerId(_ ownerId: String) {
        self.ownerId = ownerId
        barrageTableView.reloadData()
    }
    
    public func clearAllMessage() {
        reloadWorkItem?.cancel()
        dataSource.removeAll()
        barrageTableView.reloadData()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isViewReady = false
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }

    public func insertBarrages(_ barrages: [TUIBarrage]) {
        barrageTableView.layer.removeAllAnimations()
        dataSource.append(contentsOf: barrages)
        setNeedsReloadData()
    }
    
    public func getBarrageCount() -> Int {
        dataSource.totalCount
    }
    
    public override func hitTest(_ point: CGPoint, with event: UIEvent?) -> UIView? {
        let view = super.hitTest(point, with: event)
        if view == self || view == barrageTableView {
            return nil
        }
        return view
    }
    
    private func bindInteraction() {
        BarrageManager.shared.sendBarrageSubject
            .receive(on: RunLoop.main)
            .sink { [weak self] roomId, barrage in
                guard let self = self, roomId == self.roomId else { return }
                insertBarrages([barrage])
            }
            .store(in: &cancellableSet)
    }
}

extension BarrageStreamView: UITableViewDataSource {
    public func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        guard let cell = tableView.dequeueReusableCell(withIdentifier: TUIBarrageCell.cellReuseIdentifier, for: indexPath) as? TUIBarrageCell else {
            return UITableViewCell()
        }
        guard let barrage = dataSource[indexPath.row] else { return cell }
        if let view = delegate?.barrageDisplayView(self, createCustomCell: barrage) {
            cell.setContent(view)
        } else {
            cell.setContent(barrage, ownerId: ownerId)
        }
        return cell
    }

    public func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return dataSource.count
    }
}

extension BarrageStreamView: UITableViewDelegate {
    public func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        guard let barrageUser = dataSource[indexPath.row]?.user else { return }
        let user = TUIUserInfo()
        user.userId = barrageUser.userId
        user.userName = barrageUser.userName
        user.avatarUrl = barrageUser.avatarUrl
        delegate?.onBarrageClicked(user: user)
    }
    
    public func scrollViewWillBeginDragging(_ scrollView: UIScrollView) {
        isDraging = true
    }
    
    public func scrollViewWillEndDragging(_ scrollView: UIScrollView, withVelocity velocity: CGPoint, targetContentOffset: UnsafeMutablePointer<CGPoint>) {
        isDraging = false
        let contentHeight = scrollView.contentSize.height
        let tableViewHeight = scrollView.bounds.height
        if contentHeight < tableViewHeight {
            targetContentOffset.pointee.y = contentHeight - tableViewHeight + scrollView.contentInset.bottom
        }
    }
}

extension BarrageStreamView: BarrageDisplayManagerDelegate {
    func didReceiveBarrage(_ barrage: TUIBarrage) {
        insertBarrages([barrage])
    }
}

// MARK: - Private functions
extension BarrageStreamView {
    private func setNeedsReloadData() {
        let current = Date()
        if let last = lastReloadDate {
            let dur = current.timeIntervalSince(last)
            if dur <= 0.25 {
                return
            }
        }
        lastReloadDate = current
        reloadWorkItem?.cancel()
        reloadWorkItem = DispatchWorkItem(block: { [weak self] in
            guard let self = self else { return }
            barrageTableView.reloadData()
            barrageTableView.layoutIfNeeded()
            updateContentInset()
            if !isDraging {
                let indexPath = IndexPath(row: max(dataSource.count - 1, 0), section: 0)
                barrageTableView.scrollToRow(at: indexPath, at: .bottom, animated: true)
            }
        })
        if let workItem = reloadWorkItem {
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.25, execute: workItem)
        }
    }
    
    private func updateContentInset() {
        let contentHeight = barrageTableView.contentSize.height
        let tableViewHeight = barrageTableView.bounds.height
        if contentHeight < tableViewHeight {
            let topInset = tableViewHeight - contentHeight
            barrageTableView.contentInset = UIEdgeInsets(top: topInset, left: 0, bottom: 0, right: 0)
        } else {
            if barrageTableView.contentInset != .zero {
                barrageTableView.contentInset = .zero
            }
        }
    }
    
    private func constructViewHierarchy() {
        addSubview(barrageTableView)
    }
    
    private func activateConstraints() {
        barrageTableView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func initEmotions() {
        EmotionHelper.shared.useDefaultEmotions()
    }
}
