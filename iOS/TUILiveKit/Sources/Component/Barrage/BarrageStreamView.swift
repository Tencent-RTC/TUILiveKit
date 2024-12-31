//
//  BarrageStreamView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

import UIKit
import RTCCommon

protocol BarrageStreamViewDelegate: AnyObject {
    func barrageDisplayView(_ barrageDisplayView: BarrageStreamView, createCustomCell barrage: TUIBarrage) -> UIView?
}

class BarrageStreamView: UIView {
    weak var delegate: BarrageStreamViewDelegate?
    
    private let roomId: String
    private var ownerId: String = ""
    private let manager: BarrageDisplayManager
    private var lastReloadDate: Date?
    
    // Max count of dataSource, default is 1000
    private let dataSource = ListManager<TUIBarrage>(maxLength: 1000)
    private var reloadWorkItem: DispatchWorkItem?

    private lazy var barrageTableView: UITableView = {
        let view = UITableView(frame: self.bounds, style: .plain)
        view.dataSource = self
        view.showsVerticalScrollIndicator = false
        view.backgroundColor = .clear
        view.separatorStyle = .none
        view.contentInsetAdjustmentBehavior = .never
        view.estimatedRowHeight = 30.scale375Height()
        view.register(TUIBarrageCell.self, forCellReuseIdentifier: TUIBarrageCell.cellReuseIdentifier)
        view.transform = CGAffineTransform(scaleX: 1, y: -1)
        return view
    }()

    init(roomId: String) {
        self.roomId = roomId
        self.manager = BarrageDisplayManager(roomId: roomId)
        super.init(frame: .zero)
        manager.delegate = self
        initEmotions()
    }
    
    func setOwnerId(_ ownerId: String) {
        self.ownerId = ownerId
        barrageTableView.reloadData()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isViewReady = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }

    func insertBarrages(_ barrages: [TUIBarrage]) {
        barrageTableView.layer.removeAllAnimations()
        dataSource.append(contentsOf: barrages)
        var indexPaths: [IndexPath] = []
        for i in 0 ..< barrages.count {
            indexPaths.append(IndexPath(row: i, section: 0))
        }
        setNeedsReloadData()
    }
    
    func getBarrageCount() -> Int {
        dataSource.totalCount
    }
}

extension BarrageStreamView: UITableViewDataSource {
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        guard let cell = tableView.dequeueReusableCell(withIdentifier: TUIBarrageCell.cellReuseIdentifier, for: indexPath) as? TUIBarrageCell else {
            return UITableViewCell()
        }
        guard let barrage = dataSource.reverse(index: indexPath.row) else { return cell }
        if let view = delegate?.barrageDisplayView(self, createCustomCell: barrage) {
            cell.setContent(view)
        } else {
            cell.setContent(barrage, ownerId: ownerId)
        }
        cell.transform = CGAffineTransform(scaleX: 1, y: -1)
        return cell
    }

    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return dataSource.count
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
            barrageTableView.scrollToRow(at: IndexPath(row: 0, section: 0), at: .top, animated: true)
        })
        if let workItem = reloadWorkItem {
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.25, execute: workItem)
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
