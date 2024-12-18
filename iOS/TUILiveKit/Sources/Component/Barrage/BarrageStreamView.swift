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
    
    private var dataSource: [String : [TUIBarrage]] = [:]
    private var barrageCount: [String: Int] = [:]

    private lazy var barrageTableView: UITableView = {
        let view = UITableView(frame: self.bounds, style: .plain)
        view.dataSource = self
        view.showsVerticalScrollIndicator = false
        view.backgroundColor = .clear
        view.separatorStyle = .none
        view.contentInsetAdjustmentBehavior = .never
        view.estimatedRowHeight = 30.scale375Height()
        view.register(TUIBarrageCell.self, forCellReuseIdentifier: TUIBarrageCell.cellReuseIdentifier)
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
        if dataSource[roomId] != nil {
            dataSource[roomId]?.append(contentsOf: barrages)
        } else {
            dataSource[roomId] = barrages
        }
        barrageTableView.reloadData()
        DispatchQueue.main.async { [weak self] in
            guard let self = self, let barrage =  self.dataSource[self.roomId] else { return }
            self.barrageTableView.scrollToRow(at: IndexPath(item: barrage.count == 0 ? 0 : (barrage.count - 1), section: 0),
                                              at: .bottom,
                                              animated: true)
        }
        incrementBarrageCount(for: roomId)
    }
    
    func getBarrageCount() -> Int {
        return barrageCount[roomId] ?? 0
    }
}

extension BarrageStreamView: UITableViewDataSource {
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        guard let cell = tableView.dequeueReusableCell(withIdentifier: TUIBarrageCell.cellReuseIdentifier, for: indexPath) as? TUIBarrageCell,
              let barrage = dataSource[roomId]?[indexPath.row]
        else {
            return UITableViewCell()
        }
        if let view = delegate?.barrageDisplayView(self, createCustomCell: barrage) {
            cell.setContent(view)
        } else {
            cell.setContent(barrage, ownerId: ownerId)
        }
        return cell
    }

    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return dataSource[roomId]?.count ?? 0
    }
}

extension BarrageStreamView: BarrageDisplayManagerDelegate {
    func didReceiveBarrage(_ barrage: TUIBarrage) {
        insertBarrages([barrage])
    }
}

// MARK: - Private functions
extension BarrageStreamView {
    private func incrementBarrageCount(for roomId: String) {
        if let count = barrageCount[roomId] {
            barrageCount[roomId] = count + 1
        } else {
            barrageCount[roomId] = 1
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
