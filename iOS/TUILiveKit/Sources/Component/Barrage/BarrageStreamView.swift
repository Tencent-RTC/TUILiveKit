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

extension BarrageStreamViewDelegate {
    func barrageDisplayView(_ barrageDisplayView: BarrageStreamView, createCustomCell barrage: TUIBarrage) -> UIView? {
        return nil
    }
}

class BarrageStreamView: UIView {
    weak var delegate: BarrageStreamViewDelegate?
    private var roomId: String
    private var dataSource: [String : [TUIBarrage]] = [:]
    private var cellHeightMap: [Int : CGFloat] = [:]
    private var announcementTitle = ""
    private var announcementContent = ""
    private var barrageCount: [String: Int] = [:]
    private lazy var barrageManager: TUIBarrageManager = {
        TUIBarrageManager.defaultCreate(roomId: roomId, delegate: self)
    }()

    private let announcementView: UIView = {
        let view = UIView()
        view.backgroundColor = .clear
        return view
    }()

    private let announcementContentLabel: UILabel = {
        let label = UILabel()
        label.font = UIFont(name: "PingFangSC-Regular", size: 14)
        label.numberOfLines = 0
        label.textAlignment = .left
        label.lineBreakMode = .byTruncatingTail
        label.textColor = .white
        return label
    }()

    private lazy var barrageTableView: UITableView = {
        let view = UITableView(frame: self.bounds, style: .plain)
        view.delegate = self
        view.dataSource = self
        view.showsVerticalScrollIndicator = false
        view.backgroundColor = .clear
        view.separatorStyle = .none
        view.contentInsetAdjustmentBehavior = .never
        view.estimatedRowHeight = 30.scale375Height()
        view.register(TUIBarrageCell.self, forCellReuseIdentifier: TUIBarrageCell.cellReuseIdentifier)
        return view
    }()

    init(roomId: String, ownerId: String) {
        self.roomId = roomId
        TUIBarrageStore.shared.ownerId = ownerId
        super.init(frame: .zero)
        barrageManager.initService()
        initEmotions()
        addObserver()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func initEmotions() {
        let faceManager = EmotionHelper.shared
        faceManager.useDefaultEmotions()
    }

    private func addObserver() {
        TUIBarrageStore.shared.barrageMap.addObserver(self) { [weak self] barrageMap, _ in
            guard let self = self, let barrage = barrageMap[roomId] else { return }
            self.insertBarrages([barrage])
        }
    }

    private var isViewReady = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }

    private func constructViewHierarchy() {
        addSubview(announcementView)
        addSubview(barrageTableView)
        announcementView.addSubview(announcementContentLabel)
    }

    private func activateConstraints() {
        announcementView.snp.makeConstraints { make in
            make.top.leading.trailing.equalToSuperview()
            make.height.equalTo(announcementContentLabel.mm_h + 6.scale375())
        }
        announcementContentLabel.snp.makeConstraints { make in
            make.top.leading.equalToSuperview().offset(4.scale375())
            make.width.equalTo(announcementContentLabel.mm_w)
            make.height.equalTo(announcementContentLabel.mm_h)
        }
        barrageTableView.snp.makeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.top.equalTo(announcementView.snp.bottom).offset(20.scale375Height())
        }
    }

    func setRoomId(roomId: String) {
        self.roomId = roomId
        barrageManager = TUIBarrageManager.defaultCreate(roomId: roomId, delegate: self)
        barrageManager.initService()
    }
    
    func setOwnerId(ownerId: String) {
        TUIBarrageStore.shared.ownerId = ownerId
    }
    
    func setAnnouncement(title: String,
                         content: String,
                         titleBackgroundColor: UIColor = .b1,
                         titleForegroundColor: UIColor = .white,
                         titleFont: UIFont = UIFont(name: "PingFangSC-Regular", size: 14) ?? UIFont(),
                         contentBackgroundColor: UIColor = .clear,
                         contentForegroundColor: UIColor = .white,
                         contentFont: UIFont = UIFont(name: "PingFangSC-Regular", size: 14) ?? UIFont()) {
        announcementTitle = title
        announcementContent = content
        let attributedAnnouncementString = NSMutableAttributedString(string: "")
        let attributedTitleString = NSAttributedString(string: "\(title)",
                                                       attributes: [.backgroundColor: titleBackgroundColor,
                                                                    .foregroundColor: titleForegroundColor,
                                                                    .font: titleFont,])
        let blankAttributedString = NSAttributedString(string: " ")
        let attributedContentString = NSAttributedString(string: content, attributes: [.backgroundColor: contentBackgroundColor,
                                                                                       .foregroundColor: contentForegroundColor,
                                                                                       .font: contentFont,])
        attributedAnnouncementString.append(attributedTitleString)
        attributedAnnouncementString.append(blankAttributedString)
        attributedAnnouncementString.append(attributedContentString)
        announcementContentLabel.attributedText = attributedAnnouncementString
        announcementContentLabel.mm_w = 231.scale375()
        announcementContentLabel.sizeToFit()
    }

    func setAnnouncementBackgroundColor(color: UIColor) {
        announcementView.backgroundColor = color
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
    
    private func incrementBarrageCount(for roomId: String) {
        if let count = barrageCount[roomId] {
            barrageCount[roomId] = count + 1
        } else {
            barrageCount[roomId] = 1
        }
    }

    func getBarrageCount() -> Int {
        return barrageCount[roomId] ?? 0
    }

    deinit {
        removeObserver()
    }

    private func removeObserver() {
        TUIBarrageStore.shared.barrageMap.removeObserver(self)
    }
}

extension BarrageStreamView: UITableViewDataSource {
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        guard let cell = tableView.dequeueReusableCell(withIdentifier: TUIBarrageCell.cellReuseIdentifier, for: indexPath) as? TUIBarrageCell,
              let barrage = dataSource[roomId]?[indexPath.row]
        else {
            return UITableViewCell()
        }
        guard let view = delegate?.barrageDisplayView(self, createCustomCell: barrage) else {
            cell.useDefaultCell(barrage: barrage)
            cell.selectionStyle = .none
            cellHeightMap[indexPath.row] = cell.getCellHeight()
            return cell
        }
        cell.useCustomCell(view)
        cellHeightMap[indexPath.row] = cell.getCellHeight()
        cell.selectionStyle = .none
        
        return cell
    }

    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return dataSource[roomId]?.count ?? 0
    }

    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return cellHeightMap[indexPath.row] ?? 33.scale375Height()
    }
}

extension BarrageStreamView: UITableViewDelegate {
    func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
}

extension BarrageStreamView: TUIBarrageManagerDelegate {
    func willSendBarrage(_ barrage: TUIBarrage) {
    }

    func didSendBarrage(_ barrage: TUIBarrage) {
    }

    func didReceiveBarrage(_ barrage: TUIBarrage) {
        insertBarrages([barrage])
    }
}
