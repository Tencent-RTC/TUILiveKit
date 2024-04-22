//
// TUIBarrageDisplayView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/3/19.
//

import UIKit

protocol TUIBarrageDisplayViewDelegate: AnyObject {
    func barrageDisplayView(_ barrageDisplayView: TUIBarrageDisplayView, createCustomCell barrage: TUIBarrage) -> UIView?
}

extension TUIBarrageDisplayViewDelegate {
    func barrageDisplayView(_ barrageDisplayView: TUIBarrageDisplayView, createCustomCell barrage: TUIBarrage) -> UIView? {
        return nil
    }
}

class TUIBarrageDisplayView: UIView {
    weak var delegate: TUIBarrageDisplayViewDelegate?
    private let roomId: String
    private var dataSource: [TUIBarrage] = []
    private var cellHeight: CGFloat = 0
    private var announcementTitle = ""
    private var announcementContent = ""
    private var barrageCount = 0
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
        view.register(TUIBarrageCell.self, forCellReuseIdentifier: TUIBarrageCell.cellReuseIdentifier)
        return view
    }()

    init(roomId: String) {
        self.roomId = roomId
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
        TUIBarrageStore.shared.barrage.addObserver(self) { [weak self] barrage, _ in
            guard let self = self else { return }
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
        dataSource.append(contentsOf: barrages)
        barrageTableView.reloadData()
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            self.barrageTableView.scrollToRow(at: IndexPath(item: self.dataSource.count - 1, section: 0), at: .bottom, animated: true)
        }
        barrageCount += 1
    }

    func getBarrageCount() -> Int {
        return barrageCount
    }

    deinit {
        removeObserver()
    }

    private func removeObserver() {
        TUIBarrageStore.shared.barrage.removeObserver(self)
    }
}

extension TUIBarrageDisplayView: UITableViewDataSource {
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        guard let cell = tableView.dequeueReusableCell(withIdentifier: TUIBarrageCell.cellReuseIdentifier, for: indexPath) as? TUIBarrageCell else {
            return UITableViewCell()
        }
        let barrage = dataSource[indexPath.row]
        guard let view = delegate?.barrageDisplayView(self, createCustomCell: barrage) else {
            cell.useDefaultCell(barrage: barrage)
            cellHeight = cell.getCellHeight()
            cell.selectionStyle = .none
            return cell
        }
        cell.useCustomCell(view)
        cellHeight = cell.getCellHeight()
        cell.selectionStyle = .none
        return cell
    }

    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return dataSource.count
    }

    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return cellHeight
    }
}

extension TUIBarrageDisplayView: UITableViewDelegate {
    func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
}

extension TUIBarrageDisplayView: TUIBarrageManagerDelegate {
    func willSendBarrage(_ barrage: TUIBarrage) {
    }

    func didSendBarrage(_ barrage: TUIBarrage) {
    }

    func didReceiveBarrage(_ barrage: TUIBarrage) {
        insertBarrages([barrage])
    }
}
