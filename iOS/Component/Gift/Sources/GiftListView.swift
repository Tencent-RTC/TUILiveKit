//
//  GiftListView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import SnapKit
import UIKit
import TUICore
import TUILiveResources

public protocol GiftListViewDelegate: AnyObject {
    func onSendGift(gift model: TUIGift, giftCount: Int)
}

public class GiftListView: UIView {
    public weak var delegate: GiftListViewDelegate?
    
    private let roomId: String
    private lazy var manager = GiftManager(roomId: roomId)
    private var giftDataSource: [TUIGift] = []
    private var currentSelectedCellIndex: IndexPath = IndexPath(row: 0, section: 0)
    
    private var rows: Int = 2
    private var itemSize: CGSize = CGSize(width: 74, height: 74 + 53)
    
    private lazy var flowLayout: TUIGiftSideslipLayout = {
        let layout = TUIGiftSideslipLayout()
        layout.scrollDirection = .horizontal
        layout.itemSize = itemSize
        layout.rows = rows
        return layout
    }()

    private lazy var collectionView: UICollectionView = {
        let view = UICollectionView(frame: self.bounds, collectionViewLayout: self.flowLayout)
        if #available(iOS 11.0, *) {
            view.contentInsetAdjustmentBehavior = .never
        }
        view.register(TUIGiftCell.self, forCellWithReuseIdentifier: TUIGiftCell.cellReuseIdentifier)
        view.isPagingEnabled = true
        view.scrollsToTop = false
        view.delegate = self
        view.dataSource = self
        view.showsVerticalScrollIndicator = false
        view.showsHorizontalScrollIndicator = false
        view.backgroundColor = .clear
        return view
    }()

    public init(roomId: String, delegate: GiftListViewDelegate? = nil) {
        self.roomId = roomId
        self.delegate = delegate
        super.init(frame: .zero)
        setupUI()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    public func setGiftList(_ giftList: [TUIGift]) {
        if giftDataSource.isEmpty {
            LiveKitLog.warn("\(#file)", "\(#line)", "giftModelSource empty!")
        }
        self.giftDataSource = giftList
        collectionView.reloadData()
    }
    
    private var isViewReady = false
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        if !giftDataSource.isEmpty {
            collectionView.selectItem(at: IndexPath(row: 0, section: 0), animated: false, scrollPosition: [])
        }
    }
}

// MARK: - Special config
extension GiftListView {
    public func setRows(rows: Int) {
        if flowLayout.rows != rows {
            flowLayout.rows = rows
            collectionView.reloadData()
        }
    }

    public func setItemSize(itemSize: CGSize) {
        if flowLayout.itemSize == itemSize {
            flowLayout.itemSize = itemSize
            collectionView.reloadData()
        }
    }
}

// MARK: - Private functions

extension GiftListView {
    private func setupUI() {
        addSubview(collectionView)
        collectionView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
}

// MARK: UICollectionViewDelegate

extension GiftListView: UICollectionViewDelegate {
    public func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        let preSelectedCellIndex = currentSelectedCellIndex
        currentSelectedCellIndex = indexPath
        if let cell = collectionView.cellForItem(at: preSelectedCellIndex) as? TUIGiftCell {
            cell.isSelected = false
        }
        if let cell = collectionView.cellForItem(at: currentSelectedCellIndex) as? TUIGiftCell {
            cell.isSelected = true
        }
    }
}

// MARK: UICollectionViewDataSource

extension GiftListView: UICollectionViewDataSource {
    public func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return giftDataSource.count
    }
    
    public func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let reuseCell = collectionView.dequeueReusableCell(withReuseIdentifier: TUIGiftCell.cellReuseIdentifier, for: indexPath)
        guard let cell = reuseCell as? TUIGiftCell else { return reuseCell }
        if giftDataSource.count > indexPath.row {
            let model = giftDataSource[indexPath.row]
            cell.giftModel = model
            cell.sendBlock = { [weak self, weak cell] giftModel in
                guard let self = self else { return }
                guard let cell = cell else { return }
                self.delegate?.onSendGift(gift: giftModel, giftCount: 1)
                cell.isSelected = false
            }
        }
        return cell
    }
}

extension GiftListView {
    public func sendGift(model: TUIGift, giftCount: Int, receiver: TUIGiftUser, completion: TUIGiftIMSendBlock) {
        DataReporter.reportEventData(eventKey: getReportKey())
        manager.sendGift(model, receiver: receiver, giftCount: giftCount) { code, msg in
            completion?(code, msg)
        }
    }
}

// MARK: DataReport
private extension GiftListView {
    private func getReportKey() -> Int {
        let isSupportEffectPlayer = isSupportEffectPlayer()
        var key = Constants.DataReport.kDataReportLiveGiftSVGASendCount
        switch DataReporter.componentType {
        case .liveRoom:
            key = isSupportEffectPlayer ? Constants.DataReport.kDataReportLiveGiftEffectSendCount :
            Constants.DataReport.kDataReportLiveGiftSVGASendCount
        case .voiceRoom:
            key = isSupportEffectPlayer ? Constants.DataReport.kDataReportVoiceGiftEffectSendCount :
            Constants.DataReport.kDataReportVoiceGiftSVGASendCount
        }
        return key
    }
    
    private func isSupportEffectPlayer() -> Bool {
        let service = TUICore.getService("TUIEffectPlayerService")
        return service != nil
    }
}
