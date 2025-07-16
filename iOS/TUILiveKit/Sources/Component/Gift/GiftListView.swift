//
//  GiftListView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import SnapKit
import UIKit
import TUICore
import RTCRoomEngine

public class GiftListView: UIView {
    private let roomId: String
    private let service: GiftService
    private var giftDataSource: [TUIGiftInfo] = []
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
    
    public init(roomId: String) {
        self.roomId = roomId
        self.service = GiftServiceFactory.getGiftService(roomId: roomId)
        super.init(frame: .zero)
        addObserver()
        setupUI()
        getGiftList()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        removeObserver()
    }
    
    private var isViewReady = false
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
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

    private func addObserver() {
        TUIGiftStore.shared.giftListMap.addObserver(self) { [weak self] giftListData, _ in
            guard let self = self, let giftList = giftListData[self.roomId] else { return }
            self.giftDataSource = giftList
            DispatchQueue.main.async {
                self.collectionView.reloadData()
                self.collectionView.layoutIfNeeded()
                if self.currentSelectedCellIndex.row < self.giftDataSource.count {
                    self.collectionView.selectItem(at: self.currentSelectedCellIndex, animated: false, scrollPosition: [])
                }
            }
        }
    }
    
    private func removeObserver() {
        TUIGiftStore.shared.giftListMap.removeObserver(self)
    }
    
    private func getGiftList() {
        Task {
            do {
                LiveKitLog.info("\(#file)", "\(#line)", "getGiftList")
                try await service.getGiftList()
            } catch let err as InternalError {
                LiveKitLog.error("\(#file)", "\(#line)", "getGiftList failed:\(err.localizedMessage)")
                DispatchQueue.main.async {
                    self.makeToast(err.localizedMessage)
                }
            }
        }
    }
    
    private func sendGift(giftInfo: TUIGiftInfo, giftCount: Int) {
        DataReporter.reportEventData(eventKey: getReportKey())
        Task {
            do {
                LiveKitLog.info("\(#file)", "\(#line)", "sendGift giftId: \(giftInfo.giftId), giftCount: \(giftCount)")
                try await service.sendGift(giftInfo: giftInfo, giftCount: giftCount)
            } catch let err as InternalError {
                LiveKitLog.error("\(#file)", "\(#line)", "sendGift failed:\(err.localizedMessage)")
                DispatchQueue.main.async {
                    self.makeToast(err.localizedMessage)
                }
            }
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
            let giftInfo = giftDataSource[indexPath.row]
            cell.giftInfo = giftInfo
            cell.sendBlock = { [weak self, weak cell] giftInfo in
                guard let self = self else { return }
                guard let cell = cell else { return }
                self.sendGift(giftInfo: giftInfo, giftCount: 1)
                cell.isSelected = false
            }
        }
        return cell
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
