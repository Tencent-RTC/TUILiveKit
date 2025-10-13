//
//  GiftListView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import AtomicXCore
import Combine
import RTCRoomEngine
import SnapKit
import TUICore

public class GiftListView: UIView {
    private let liveId: String
    private var store: GiftStore {
        GiftStore.create(liveID: liveId)
    }

    private var giftDataSource: [Gift] = []
    private var currentSelectedCellIndex: IndexPath = .init(row: 0, section: 0)
    private var cancellableSet: Set<AnyCancellable> = []

    private var rows: Int = 2
    private var itemSize: CGSize = .init(width: 74, height: 74 + 53)

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
        self.liveId = roomId
        super.init(frame: .zero)
        addObserver()
        setupUI()
        var language = TUIGlobalization.getPreferredLanguage() ?? "en"
        if language != "en", language != "zh-Hans", language != "zh-Hant" {
            language = "en"
        }
        store.setLanguage(language)
        store.refreshUsableGifts(completion: nil)
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    deinit {
        removeObserver()
    }

    private var isViewReady = false
    override public func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
    }
}

// MARK: - Special config

public extension GiftListView {
    func setRows(rows: Int) {
        if flowLayout.rows != rows {
            flowLayout.rows = rows
            collectionView.reloadData()
        }
    }

    func setItemSize(itemSize: CGSize) {
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
        store.state.subscribe(StatePublisherSelector(keyPath: \GiftState.usableGifts))
            .receive(on: RunLoop.main)
            .sink { [weak self] categories in
                guard let self = self else { return }
                var all: [Gift] = []
                categories.forEach { all.append(contentsOf: $0.giftList) }
                giftDataSource = all
                DispatchQueue.main.async { [weak self] in
                    guard let self = self else { return }
                    collectionView.reloadData()
                    collectionView.layoutIfNeeded()
                    if currentSelectedCellIndex.row < giftDataSource.count {
                        collectionView.selectItem(at: currentSelectedCellIndex, animated: false, scrollPosition: [])
                    }
                }
            }
            .store(in: &cancellableSet)
    }

    private func removeObserver() {
        cancellableSet.forEach { $0.cancel() }
        cancellableSet.removeAll()
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
                if let self = self {
                    DataReporter.reportEventData(eventKey: getReportKey())
                    store.sendGift(giftID: giftInfo.giftID, count: 1) { [weak self] result in
                        guard let self = self else { return }
                        switch result {
                        case .failure(let error):
                            let err = InternalError(code: error.code, message: error.message)
                            GiftManager.shared.toastSubject.send(err.localizedMessage)
                        default: break
                        }
                    }
                }
                if let cell = cell {
                    cell.isSelected = false
                }
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
