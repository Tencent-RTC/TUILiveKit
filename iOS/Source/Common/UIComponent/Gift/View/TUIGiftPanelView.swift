//
//  TUIGiftPanelView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import SnapKit
import UIKit

protocol TUIGiftPanelDelegate: AnyObject {
    func onSendGift(gift model: TUIGift,giftCount: Int)
    func onGiftDidSend(_ giftView: TUIGiftPanelView,
                       gift model: TUIGift,
                       sender: TUIGiftUser,
                       receiver: TUIGiftUser,
                       giftCount: Int,
                       isSuccess: Bool, message: String)
    func onLikeDidSend(_ giftView: TUIGiftPanelView, sender: TUIGiftUser, isSuccess: Bool, message: String)
}

class TUIGiftPanelView: UIView {
    var groupId: String = ""
    weak var delegate: TUIGiftPanelDelegate?
    var presenter: TUIGiftPresenter = TUIGiftPresenter()
    var sendLikeDate: Date = Date()
    var currentLikeCount: Int = 0
    private var sendLikeWorkItem: DispatchWorkItem?
    private var rows: Int = 2
    private var itemSize: CGSize = CGSize()
    private var giftDataSource: [TUIGift] = []
    private var currentSelectedCellIndex: IndexPath?

    let config: TUIGiftPanelConfig = TUIGiftPanelConfig.defaultCreate()

    let flowLayout: TUIGiftSideslipLayout = {
        let layout = TUIGiftSideslipLayout()
        layout.scrollDirection = .horizontal
        return layout
    }()

    lazy var collectionView: UICollectionView = {
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

    init(_ delegate: TUIGiftPanelDelegate, groupId: String) {
        self.delegate = delegate
        self.groupId = groupId
        super.init(frame: .zero)
        initPresenter()
        setRows(rows: config.rows)
        setItemSize(itemSize: config.itemSize)
        setupUI()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    func setRows(rows: Int) {
        if self.rows != rows {
            self.rows = rows
        }
    }

    func setItemSize(itemSize: CGSize) {
        self.itemSize = itemSize
    }

    func setGiftModelSource(_ giftDataSource: [TUIGift]) {
        self.giftDataSource = giftDataSource
    }

    func reloadData() {
        config.rows = rows
        config.itemSize = itemSize
        config.giftDataSource = giftDataSource
        flowLayout.itemSize = config.itemSize
        flowLayout.rows = config.rows
        collectionView.reloadData()
    }
    
    private func initPresenter() {
        presenter = TUIGiftPresenter.defaultCreate(self, groupId: groupId)
        sendLikeDate = Date(timeIntervalSinceNow: -1 * 60)
        currentLikeCount = 0
    }
}

// MARK: Layout

extension TUIGiftPanelView {
    func setupUI() {
        addSubview(collectionView)
        collectionView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
}

// MARK: UICollectionViewDelegate

extension TUIGiftPanelView: UICollectionViewDelegate {
    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        handleCellSelectedState(indexPath)
    }
    
    func handleCellSelectedState(_ indexPath: IndexPath) {
        let preSelectedCellIndex = currentSelectedCellIndex
        currentSelectedCellIndex = indexPath
        if let index = preSelectedCellIndex,
           let cell = collectionView.cellForItem(at: index) as? TUIGiftCell {
            cell.isSelected = false
        }
        if let index = currentSelectedCellIndex, let cell = collectionView.cellForItem(at: index) as? TUIGiftCell {
            cell.isSelected = true
        }
    }
    
}

// MARK: UICollectionViewDataSource

extension TUIGiftPanelView: UICollectionViewDataSource {
    func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }
    
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return config.giftDataSource.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: TUIGiftCell.cellReuseIdentifier, for: indexPath) as! TUIGiftCell
        if config.giftDataSource.count > indexPath.row {
            let model = config.giftDataSource[indexPath.row]
            cell.giftModel = model
            cell.isSelected = false
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

extension TUIGiftPanelView {
    func sendGift(model: TUIGift, giftCount: Int, receiver: TUIGiftUser) {
        presenter.sendGift(model, receiver: receiver, giftCount: giftCount)
    }

    @objc func sendLike() {
        let maxLikeCount: Int = 20
        let maxDuration: Double = 5
        if currentLikeCount >= maxLikeCount {
            presenter.sendLike()
            currentLikeCount = 0
            sendLikeDate = Date()
            return
        }
        let duration = -sendLikeDate.timeIntervalSinceNow
        if duration > maxDuration {
            presenter.sendLike()
            currentLikeCount = 0
            sendLikeDate = Date()
        } else {
            currentLikeCount += 1
            let sender = TUIGiftUser()
            onLikeDidSend(sender: sender, isSuccess: false, message: "send like by local.")
            let delayInSeconds = maxDuration - duration
            sendLikeWorkItem?.cancel()
            let workItem = DispatchWorkItem { [weak self] in
                self?.sendLike()
            }
            sendLikeWorkItem = workItem
            DispatchQueue.main.asyncAfter(deadline: .now() + delayInSeconds, execute: workItem)
        }
    }
}

// MARK: TUIGiftPresenterDelegate

extension TUIGiftPanelView: TUIGiftPresenterDelegate {
    func onGiftDidSend(_ model: TUIGift, sender: TUIGiftUser, receiver: TUIGiftUser, giftCount: Int, isSuccess: Bool, message: String) {
        delegate?.onGiftDidSend(self, gift: model, sender: sender, receiver: receiver, giftCount: giftCount, isSuccess: isSuccess, message: message)
    }

    func onLikeDidSend(sender: TUIGiftUser, isSuccess: Bool, message: String) {
        delegate?.onLikeDidSend(self, sender: sender, isSuccess: isSuccess, message: message)
    }

    func onReceiveGift(_ model: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser) {
    }

    func onReceiveLike(sender: TUIGiftUser) {
        
    }
}
