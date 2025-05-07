//
//  LiveListRootView.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/5/30.
//

import UIKit
import ESPullToRefresh
import Combine
import RTCRoomEngine
import RTCCommon

class LiveListRootView: UIView {
    
    private let manager: LiveListManager
    
    private var cancellableSet = Set<AnyCancellable>()
    private var liveListDataSource: [LiveInfo] = []
    
    lazy var liveListCollectionView: UICollectionView = {
        let horizontalSpacing = 8.0
        let verticalSpacing = 8.0
        let itemWidth = (UIScreen.main.bounds.width - horizontalSpacing * 5) * 0.5
        let flowLayout = UICollectionViewFlowLayout()
        flowLayout.itemSize = CGSize(width: itemWidth, height: itemWidth * 1.65)
        flowLayout.minimumLineSpacing = verticalSpacing
        flowLayout.minimumInteritemSpacing = horizontalSpacing
        flowLayout.scrollDirection = .vertical
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: flowLayout)
        collectionView.contentInset = UIEdgeInsets(top: 10, left: horizontalSpacing * 2, bottom: 10, right: horizontalSpacing * 2)
        collectionView.register(LiveListCell.self, forCellWithReuseIdentifier: LiveListCell.reuseIdentify)
        collectionView.backgroundColor = .clear
        collectionView.translatesAutoresizingMaskIntoConstraints = false
        collectionView.alwaysBounceVertical = true
        return collectionView
    }()
    
    init(manager: LiveListManager) {
        self.manager = manager
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        addRefreshDataEvent()
        subscribeRoomInfoListState()
        isViewReady = true
    }
}

extension LiveListRootView {
    private func constructViewHierarchy() {
        addSubview(liveListCollectionView)
    }
    
    private func activateConstraints() {
        liveListCollectionView.snp.makeConstraints { make in
            make.left.right.bottom.equalToSuperview()
            make.top.equalTo(safeAreaLayoutGuide.snp.top)
        }
    }
    
    private func bindInteraction() {
        liveListCollectionView.dataSource = self
        liveListCollectionView.delegate = self
    }
    
    private func addRefreshDataEvent() {
        
        let header = ESRefreshHeaderAnimator(frame: CGRect.zero)
        header.pullToRefreshDescription = .pullToRefreshText
        header.releaseToRefreshDescription = .releaseToRefreshText
        header.loadingDescription = .loadingText
        
        let footer = ESRefreshFooterAnimator(frame: CGRect.zero)
        footer.loadingMoreDescription = .loadingMoreText
        footer.noMoreDataDescription = .noMoreDataText
        footer.loadingDescription = .loadingText
        
        liveListCollectionView.es.addPullToRefresh(animator: header) { [weak self] in
            guard let self = self else { return }
            refreshRoomListData()
            self.liveListCollectionView.es.stopPullToRefresh()
        }
        
        liveListCollectionView.es.addInfiniteScrolling(animator: footer) { [weak self] in
            guard let self = self else { return }
            let cursor = manager.state.liveInfoListResult.cursor
            if cursor != "" {
                manager.getLiveList(cursor: cursor)
                self.liveListCollectionView.es.stopLoadingMore()
            }
        }
    }
    
    func refreshRoomListData() {
        manager.getLiveList(cursor: "")
    }
    
    private func subscribeRoomInfoListState() {
        manager.subscribeState(StateSelector(keyPath: \LiveListState.liveInfoListResult))
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] result in
                guard let self = self else { return }
                if result.cursor == "" {
                    self.liveListCollectionView.es.noticeNoMoreData()
                } else {
                    self.liveListCollectionView.es.resetNoMoreData()
                }
                if result.isFirstFetch {
                    self.liveListDataSource.removeAll()
                }
                self.liveListDataSource.append(contentsOf: result.liveInfoList)
                self.liveListCollectionView.reloadData()
            }
            .store(in: &cancellableSet)
    }
}

extension LiveListRootView: UICollectionViewDataSource {
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return liveListDataSource.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: LiveListCell.reuseIdentify, for: indexPath)
        if let roomListCell = cell as? LiveListCell {
            roomListCell.updateView(liveInfo: liveListDataSource[indexPath.item])
        }
        return cell
    }
}

extension LiveListRootView: UICollectionViewDelegate {
    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        let liveInfo = liveListDataSource[indexPath.item]
        manager.routeTo(.toLive(liveInfo))
    }
}

extension String {
    static let pullToRefreshText = localized("Pull to refresh")
    static let releaseToRefreshText = localized("Release to refresh")
    static let loadingText = localized("Loading...")
    static let loadingMoreText = localized("Loading more")
    static let noMoreDataText = localized("There is no more data")
}
