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

class LiveListRootView: UIView {
    
    private var store: LiveListStore
    
    private var cancellableSet = Set<AnyCancellable>()
    private var liveListDataSource: [TUILiveInfo] = []
    
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
    
    init(store: LiveListStore) {
        self.store = store
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
            let cursor = self.store.selectCurrent(LiveListSelectors.getLiveInfoList).cursor
            if cursor != "" {
                self.store.dispatch(action: LiveListActions.getLiveInfoList(payload: cursor))
                self.liveListCollectionView.es.stopLoadingMore()
            }
        }
    }
    
    func refreshRoomListData() {
        liveListDataSource.removeAll()
        liveListCollectionView.reloadData()
        store.dispatch(action: LiveListActions.getLiveInfoList(payload: ""))
    }
    
    private func subscribeRoomInfoListState() {
        store.select(LiveListSelectors.getLiveInfoList)
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] result in
                guard let self = self else { return }
                if result.cursor == "" {
                    self.liveListCollectionView.es.noticeNoMoreData()
                } else {
                    self.liveListCollectionView.es.resetNoMoreData()
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
        store.dispatch(action: LiveListNavigatorActions.navigatorTo(payload: .toLive(liveInfo)))
    }
}

extension String {
    static let pullToRefreshText = localized("live.room.list.pullToRefresh")
    static let releaseToRefreshText = localized("live.room.list.releaseToRefresh")
    static let loadingText = localized("live.room.list.loading")
    static let loadingMoreText = localized("live.room.list.loadingMore")
    static let noMoreDataText = localized("live.room.list.noMoreData")
    static let startLiveRoomText = localized("live.room.list.startLiveRoom")
}
