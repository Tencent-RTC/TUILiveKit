//
//  RoomListRootView.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/5/30.
//

import UIKit
import ESPullToRefresh
import Combine
import RTCRoomEngine

class RoomListRootView: UIView {
    
    @Injected private var store: RoomListStoreProvider
    
    private var cancellableSet = Set<AnyCancellable>()
    private var roomListDataSource: [TUILiveInfo] = []
    
    lazy var roomListCollectionView: UICollectionView = {
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
        collectionView.register(RoomListCell.self, forCellWithReuseIdentifier: RoomListCell.reuseIdentify)
        collectionView.backgroundColor = .clear
        collectionView.translatesAutoresizingMaskIntoConstraints = false
        collectionView.alwaysBounceVertical = true
        return collectionView
    }()
    
    lazy var navigationView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .clear
        return view
    }()
    
    lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .black
        label.textAlignment = .center
        label.text = .liveTitleText
        return label
    }()
    
    lazy var backButton: UIButton = {
        let backBtn = UIButton(type: .custom)
        backBtn.setImage(.liveBundleImage("live_back"), for: .normal)
        return backBtn
    }()
    
    lazy var helpButton: UIButton = {
        let helpBtn = UIButton(type: .custom)
        helpBtn.setImage(.liveBundleImage("help_small"), for: .normal)
        return helpBtn
    }()
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        addRefreshDataEvent()
        subscribeRoomInfoListState()
        backgroundColor = UIColor(hex:"F2F5FC")
        isViewReady = true
    }
}

extension RoomListRootView {
    private func constructViewHierarchy() {
        addSubview(roomListCollectionView)
        addSubview(navigationView)
        navigationView.addSubview(backButton)
        navigationView.addSubview(helpButton)
        navigationView.addSubview(titleLabel)
    }
    
    private func activateConstraints() {
        navigationView.snp.makeConstraints { make in
            make.left.right.equalToSuperview()
            make.top.equalTo(safeAreaLayoutGuide.snp.top)
            make.height.equalTo(44)
        }
        
        backButton.snp.makeConstraints { make in
            make.left.equalToSuperview().offset(10)
            make.centerY.equalToSuperview()
            make.size.equalTo(CGSize(width: 30, height: 30))
        }
        
        helpButton.snp.makeConstraints { make in
            make.right.equalToSuperview().offset(-10)
            make.centerY.equalToSuperview()
            make.size.equalTo(CGSize(width: 30, height: 30))
        }
        
        titleLabel.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.centerY.equalToSuperview()
            make.width.equalToSuperview().multipliedBy(0.5)
        }
        
        roomListCollectionView.snp.makeConstraints { make in
            make.left.right.bottom.equalToSuperview()
            make.top.equalTo(navigationView.snp.bottom)
        }
    }
    
    private func bindInteraction() {
        roomListCollectionView.dataSource = self
        roomListCollectionView.delegate = self
        backButton.addTarget(self, action: #selector(backBtnClick), for: .touchUpInside)
        helpButton.addTarget(self, action: #selector(helpBtnClick), for: .touchUpInside)
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
        
        roomListCollectionView.es.addPullToRefresh(animator: header) { [weak self] in
            guard let self = self else { return }
            refreshRoomListData()
            self.roomListCollectionView.es.stopPullToRefresh()
        }
        
        roomListCollectionView.es.addInfiniteScrolling(animator: footer) { [weak self] in
            guard let self = self else { return }
            let cursor = self.store.selectCurrent(RoomListSelectors.getRoomInfoList).cursor
            if cursor != "" {
                self.store.dispatch(action: RoomListActions.getRoomInfoList(payload: cursor))
                self.roomListCollectionView.es.stopLoadingMore()
            }
        }
        
        roomListCollectionView.es.startPullToRefresh()
    }
    
    private func refreshRoomListData() {
        roomListDataSource.removeAll()
        store.dispatch(action: RoomListActions.updateRoomInfoList(payload: RoomListResult(cursor: "",roomInfoList: [])))
        let cursor = store.selectCurrent(RoomListSelectors.getRoomInfoList).cursor
        store.dispatch(action: RoomListActions.getRoomInfoList(payload: cursor))
    }
    
    private func subscribeRoomInfoListState() {
        store.select(RoomListSelectors.getRoomInfoList)
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] result in
                guard let self = self else { return }
                if result.cursor == "" {
                    self.roomListCollectionView.es.noticeNoMoreData()
                } else {
                    self.roomListCollectionView.es.resetNoMoreData()
                }
                self.roomListDataSource.append(contentsOf: result.roomInfoList)
                self.roomListCollectionView.reloadData()
            }
            .store(in: &cancellableSet)
    }
}

extension RoomListRootView {
    @objc
    private func backBtnClick(sender: UIButton) {
        store.dispatch(action: RoomListNavigatorActions.navigatorTo(payload: .exit))
    }
    
    @objc
    private func helpBtnClick(sender: UIButton) {
        if let url = URL(string: "https://cloud.tencent.com/document/product/647/105441") {
            UIApplication.shared.open(url, options: [:], completionHandler: nil)
        }
    }
}

extension RoomListRootView: UICollectionViewDataSource {
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return roomListDataSource.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: RoomListCell.reuseIdentify, for: indexPath)
        if let roomListCell = cell as? RoomListCell {
            roomListCell.updateView(liveInfo: roomListDataSource[indexPath.item])
        }
        return cell
    }
}

extension RoomListRootView: UICollectionViewDelegate {
    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        let liveInfo = roomListDataSource[indexPath.item]
        store.dispatch(action: RoomListNavigatorActions.navigatorTo(payload: .toLive(liveInfo)))
    }
}

extension RoomListRootView {
    static let session = ResolverScopeCache()
}

extension String {
    static let liveTitleText = localized("live.room.list.live")
    static let pullToRefreshText = localized("live.room.list.pullToRefresh")
    static let releaseToRefreshText = localized("live.room.list.releaseToRefresh")
    static let loadingText = localized("live.room.list.loading")
    static let loadingMoreText = localized("live.room.list.loadingMore")
    static let noMoreDataText = localized("live.room.list.noMoreData")
    static let startLiveRoomText = localized("live.room.list.startLiveRoom")
}
