//
//  TUILiveAudienceViewController.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/18.
//

import UIKit
import Combine
import RTCRoomEngine

public class TUILiveAudienceViewController: UIViewController {
    private let liveListStore: LiveListStoreProvider
    private let routerStore: RouterStoreProvider = RouterStoreProvider()
    private let store: LiveStoreProvider
    private lazy var routerCenter: RouterControlCenter = {
        let routerCenter = RouterControlCenter(rootViewController: self,
                                               store: store,
                                               rootRoute: .audience,
                                               routerStore: routerStore)
        return routerCenter
    }()
    
    private var displayLiveInfo: TUILiveInfo
    
    private lazy var liveInfoListPublisher = self.liveListStore.select(LiveListSelectors.getLiveInfoList)
    
    private var linkStatusPublisher: AnyPublisher<LinkStatus, Never>?
    private var linkStatusCancellable: AnyCancellable?
    
    private var cancellableSet = Set<AnyCancellable>()
    private var liveInfoList: [TUILiveInfo] = []
    private var isFirstLoad: Bool = true
    
    private let displayCollectionView: UICollectionView = {
        let flowLayout = UICollectionViewFlowLayout()
        flowLayout.scrollDirection = .vertical
        flowLayout.itemSize = UIScreen.main.bounds.size
        flowLayout.minimumLineSpacing = 0
        flowLayout.minimumLineSpacing = 0
        
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: flowLayout)
        collectionView.isPagingEnabled = true
        collectionView.showsVerticalScrollIndicator = false
        collectionView.contentInsetAdjustmentBehavior = .never
        collectionView.register(TUILiveDisplayViewCell.self, forCellWithReuseIdentifier: TUILiveDisplayViewCell.reuseIdentifier)
        collectionView.backgroundColor = .clear
        return collectionView
    }()
    
    init(liveInfo: TUILiveInfo, liveListStore: LiveListStoreProvider) {
        self.displayLiveInfo = liveInfo
        self.liveListStore = liveListStore
        self.store = LiveStoreFactory.getLiveStore(roomId: liveInfo.roomInfo.roomId)
        super.init(nibName: nil, bundle: nil)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        LiveStoreFactory.removeAllStore()
        LiveRoomViewStoreFactory.removeAllStore()
        VoiceRoomViewStoreFactory.removeAllStore()
        print("deinit \(type(of: self))")
    }
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        view.backgroundColor = .black
    }
    
    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(true, animated: false)
    }
    
    public override func viewDidDisappear(_ animated: Bool) {
        super.viewDidDisappear(animated)
        navigationController?.setNavigationBarHidden(false, animated: false)
    }
}

extension TUILiveAudienceViewController {
    
    private func constructViewHierarchy() {
        view.addSubview(displayCollectionView)
    }
    
    private func activateConstraints() {
        displayCollectionView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func bindInteraction() {
        displayCollectionView.delegate = self
        displayCollectionView.dataSource = self
        subscribeLiveListState()
        subscribeLiveLinkState(store: store)
        routerCenter.subscribeRouter()
    }
    
    private func subscribeLiveListState() {
        liveInfoListPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] result in
                guard let self = self else { return }
                let filterLiveInfoList = result.liveInfoList.filter { [weak self] liveInfo in
                    guard let self = self else { return true }
                    return liveInfo.roomInfo.roomId != self.displayLiveInfo.roomInfo.roomId
                }
                if self.liveInfoList.count == 0 {
                    self.liveInfoList.append(self.displayLiveInfo)
                }
                self.liveInfoList.append(contentsOf: filterLiveInfoList)
                
                CATransaction.begin()
                CATransaction.setCompletionBlock { [weak self] in
                    guard let self = self else { return }
                    if isFirstLoad {
                        self.scrollViewDidEndDecelerating(self.displayCollectionView)
                        isFirstLoad = false
                    }
                }
                self.displayCollectionView.reloadData()
                CATransaction.commit()
            }
            .store(in: &cancellableSet)
    }
    
    private func subscribeLiveLinkState(store: LiveStore) {
        guard linkStatusCancellable == nil else { return }
        linkStatusPublisher = store.select(ViewSelectors.getLinkStatus)
        linkStatusCancellable = linkStatusPublisher?
            .receive(on: RunLoop.main)
            .sink { [weak self] linkStatus in
                guard let self = self else { return }
                        if linkStatus == .applying || linkStatus == .linking {
                    self.displayCollectionView.isScrollEnabled = false
                } else {
                    self.displayCollectionView.isScrollEnabled = true
                }
            }
    }
    
    private func unSubscribeLiveLinkState() {
        if let linkStatusCancellable = linkStatusCancellable {
            linkStatusCancellable.cancel()
        }
        linkStatusCancellable = nil
    }
}

extension TUILiveAudienceViewController: UICollectionViewDelegate {
    public func collectionView(_ collectionView: UICollectionView, willDisplay cell: UICollectionViewCell, forItemAt indexPath: IndexPath) {
        if liveInfoList.count - indexPath.item <= 3 {
            let cursor = liveListStore.selectCurrent(LiveListSelectors.getLiveInfoList).cursor
            if cursor != "" {
                liveListStore.dispatch(action: LiveListActions.getLiveInfoList(payload: cursor))
            }
        }
        if let displayViewCell = cell as? TUILiveDisplayViewCell {
            displayViewCell.prepareDisplay()
        }
    }
    
    public func collectionView(_ collectionView: UICollectionView, didEndDisplaying cell: UICollectionViewCell, forItemAt indexPath: IndexPath) {
        if let displayViewCell = cell as? TUILiveDisplayViewCell {
            displayViewCell.stopDisplay()
        }
    }
}

extension TUILiveAudienceViewController: UICollectionViewDataSource {
    public func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return liveInfoList.count
    }
    
    public func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: TUILiveDisplayViewCell.reuseIdentifier, for: indexPath)
        if let displayCell = cell as? TUILiveDisplayViewCell {
            let liveInfo = liveInfoList[indexPath.item]
            displayCell.joinRoom(roomId: liveInfo.roomInfo.roomId, routerStore: routerStore)
        }
        return cell
    }
}

extension TUILiveAudienceViewController: UIScrollViewDelegate {
    public func scrollViewDidEndDecelerating(_ scrollView: UIScrollView) {
        let row = scrollView.contentOffset.y / max(scrollView.bounds.height, 1)
        let indexPath = IndexPath(row: Int(row), section: 0)
        if let displayCell = displayCollectionView.cellForItem(at: indexPath) as? TUILiveDisplayViewCell {
            let store = LiveStoreFactory.getLiveStore(roomId: displayCell.roomId)
            routerCenter.updateStore(rootRoute: .audience, store: store)
            displayCell.startDisplay(routerCenter: routerCenter)
            unSubscribeLiveLinkState()
            subscribeLiveLinkState(store: store)
        }
    }
}

class TUILiveDisplayViewCell: UICollectionViewCell {
    static let reuseIdentifier = "TUILiveDisplayView"
    var roomId: String = ""
    var view: UIView = UIView()
    override init(frame: CGRect) {
        super.init(frame: frame)
        backgroundColor = .clear
        contentView.backgroundColor = .clear
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func prepareForReuse() {
        super.prepareForReuse()
        finishDisplay()
    }
    
    func joinRoom(roomId: String, routerStore: RouterStore) {
        let store = LiveStoreFactory.getLiveStore(roomId: roomId)
        self.roomId = roomId
        
        if !store.selectCurrent(RoomSelectors.getRoomId).isEmpty {
            return
        }
        
        contentView.subviews.forEach { subView in
            subView.removeFromSuperview()
        }
        
        guard let roomType = LiveIdentityGenerator.shared.getIDType(roomId) else { return }
        switch roomType {
        case .live:
            view = AudienceView(roomId: roomId, routerStore: routerStore)
            contentView.addSubview(view)
            view.snp.makeConstraints { make in
                make.edges.equalToSuperview()
            }
        case .voice:
            view = VoiceRoomRootView(frame: .zero, roomId: roomId, routerStore: routerStore)
            contentView.addSubview(view)
            view.snp.makeConstraints { make in
                make.edges.equalToSuperview()
            }
        }
        store.dispatch(action: MediaActions.muteAllRemoteAudio(payload: true))
    }
    
    func prepareDisplay() {
        let store = LiveStoreFactory.getLiveStore(roomId: roomId)
        store.dispatch(action: MediaActions.muteAllRemoteAudio(payload: true))
    }
    
    func startDisplay(routerCenter: RouterControlCenter) {
        let store = LiveStoreFactory.getLiveStore(roomId: roomId)
        store.dispatch(action: MediaActions.muteAllRemoteAudio(payload: false))
        if let view = view as? AudienceView {
            routerCenter.routerProvider = view
        }
        
        if let view = view as? VoiceRoomRootView {
            routerCenter.routerProvider = view
        }
    }
    
    func stopDisplay() {
        let store = LiveStoreFactory.getLiveStore(roomId: roomId)
        store.dispatch(action: MediaActions.muteAllRemoteAudio(payload: true))
    }
    
    func finishDisplay() {
        let store = LiveStoreFactory.getLiveStore(roomId: roomId)
        store.dispatch(action: RoomActions.leave())
        store.dispatch(action: OperationActions.clearAllState())
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
}
