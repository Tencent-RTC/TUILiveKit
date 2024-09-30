//
//  MatrixVideoRenderView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/30.
//
import Combine
import Foundation
import RTCCommon

class MatrixVideoRenderCell: UICollectionViewCell {
    private var renderView: RenderView?
    func updateRenderView(_ renderView: RenderView) {
        if self.renderView?.superview == contentView {
            self.renderView?.removeFromSuperview()
        }
        self.renderView = renderView
        contentView.addSubview(renderView)
        renderView.snp.remakeConstraints({ make in
            make.edges.equalToSuperview()
        })
    }
}

class MatrixVideoRenderView: UIView {
    lazy var renderManager:MatrixVideoRenderManager = store.renderManager
    // MARK: - private property.
    var store: LiveStoreProvider

    private lazy var liveStatusPublisher = store.select(ViewSelectors.getLiveStatus)
    private lazy var linkStatusPublisher = store.select(ViewSelectors.getLinkStatus)
    
    private lazy var seatListPublisher = store.select(SeatSelectors.getSeatList)
    private lazy var connectedUsersPublisher = store.select(ConnectionSelectors.getConnectedUsers)
    private lazy var battleUsersPublisher = store.select(BattleSelectors.getBattleUsers)

    private var cancellableSet = Set<AnyCancellable>()
    private var renderUserList: [VideoRenderModel] = []
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()
    
    lazy var videoRenderLayout: MatrixVideoRenderLayout = {
        let layout = MatrixVideoRenderLayout()
        return layout
    }()

    lazy var attendeeCollectionView: UICollectionView = {
        let collection = UICollectionView(frame: .zero,collectionViewLayout:videoRenderLayout)
        collection.register(MatrixVideoRenderCell.self, forCellWithReuseIdentifier: MatrixVideoRenderCell.cellReuseIdentifier)
        collection.isPagingEnabled = false
        collection.showsVerticalScrollIndicator = false
        collection.showsHorizontalScrollIndicator = false
        collection.isUserInteractionEnabled = true
        collection.contentMode = .scaleToFill
        collection.backgroundColor = .clear
        collection.isPrefetchingEnabled = true
        collection.contentInsetAdjustmentBehavior = .never
        collection.dataSource = self
        collection.delegate = self
        return collection
    }()
    
    private lazy var battleInfoView: BattleInfoView = {
        let view = BattleInfoView(store: store)
        return view
    }()
    
    init(store: LiveStoreProvider) {
        self.store = store
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
    }

    func constructViewHierarchy() {
        addSubview(attendeeCollectionView)
        addSubview(battleInfoView)
    }

    func activateConstraints() {
        attendeeCollectionView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        battleInfoView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    func bindInteraction() {
        subscribeLiveState()
    }
    
    func updateView(list:[VideoRenderModel]) {
        let diff = Set(renderUserList).subtracting(Set(list))
        renderManager.removeRenderViews(diff.shuffled())
        renderUserList = list
        videoRenderLayout.renderCount = list.count
        attendeeCollectionView.reloadData()
    }
}

extension MatrixVideoRenderView {
    
    func subscribeLiveState() {
        liveStatusPublisher
            .combineLatest(linkStatusPublisher, seatListPublisher, connectedUsersPublisher)
            .receive(on: RunLoop.main)
            .sink { [weak self] liveStatus, linkStatus, seatList, connectedUsers in
                guard let self = self else { return }
                if connectedUsers.count > 0 {
                    self.updateView(list: getVideoRenderVideoModelList(from: connectedUsers))
                } else {
                    self.updateView(list: getVideoRenderVideoModelList(from: seatList))
                }
            }
            .store(in: &cancellableSet)
    }
    
    func subscribeBattleState() {
        battleUsersPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] battleUsers in
                guard let self = self else { return }
                self.handleBattleUserListChanged(battleUsers)
            }
            .store(in: &cancellableSet)
    }
    
    private func handleBattleUserListChanged(_ battleUsers: [BattleUser]) {
        if battleUsers.count > 2 {
            bringSubviewToFront(battleInfoView)
        }
    }
    
    private func getVideoRenderVideoModelList(from connectedUsers: [ConnectionUser]) -> [VideoRenderModel] {
        var list = connectedUsers.map { user in
            return VideoRenderModel(connectionUser: user)
        }
        if let index = list.firstIndex(where: {$0.roomId == store.roomState.roomId}) {
            let currentUser = list.remove(at: index)
            list.insert(currentUser, at: 0)
        }
        return list
    }
    
    private func getVideoRenderVideoModelList(from seatList: [SeatInfo]) -> [VideoRenderModel] {
        var list = seatList.filter { !$0.userId.isEmpty}
        if store.viewState.liveStatus == .previewing || store.viewState.liveStatus == .pushing {
            list = list.filter({ [weak self] in
                guard let self = self else { return false }
                return $0.userId != self.store.userState.selfInfo.userId
            })
            list.insert(SeatInfo(userInfo: store.userState.selfInfo), at: 0)
        } else if store.viewState.liveStatus == .playing && store.viewState.linkStatus == .applying {
            list = list.filter({ [weak self] in
                guard let self = self else { return false }
                return $0.userId != store.userState.selfInfo.userId
            })
            list.append(SeatInfo(userInfo: store.userState.selfInfo))
        }
        return list.map { seat in
            var renderModel = VideoRenderModel(seatInfo: seat)
            renderModel.roomId = store.roomState.roomId
            return renderModel
        }
    }
}

// MARK: - UICollectionViewDelegateFlowLayout
extension MatrixVideoRenderView: UICollectionViewDelegateFlowLayout,
    UIScrollViewDelegate,
    UICollectionViewDataSource {
    func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 1
    }

    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return min(renderUserList.count, 9)
    }

    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(
            withReuseIdentifier: MatrixVideoRenderCell.cellReuseIdentifier,
            for: indexPath) as! MatrixVideoRenderCell
        if indexPath.item >= renderUserList.count {
            return cell
        }
        let seatInfo = renderUserList[indexPath.item]
        let renderView = renderManager.getRenderView(seatInfo, store: store)
        cell.updateRenderView(renderView)
        return cell
    }
}
