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
    let renderManager:MatrixVideoRenderManager = MatrixVideoRenderManager()
    // MARK: - private property.
    var store: LiveStore
    private lazy var seatListPublisher = store.select(SeatSelectors.getSeatList)
    private lazy var liveStatusPublisher = store.select(ViewSelectors.getLiveStatus)
    private lazy var linkStatusPublisher = store.select(ViewSelectors.getLinkStatus)
    
    private var cancellableSet = Set<AnyCancellable>()
    private var renderUserList: [SeatInfo] = []
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
    
    init(store: LiveStore) {
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
    }

    func activateConstraints() {
        attendeeCollectionView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    func bindInteraction() {
        subscribeState()
    }
    
    func updateView() {
        let list = getRenderUserList()
        let diff = Set(renderUserList).subtracting(Set(list))
        renderManager.removeRenderViews(diff.shuffled())
        renderUserList = list
        videoRenderLayout.renderCount = list.count
        attendeeCollectionView.reloadData()
    }
}

extension MatrixVideoRenderView {
    
    func getRenderUserList() -> [SeatInfo] {
        var list = store.seatState.seatList.filter { !$0.userId.isEmpty}
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
        return list
    }
    
    func subscribeState() {
        seatListPublisher
            .receive(on: RunLoop.main)
            .map({ seatlist in
                return seatlist.filter { !$0.userId.isEmpty }
            })
            .filter { $0.count != 0 }
            .sink { [weak self] seatList in
                guard let self = self else { return }
                self.updateView()
            }
            .store(in: &cancellableSet)
        
        liveStatusPublisher
                .receive(on: RunLoop.main)
                .sink { [weak self] status in
                    guard let self = self else { return }
                    self.updateView()
                }
                .store(in: &cancellableSet)
        
        linkStatusPublisher
                .receive(on: RunLoop.main)
                .sink { [weak self] status in
                    guard let self = self else { return }
                    self.updateView()
                }
                .store(in: &cancellableSet)
        
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
