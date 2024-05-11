//
//  MatrixVideoRenderView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/30.
//

import Foundation

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
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()

    private var liveRoomInfo:LiveRoomInfo {
          engineService.liveRoomInfo
    }
    private var engineService: RoomEngineService
    init(engineService: RoomEngineService) {
        self.engineService = engineService
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .clear
        renderUserList.value = getRenderUserList()
        constructViewHierarchy()
        activateConstraints()
        liveRoomInfo.linkingAudienceList.addObserver(self) { [weak self] _, _ in
            self?.updateView()
        }
        engineService.liveRoomInfo.selfInfo.status.addObserver(self) { [weak self] _, _ in
            self?.updateView()
        }
    }

    private let renderUserList: Observable<[UserInfo]> = Observable([])

    lazy var videoRenderLayout: MatrixVideoRenderLayout = {
        let layout = MatrixVideoRenderLayout(renderUserList: self.renderUserList)
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

    func updateView() {
        let list = getRenderUserList()
        let diff = Set(renderUserList.value).subtracting(Set(list))
        renderManager.removeRenderViews(diff.shuffled())
        renderUserList.value = list
        attendeeCollectionView.reloadData()
    }
}

// MARK: Layout

extension MatrixVideoRenderView {
    func constructViewHierarchy() {
        backgroundColor = .clear
        addSubview(attendeeCollectionView)
    }

    func getRenderUserList() -> [UserInfo] {
        var list: [UserInfo] = Array(liveRoomInfo.linkingAudienceList.value)
        if liveRoomInfo.userLiveStatus.value == .previewing || liveRoomInfo.userLiveStatus.value == .pushing{
            list = list.filter({ $0.userId != engineService.liveRoomInfo.selfInfo.userId })
            list.insert(engineService.liveRoomInfo.selfInfo, at: 0)
        } else if engineService.liveRoomInfo.selfInfo.status.value == .none
            && engineService.liveRoomInfo.selfInfo.role.value == .anchor {
            list = list.filter({ $0.userId != engineService.liveRoomInfo.selfInfo.userId })
            list.insert(engineService.liveRoomInfo.selfInfo, at: 0)
        } else if engineService.liveRoomInfo.selfInfo.status.value == .applying {
            list = list.filter({ $0.userId != engineService.liveRoomInfo.selfInfo.userId })
            list.append(engineService.liveRoomInfo.selfInfo)
        }
        return list
    }

    func activateConstraints() {
        attendeeCollectionView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
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
        return min(renderUserList.value.count, 9)
    }

    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(
            withReuseIdentifier: MatrixVideoRenderCell.cellReuseIdentifier,
            for: indexPath) as! MatrixVideoRenderCell
        if indexPath.item >= renderUserList.value.count {
            return cell
        }
        let userInfo = renderUserList.value[indexPath.item]
        let renderView = renderManager.getRenderView(userInfo, engineService)
        cell.updateRenderView(renderView)
        return cell
    }
}
