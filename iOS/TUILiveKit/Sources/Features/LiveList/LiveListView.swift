//
//  LiveListView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2025/4/15.
//

import UIKit
import ESPullToRefresh
import RTCCommon
import RTCRoomEngine

public class LiveListView: UIView {
    public weak var adapter: LiveListViewAdapter?
    public weak var dataSource: LiveListDataSource?
    public weak var itemClickDelegate: OnItemClickDelegate?
    
    public init(style: LiveListViewStyle) {
        self.currentStyle = style
        super.init(frame: .zero)
        self.adapter = self
        self.dataSource = self
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    public func setColumnStyle(style: LiveListViewStyle) {
        guard currentStyle != style else { return }
        LiveKitLog.info("\(#file)","\(#line)", "setColumnStyle style: \(style)")
        currentStyle = style
        collectionView.contentInsetAdjustmentBehavior = currentStyle == .doubleColumn ? .automatic : .never
        activateConstraints()
        layoutIfNeeded()
        
        if style == .singleColumn {
            let targetIndexPath = mostVisibleIndexPath()
            let singleColumnLayout = UICollectionViewFlowLayout()
            singleColumnLayout.itemSize = CGSize(width: UIScreen.main.bounds.width, height: UIScreen.main.bounds.height)
            singleColumnLayout.minimumInteritemSpacing = 0
            singleColumnLayout.minimumLineSpacing = 0
            collectionView.isPagingEnabled = true
            collectionView.contentInset = .zero
            collectionView.setCollectionViewLayout(singleColumnLayout, animated: true)
            collectionView.layoutIfNeeded()
            if let targetIndexPath = targetIndexPath {
                collectionView.scrollToItem(at: targetIndexPath, at: .top, animated: true)
                currentPageInSingleStyle = targetIndexPath.item
                if let newLiveView = collectionView.cellForItem(at: targetIndexPath) as? LiveListViewCell {
                    newLiveView.startPreload(roomId: liveList[targetIndexPath.item].roomId, isMuteAudio: false)
                }
            }
            doublePlayingIndexPaths.removeAll()
        } else {
            let doubleColumnLayout = UICollectionViewFlowLayout()
            doubleColumnLayout.itemSize = CGSize(width: 168.scale375(), height: 262.scale375Height())
            doubleColumnLayout.minimumLineSpacing = 8.scale375Height()
            doubleColumnLayout.minimumInteritemSpacing = 6.scale375()
            doubleColumnLayout.scrollDirection = .vertical
            collectionView.contentInset = UIEdgeInsets(top: 12.scale375Height(), left: 16.scale375(), bottom: 12.scale375Height(), right: 16.scale375())
            collectionView.isPagingEnabled = false
            collectionView.setCollectionViewLayout(doubleColumnLayout, animated: true)
            collectionView.layoutIfNeeded()
        }
        
        for cell in collectionView.visibleCells {
            if let indexPath = collectionView.indexPath(for: cell), let cell = cell as? LiveListViewCell {
                let liveInfo = liveList[indexPath.item]
                currentStyle == .singleColumn ? cell.addBlurEffect() : cell.removeBlurEffect()
                setLiveInfoView(cell: cell, info: liveInfo)
            }
        }
        DispatchQueue.main.async {
            self.preloadTopFullyVisibleRow()
        }
    }
    
    public func refreshLiveList() {
        let now = Date().timeIntervalSince1970
        guard !isFetchingLiveList, now - lastRefreshTime >= 2 else { return }
        LiveKitLog.info("\(#file)","\(#line)", "refreshLiveList")
        lastRefreshTime = now
        isFirstFetch = true
        isFetchingLiveList = true
        dataSource?.fetchLiveList(cursor: "") { [weak self] cursor, liveList in
            guard let self = self else { return }
            isFetchingLiveList = false
            onFetchLiveListSuccess(cursor: cursor, liveList: liveList)
        } onError: { [weak self] _, _ in
            guard let self = self else { return }
            isFetchingLiveList = false
            preloadTopFullyVisibleRow()
        }
        if FloatWindow.shared.isShowingFloatWindow(),
           let floatRoomId = FloatWindow.shared.getCurrentRoomId(),
           let item = liveList.firstIndex(where: { $0.roomId == floatRoomId }) {
            willEnterRoomIndexPath = IndexPath(item: item, section: 0)
        } else {
            willEnterRoomIndexPath = nil
        }
        isOnCurrentView = true
    }
    
    public func onRouteToNextPage() {
        LiveKitLog.info("\(#file)","\(#line)", "onRouteToNextPage")
        for indexPath in doublePlayingIndexPaths {
            guard indexPath.item < liveList.count,
                  let cell = collectionView.cellForItem(at: indexPath) as? LiveListViewCell,
                  indexPath != willEnterRoomIndexPath
            else { continue }
            cell.stopPreload()
        }
        doublePlayingIndexPaths.removeAll()
        isOnCurrentView = false
    }
    
    private var currentStyle: LiveListViewStyle
    private var liveList: [LiveInfo] = []
    private var cursor = ""
    private var isFirstFetch: Bool = true
    private var isFetchingLiveList: Bool = false
    private var currentPageInSingleStyle = 0
    private var lastRefreshTime: TimeInterval = 0
    private var doublePlayingIndexPaths = Set<IndexPath>()
    private var willEnterRoomIndexPath: IndexPath?
    private var isOnCurrentView = true
    
    private let singleLiveInfoViewTag = 1001
    private let doubleLiveInfoViewTag = 1002
    private let fetchCount = 20
    
    private var isViewReady = false
    public override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        addRefreshDataEvent()
        refreshLiveList()
        isViewReady = true
    }
    
    private lazy var collectionView: UICollectionView = {
        let viewLayout = UICollectionViewFlowLayout()
        if currentStyle == .singleColumn {
            viewLayout.itemSize = CGSize(width: UIScreen.main.bounds.width, height: UIScreen.main.bounds.height)
            viewLayout.minimumInteritemSpacing = 0
            viewLayout.minimumLineSpacing = 0
        } else {
            viewLayout.itemSize = CGSize(width: 168.scale375(), height: 262.scale375Height())
            viewLayout.minimumLineSpacing = 8.scale375Height()
            viewLayout.minimumInteritemSpacing = 6.scale375()
        }
        viewLayout.scrollDirection = .vertical
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: viewLayout)
        collectionView.isPagingEnabled = currentStyle == .singleColumn
        collectionView.contentInset = currentStyle == .singleColumn ? .zero : UIEdgeInsets(top: 12.scale375Height(), left: 16.scale375(), bottom: 12.scale375Height(), right: 16.scale375())
        collectionView.contentInsetAdjustmentBehavior = currentStyle == .singleColumn ? .never : .automatic
        collectionView.register(LiveListViewCell.self, forCellWithReuseIdentifier: LiveListViewCell.cellReuseIdentifier)
        collectionView.backgroundColor = .clear
        collectionView.translatesAutoresizingMaskIntoConstraints = false
        collectionView.showsVerticalScrollIndicator = false
        collectionView.alwaysBounceVertical = true
        collectionView.scrollsToTop = false
        return collectionView
    }()
    
    deinit {
        for cell in collectionView.visibleCells {
            if let liveCell = cell as? LiveListViewCell {
                liveCell.stopPreload()
            }
        }
    }
}

// MARK: - Private
extension LiveListView {
    private func constructViewHierarchy() {
        addSubview(collectionView)
    }
    
    private func activateConstraints() {
        collectionView.snp.remakeConstraints { make in
            make.left.right.bottom.equalToSuperview()
            if currentStyle == .doubleColumn {
                make.top.equalTo(safeAreaLayoutGuide.snp.top)
            } else {
                make.top.equalToSuperview()
            }
        }
    }
    
    private func bindInteraction() {
        collectionView.dataSource = self
        collectionView.delegate = self
    }
    
    private func fetchLiveList() {
        guard !isFetchingLiveList else { return }
        isFetchingLiveList = true
        dataSource?.fetchLiveList(cursor: cursor) { [weak self] cursor, liveList in
            guard let self = self else { return }
            isFetchingLiveList = false
            onFetchLiveListSuccess(cursor: cursor, liveList: liveList)
        } onError: { [weak self] _, _ in
            guard let self = self else { return }
            isFetchingLiveList = false
        }
    }
    
    private func onFetchLiveListSuccess(cursor: String, liveList: [LiveInfo]) {
        LiveKitLog.info("\(#file)","\(#line)", "onFetchLiveListSuccess")
        if cursor == "" {
            collectionView.es.noticeNoMoreData()
        } else {
            collectionView.es.resetNoMoreData()
        }
        if isFirstFetch {
            self.liveList.removeAll()
        }
        self.cursor = cursor
        self.liveList.append(contentsOf: liveList)
        isFirstFetch = false
        guard isOnCurrentView else { return }
        collectionView.reloadData()
        collectionView.layoutIfNeeded()
        preloadTopFullyVisibleRow()
    }
    
    private func preloadTopFullyVisibleRow() {
        guard currentStyle == .doubleColumn, isOnCurrentView else { return }
        
        LiveKitLog.info("\(#file)","\(#line)", "preloadTopFullyVisibleRow")
        let visibleIndexPaths = collectionView.indexPathsForVisibleItems
        guard !visibleIndexPaths.isEmpty else { return }
        let topFullyVisibleRowIndexPaths = getTopFullyVisibleRowIndexPaths()
        
        guard !topFullyVisibleRowIndexPaths.isEmpty else {
            for indexPath in visibleIndexPaths {
                if let cell = collectionView.cellForItem(at: indexPath) as? LiveListViewCell {
                    cell.stopPreload()
                    doublePlayingIndexPaths.remove(indexPath)
                }
            }
            return
        }
        
        for indexPath in visibleIndexPaths {
            guard let cell = collectionView.cellForItem(at: indexPath) as? LiveListViewCell else { continue }
            if topFullyVisibleRowIndexPaths.contains(indexPath) {
                guard !doublePlayingIndexPaths.contains(indexPath) else { continue }
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) { [weak self, weak cell] in
                    guard let self = self, let cell = cell,
                          self.isOnCurrentView,
                          !doublePlayingIndexPaths.contains(indexPath),
                          self.collectionView.cellForItem(at: indexPath) === cell,
                          self.getTopFullyVisibleRowIndexPaths().contains(indexPath)
                    else { return }
                    cell.startPreload(roomId: self.liveList[indexPath.item].roomId)
                    doublePlayingIndexPaths.insert(indexPath)
                }
            } else {
                cell.stopPreload()
                doublePlayingIndexPaths.remove(indexPath)
            }
        }
    }
    
    private func getTopFullyVisibleRowIndexPaths() -> [IndexPath] {
        let visibleIndexPaths = collectionView.indexPathsForVisibleItems
        let visibleRect = CGRect(origin: collectionView.contentOffset, size: collectionView.bounds.size)
        let fullyVisibleIndexPaths = visibleIndexPaths.filter { indexPath in
            if let cell = collectionView.cellForItem(at: indexPath) {
                return visibleRect.contains(cell.frame)
            }
            return false
        }
        guard !fullyVisibleIndexPaths.isEmpty else { return [] }
        let minY = fullyVisibleIndexPaths
            .compactMap { collectionView.cellForItem(at: $0)?.frame.origin.y }
            .min() ?? 0
        return fullyVisibleIndexPaths.filter {
            guard let cell = collectionView.cellForItem(at: $0) else { return false }
            return abs(cell.frame.origin.y - minY) < 1
        }
    }
    
    private func mostVisibleIndexPath() -> IndexPath? {
        let visibleIndexPaths = collectionView.indexPathsForVisibleItems
        guard !visibleIndexPaths.isEmpty else { return nil }
        let visibleRect = CGRect(origin: collectionView.contentOffset, size: collectionView.bounds.size)
        let fullyVisibleIndexPaths = visibleIndexPaths.filter { indexPath in
            if let cell = collectionView.cellForItem(at: indexPath) {
                return visibleRect.contains(cell.frame)
            }
            return false
        }
        return fullyVisibleIndexPaths.min()
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
        
        collectionView.es.addPullToRefresh(animator: header) { [weak self] in
            guard let self = self else { return }
            refreshLiveList()
            self.collectionView.es.stopPullToRefresh()
        }
        
        collectionView.es.addInfiniteScrolling(animator: footer) { [weak self] in
            guard let self = self, cursor != "" else { return }
            fetchLiveList()
            self.collectionView.es.stopLoadingMore()
        }
    }
}

extension LiveListView: UICollectionViewDelegate {
    public func collectionView(_ collectionView: UICollectionView, willDisplay cell: UICollectionViewCell, forItemAt indexPath: IndexPath) {
        guard let cell = cell as? LiveListViewCell, isOnCurrentView else { return }
        if currentStyle == .singleColumn {
            let isMute = currentPageInSingleStyle != indexPath.item
            cell.startPreload(roomId: liveList[indexPath.item].roomId, isMuteAudio: isMute)
        }
    }

    public func collectionView(_ collectionView: UICollectionView, didEndDisplaying cell: UICollectionViewCell, forItemAt indexPath: IndexPath) {
        guard let cell = cell as? LiveListViewCell, indexPath.item < liveList.count, isOnCurrentView else { return }
        cell.stopPreload()
        doublePlayingIndexPaths.remove(indexPath)
    }
    
    public func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        guard indexPath.item < liveList.count, let cell = collectionView.cellForItem(at: indexPath) as? LiveListViewCell else { return }
        let originFrame = cell.convert(cell.bounds, to: window)
        itemClickDelegate?.onItemClick(liveInfo: liveList[indexPath.item], frame: originFrame)
        willEnterRoomIndexPath = indexPath
    }
}

extension LiveListView: UICollectionViewDataSource {
    public func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return liveList.count
    }
    
    public func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: LiveListViewCell.cellReuseIdentifier, for: indexPath)
        if let liveListViewCell = cell as? LiveListViewCell {
            let info = liveList[indexPath.item]
            liveListViewCell.updateView(liveInfo: info)
            if currentStyle == .singleColumn {
                liveListViewCell.addBlurEffect()
            }
            setLiveInfoView(cell: liveListViewCell, info: info)
        }
        return cell
    }
    
    private func setLiveInfoView(cell: LiveListViewCell, info: LiveInfo) {
        guard let adapter = adapter else { return }
        let singleInfoView = cell.contentView.viewWithTag(singleLiveInfoViewTag)
        let doubleInfoView = cell.contentView.viewWithTag(doubleLiveInfoViewTag)

        if currentStyle == .singleColumn {
            doubleInfoView?.safeRemoveFromSuperview()
            if let singleView = singleInfoView {
                adapter.updateLiveInfoView(view: singleView, info: info)
            } else {
                let liveInfoView = adapter.createLiveInfoView(info: info)
                liveInfoView.isUserInteractionEnabled = false
                liveInfoView.tag = singleLiveInfoViewTag
                cell.contentView.addSubview(liveInfoView)
                liveInfoView.snp.makeConstraints { make in
                    make.edges.equalToSuperview()
                }
            }
        } else {
            singleInfoView?.safeRemoveFromSuperview()
            if let doubleView = doubleInfoView {
                adapter.updateLiveInfoView(view: doubleView, info: info)
            } else {
                let liveInfoView = adapter.createLiveInfoView(info: info)
                liveInfoView.isUserInteractionEnabled = false
                liveInfoView.tag = doubleLiveInfoViewTag
                cell.contentView.addSubview(liveInfoView)
                liveInfoView.snp.makeConstraints { make in
                    make.edges.equalToSuperview()
                }
            }
        }
    }
}

extension LiveListView: UIScrollViewDelegate {
    public func scrollViewDidEndDecelerating(_ scrollView: UIScrollView) {
        preloadTopFullyVisibleRow()
       
        guard currentStyle == .singleColumn else { return }
        let pageHeight = collectionView.frame.height
        let currentOffset = collectionView.contentOffset.y
        let newPage = Int(currentOffset / pageHeight)
        
        if newPage != currentPageInSingleStyle {
            let newIndexPath = IndexPath(item: newPage, section: 0)
            if let newLiveView = collectionView.cellForItem(at: newIndexPath) as? LiveListViewCell {
                newLiveView.startPreload(roomId: liveList[newPage].roomId, isMuteAudio: false)
            }
        }
        
        currentPageInSingleStyle = newPage
    }
    
    public func scrollViewDidEndDragging(_ scrollView: UIScrollView, willDecelerate decelerate: Bool) {
        if !decelerate {
            preloadTopFullyVisibleRow()
        }
    }
}

extension LiveListView: LiveListDataSource {
    public func fetchLiveList(cursor: String, onSuccess: @escaping LiveListBlock, onError: @escaping TUIErrorBlock) {
        let liveListManager = TUIRoomEngine.sharedInstance().getExtension(extensionType: .liveListManager) as? TUILiveListManager
        liveListManager?.fetchLiveList(cursor: cursor, count: fetchCount) { cursor, list in
            let liveInfoList = list.map { tuiLiveInfo in
                LiveInfo(tuiLiveInfo: tuiLiveInfo)
            }
            onSuccess(cursor, liveInfoList)
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            let error = InternalError(code: code.rawValue, message: message)
            makeToast(error.localizedMessage)
            LiveKitLog.error("\(#file)","\(#line)","fetchLiveList:[onError:[code:\(code),message:\(message)]]")
            onError(code, message)
        }
    }
}

extension LiveListView: LiveListViewAdapter {
    public func createLiveInfoView(info: LiveInfo) -> UIView {
        return currentStyle == .singleColumn ? SingleColumnWidgetView(liveInfo: info)
                                             : DoubleColumnWidgetView(liveInfo: info)
    }
    
    public func updateLiveInfoView(view: UIView, info: LiveInfo) {
        if let infoView = view as? DoubleColumnWidgetView {
            infoView.updateView(liveInfo: info)
        } else if let infoView = view as? SingleColumnWidgetView {
            infoView.updateView(liveInfo: info)
        }
    }
}

extension String {
    static let pullToRefreshText = internalLocalized("Pull to refresh")
    static let releaseToRefreshText = internalLocalized("Release to refresh")
    static let loadingText = internalLocalized("Loading...")
    static let loadingMoreText = internalLocalized("Loading more")
    static let noMoreDataText = internalLocalized("There is no more data")
}
