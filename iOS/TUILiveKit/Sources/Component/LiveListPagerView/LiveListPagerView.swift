//
//  LiveListPagerView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2025/1/9.
//

import SnapKit
import RTCRoomEngine

public protocol LiveListViewDataSource: AnyObject {
    typealias LiveListCallback = ([LiveInfo]) -> Void
    func fetchLiveList(completionHandler: @escaping LiveListCallback)
}

public protocol LiveListViewDelegate: AnyObject {
    func onCreateView(liveInfo: LiveInfo) -> UIView
    
    func onViewWillSlideIn(view: UIView)
    func onViewDidSlideIn(view: UIView)
    func onViewSlideInCancelled(view: UIView)
    func onViewWillSlideOut(view: UIView)
    func onViewDidSlideOut(view: UIView)
    func onViewSlideOutCancelled(view: UIView)
}

public class LiveListPagerView: UIView {
    public weak var dataSource: LiveListViewDataSource?
    public weak var delegate: LiveListViewDelegate?
    
    private var isViewReady = false
    private var currentPage = 0
    private var liveList: [LiveInfo] = []
    private var willDisplayPage: IndexPath? = nil
    private let cellReuseIdentifier = "LiveListCell"
    private var isFetchingLiveList = false
    
    public init() {
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private lazy var tableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .plain)
        tableView.isPagingEnabled = true
        tableView.showsVerticalScrollIndicator = false
        tableView.contentInsetAdjustmentBehavior = .never
        tableView.register(UITableViewCell.self, forCellReuseIdentifier: cellReuseIdentifier)
        tableView.scrollsToTop = false
        tableView.isScrollEnabled = false
        tableView.backgroundColor = .clear
        return tableView
    }()
    
    override public func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        fetchLiveList()
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }
    
    override public func willMove(toWindow newWindow: UIWindow?) {
        super.willMove(toWindow: newWindow)
        if newWindow == nil {
            onLastCellSlideOut()
        }
    }
    
    private func constructViewHierarchy() {
        addSubview(tableView)
    }
    
    private func activateConstraints() {
        tableView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func bindInteraction() {
        tableView.delegate = self
        tableView.dataSource = self
    }
    
    private func fetchLiveList() {
        dataSource?.fetchLiveList { [weak self] liveList in
            guard let self = self else { return }
            self.liveList.append(contentsOf: liveList)
            DispatchQueue.main.async { [weak self] in
                self?.tableView.reloadData()
                DispatchQueue.main.async { [weak self] in
                    self?.onFirstCellSlideIn()
                }
            }
        }
    }
    
    private func onFirstCellSlideIn() {
        if self.liveList.count > 0 {
            let indexPath = IndexPath(row: 0, section: 0)
            if let firstLiveView = self.tableView.cellForRow(at: indexPath)?.contentView.subviews.first {
                self.delegate?.onViewDidSlideIn(view: firstLiveView)
            }
        }
    }
    
    private func onLastCellSlideOut() {
        if liveList.count > 0 {
            let indexPath = IndexPath(row: currentPage, section: 0)
            if let liveView = self.tableView.cellForRow(at: indexPath)?.contentView.subviews.first {
                self.delegate?.onViewDidSlideOut(view: liveView)
            }
        }
    }
    
    public func disableScrolling() {
        tableView.isScrollEnabled = false
    }
    
    public func enableScrolling() {
        tableView.isScrollEnabled = true
    }
    
    public func scrollToNextPage() {
        let nextPage = currentPage + 1
        if nextPage < liveList.count {
            let indexPath = IndexPath(row: nextPage, section: 0)
            tableView.scrollToRow(at: indexPath, at: .top, animated: true)
            let oldIndexPath = IndexPath(row: currentPage, section: 0)
            if let oldLiveView = tableView.cellForRow(at: oldIndexPath)?.contentView.subviews.first {
                delegate?.onViewWillSlideOut(view: oldLiveView)
            }
            if let newLiveView = tableView.cellForRow(at: indexPath)?.contentView.subviews.first {
                delegate?.onViewWillSlideIn(view: newLiveView)
            }
        }
    }
    
    deinit {
        debugPrint("deinit:\(self)")
    }
}

extension LiveListPagerView: UITableViewDelegate {
    public func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return UIScreen.main.bounds.height
    }
    
    public func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
        if let liveView = cell.contentView.subviews.first, (indexPath.row != 0 || currentPage != 0) {
            willDisplayPage = indexPath
            delegate?.onViewWillSlideIn(view: liveView)
        }
        if liveList.count - indexPath.row <= 3, let dataSource = dataSource, !isFetchingLiveList {
            isFetchingLiveList = true
            dataSource.fetchLiveList { [weak self] list in
                guard let self = self else { return }
                let startIndex = self.liveList.count
                self.liveList.append(contentsOf: list)
                let endIndex = self.liveList.count
                let indexPaths = (startIndex..<endIndex).map { IndexPath(row: $0, section: 0) }
                DispatchQueue.main.async {
                    self.tableView.insertRows(at: indexPaths, with: .automatic)
                }
                self.isFetchingLiveList = false
            }
        }
    }
}

extension LiveListPagerView: UITableViewDataSource {
    public func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return liveList.count
    }
    
    public func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: cellReuseIdentifier, for: indexPath)
        cell.contentView.subviews.forEach { $0.removeFromSuperview() }
        if let liveView = delegate?.onCreateView(liveInfo: liveList[indexPath.row]) {
            cell.contentView.addSubview(liveView)
            liveView.snp.makeConstraints{ make in
                make.edges.equalToSuperview()
            }
        }
        return cell
    }
}

extension LiveListPagerView: UIScrollViewDelegate {
    public func scrollViewWillBeginDragging(_ scrollView: UIScrollView) {
        let indexPath = IndexPath(row: currentPage, section: 0)
        if let liveView = tableView.cellForRow(at: indexPath)?.contentView.subviews.first {
            delegate?.onViewWillSlideOut(view: liveView)
        }
    }

    public func scrollViewDidEndDecelerating(_ scrollView: UIScrollView) {
        let pageHeight = tableView.frame.height
        let currentOffset = tableView.contentOffset.y
        let newPage = Int(currentOffset / pageHeight)
        
        if newPage != currentPage {
            scrollToNextRoom(newIndex: newPage)
        } else {
            stayOnThisRoom(index: currentPage)
        }
        
        currentPage = newPage
    }
    
    public func scrollViewDidEndScrollingAnimation(_ scrollView: UIScrollView) {
        let nextPage = currentPage + 1
        if nextPage < liveList.count {
            let oldIndexPath = IndexPath(row: currentPage, section: 0)
            let newIndexPath = IndexPath(row: nextPage, section: 0)
            if let oldLiveView = tableView.cellForRow(at: oldIndexPath)?.contentView.subviews.first {
                delegate?.onViewDidSlideOut(view: oldLiveView)
            }
            if let newLiveView = tableView.cellForRow(at: newIndexPath)?.contentView.subviews.first {
                delegate?.onViewDidSlideIn(view: newLiveView)
            }
            liveList.remove(at: currentPage)
            tableView.deleteRows(at: [oldIndexPath], with: .automatic)
            currentPage = nextPage
        }
    }
    
    private func scrollToNextRoom(newIndex: Int) {
        let newIndexPath = IndexPath(row: newIndex, section: 0)
        let oldIndexPath = IndexPath(row: currentPage, section: 0)
        if let oldLiveView = tableView.cellForRow(at: oldIndexPath)?.contentView.subviews.first {
            delegate?.onViewDidSlideOut(view: oldLiveView)
        }
        if let newLiveView = tableView.cellForRow(at: newIndexPath)?.contentView.subviews.first {
            delegate?.onViewDidSlideIn(view: newLiveView)
        }
    }
    
    private func stayOnThisRoom(index: Int) {
        let indexPath = IndexPath(row: currentPage, section: 0)
        if let slideOutCancelledView = tableView.cellForRow(at: indexPath)?.contentView.subviews.first {
            delegate?.onViewSlideOutCancelled(view: slideOutCancelledView)
        }
        if let slideInCancelledPage = willDisplayPage,
           let slideInCancelledView = tableView.cellForRow(at: slideInCancelledPage)?.contentView.subviews.first {
            delegate?.onViewSlideInCancelled(view: slideInCancelledView)
            willDisplayPage = nil
        }
    }
}
