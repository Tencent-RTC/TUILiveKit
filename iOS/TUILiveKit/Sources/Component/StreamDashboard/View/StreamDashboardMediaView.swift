//
//  StreamDashboardMediaView.swift
//  TUILiveKit
//
//  Created by jack on 2024/11/21.
//

import Foundation
import Combine
import RTCCommon

class StreamDashboardMediaView: UIView {
    private let Screen_Width = UIScreen.main.bounds.size.width
    
    private var userDataSource: [StreamDashboardUser] = []
    
    private var cancellableSet: Set<AnyCancellable> = []
    private weak var manager: StreamDashboardManager?
    init(manager: StreamDashboardManager) {
        self.manager = manager
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private lazy var collectionView: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.minimumLineSpacing = 0
        layout.minimumInteritemSpacing = 0
        layout.scrollDirection = .horizontal
        let view = UICollectionView(frame: .zero, collectionViewLayout: layout)
        view.isPagingEnabled = true
        view.showsHorizontalScrollIndicator = false
        view.backgroundColor = .clear
        view.dataSource = self
        view.delegate = self
        view.register(StreamDashboardMediaCell.self, forCellWithReuseIdentifier: StreamDashboardMediaCell.CellID)
        return view
    }()
    
    private lazy var pageControl: UIPageControl = {
        let pageControl = UIPageControl()
        pageControl.numberOfPages = userDataSource.count
        pageControl.currentPage = 0
        pageControl.pageIndicatorTintColor = .textDisabledColor
        pageControl.currentPageIndicatorTintColor = .textPrimaryColor
        return pageControl
    }()
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        subscribeState()
    }
    
}

extension StreamDashboardMediaView {
    
    private func constructViewHierarchy() {
        addSubview(collectionView)
    }
    
    private func activateConstraints() {
        collectionView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.top.equalToSuperview()
            make.height.equalTo(174.scale375Height())
            make.bottom.equalToSuperview().inset(20.scale375Height())
        }
    }
    
    private func subscribeState() {
        guard let localPublisher = manager?.subscribe(StateSelector(keyPath: \StreamDashboardState.localUsers)) else {
            return
        }
        guard let remotePublisher = manager?.subscribe(StateSelector(keyPath: \StreamDashboardState.remoteUsers)) else {
            return
        }
        localPublisher
            .removeDuplicates()
            .combineLatest(remotePublisher.removeDuplicates())
            .receive(on: RunLoop.main)
            .sink { [weak self] localUsers, remoteUsers in
                guard let self = self else { return }
                self.userDataSource = localUsers + remoteUsers
                self.updatePageControl()
                self.reloadUIData()
            }
            .store(in: &cancellableSet)
        
    }
    
    private func reloadUIData() {
        UIView.performWithoutAnimation { [weak self] in
            guard let self = self else { return }
            self.collectionView.reloadData()
            
            let isRemoteUsersEmpty = manager?.state.remoteUsers.isEmpty ?? true
            collectionView.snp.remakeConstraints { make in
                make.leading.trailing.equalToSuperview()
                make.top.equalToSuperview()
                make.height.equalTo(isRemoteUsersEmpty ? 128.scale375Height() : 174.scale375Height())
                make.bottom.equalToSuperview().inset(isRemoteUsersEmpty ? 0 : 20.scale375Height())
            }
        }
    }
    
    private func updatePageControl() {
        if userDataSource.count > 1 {
            pageControl.numberOfPages = userDataSource.count
            addPageControl()
        } else {
            removePageControl()
        }
    }
    
    private func addPageControl() {
        guard pageControl.superview == nil else { return }
        addSubview(pageControl)
        pageControl.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.top.equalTo(collectionView.snp.bottom).offset(12.scale375Height())
            make.height.equalTo(6.scale375())
        }
    }
    
    private func removePageControl() {
        pageControl.safeRemoveFromSuperview()
    }
}

extension StreamDashboardMediaView: UICollectionViewDataSource {
    
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return userDataSource.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: StreamDashboardMediaCell.CellID, for: indexPath) as! StreamDashboardMediaCell
        cell.updateData(userDataSource[indexPath.item])
        cell.changeRemoteUserEmpty(isEmpty: manager?.state.remoteUsers.isEmpty ?? true)
        return cell
    }
}

extension StreamDashboardMediaView: UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
        let isRemoteUsersEmpty = manager?.state.remoteUsers.isEmpty ?? true
        return CGSize(width: Screen_Width, height: isRemoteUsersEmpty ? 128.scale375Height() : 174.scale375Height())
    }
}

extension StreamDashboardMediaView: UIScrollViewDelegate {
    func scrollViewDidScroll(_ scrollView: UIScrollView) {
        let pageWidth = scrollView.frame.width
        let currentPage = Int((scrollView.contentOffset.x + pageWidth / 2) / pageWidth)
        pageControl.currentPage = currentPage
    }
}
