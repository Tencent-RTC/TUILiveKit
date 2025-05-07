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
    
    private lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textColor = .g7
        label.text = .mediaText
        label.font = .customFont(ofSize: 14, weight: .semibold)
        return label
    }()
    
    private var userDataSource: [StreamDashboardUser] = []
    
    private var cancellableSet: Set<AnyCancellable> = []
    
    private let Screen_Width = UIScreen.main.bounds.size.width
    private let Screen_Height = UIScreen.main.bounds.size.height
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
        layout.minimumLineSpacing = 20
        layout.minimumInteritemSpacing = 20
        layout.estimatedItemSize = CGSize(width: Screen_Width, height: 309)
        layout.scrollDirection = .vertical
        let view = UICollectionView(frame: .zero, collectionViewLayout: layout)
        view.showsVerticalScrollIndicator = false
        view.backgroundColor = .clear
        view.dataSource = self
        view.register(StreamDashboardMediaCell.self, forCellWithReuseIdentifier: StreamDashboardMediaCell.CellID)
        return view
    }()
    
    private var isViewReady: Bool = false
    override func layoutSubviews() {
        super.layoutSubviews()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        subscribeState()
    }
    
}

extension StreamDashboardMediaView {
    
    private func constructViewHierarchy() {
        addSubview(titleLabel)
        addSubview(collectionView)
    }
    
    private func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview()
            make.leading.equalToSuperview().offset(20)
        }
        collectionView.snp.makeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.top.equalTo(titleLabel.snp.bottom).offset(10)
            make.height.equalTo(350)
            make.bottom.equalToSuperview()
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
            .combineLatest(remotePublisher)
            .receive(on: RunLoop.main)
            .sink { [weak self] localUsers, remoteUsers in
                guard let self = self else { return }
                self.userDataSource = localUsers + remoteUsers
                self.reloadUIData()
            }
            .store(in: &cancellableSet)
        
    }
    
    private func reloadUIData() {
        UIView.performWithoutAnimation { [weak self] in
            guard let self = self else { return }
            self.collectionView.reloadData()
        }
    }
}

extension StreamDashboardMediaView: UICollectionViewDataSource {
    
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return userDataSource.count
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: StreamDashboardMediaCell.CellID, for: indexPath) as! StreamDashboardMediaCell
        cell.updateData(userDataSource[indexPath.item])
        return cell
    }
}

fileprivate extension String {
    static let mediaText = localized("Media Information")
}
