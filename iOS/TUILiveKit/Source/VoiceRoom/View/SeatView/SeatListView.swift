//
//  SeatView.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/14.
//

import UIKit
import SnapKit
import Kingfisher
import Combine

protocol SeatListViewDelegate: AnyObject {
    func seatListView(_ seatListView: SeatListView, didSelectSeatAt index: Int)
    
    func seatListView(_ seatListView: SeatListView, needUpdateViewState seatView: SeatView, at indexPath: IndexPath)
}

class SeatListView: UIView {
    
    @Published var seatCount: Int = 0
    weak var delegate: SeatListViewDelegate?
    
    private var cancellableSet = Set<AnyCancellable>()
    let seatCollection: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.itemSize = CGSize(width: 64, height: 90)
        layout.minimumLineSpacing = 20.0
        layout.minimumInteritemSpacing = 26
        layout.sectionInset = .init(top: 0, left: 20, bottom: 0, right: 20)
        layout.scrollDirection = .vertical
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: layout)
        collectionView.register(SeatCollectionCell.self, forCellWithReuseIdentifier: SeatCollectionCell.identifier)
        collectionView.backgroundColor = UIColor.clear
        return collectionView
    }()
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activeViewConstraint()
        bindInteraction()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        addSubview(seatCollection)
    }
    
    private func activeViewConstraint() {
        seatCollection.snp.makeConstraints { make in
            make.left.top.right.bottom.equalToSuperview()
        }
    }
    
    private func bindInteraction() {
        seatCollection.dataSource = self
        seatCollection.delegate = self
        $seatCount
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .sink { [weak self] _ in
                guard let self = self else { return }
                self.seatCollection.reloadData()
            }
            .store(in: &cancellableSet)
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }

}

extension SeatListView: UICollectionViewDelegate {
    func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        self.delegate?.seatListView(self, didSelectSeatAt: indexPath.item)
    }
}

extension SeatListView: UICollectionViewDataSource {
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return seatCount
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: SeatCollectionCell.identifier, for: indexPath)
        if let seatCell = cell as? SeatCollectionCell {
            self.delegate?.seatListView(self, needUpdateViewState: seatCell.seatView, at: indexPath)
        }
        return cell
    }
}
