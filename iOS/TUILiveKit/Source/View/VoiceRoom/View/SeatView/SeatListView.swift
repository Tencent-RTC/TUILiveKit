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
    var itemSize = CGSize(width: 70, height: 105)
    var verticalMargin = 20.0
    
    weak var delegate: SeatListViewDelegate?
    @Published var seatCount: Int = 10
    private var cancellableSet = Set<AnyCancellable>()
    
    lazy var seatCollection: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.itemSize = itemSize
        layout.minimumLineSpacing = verticalMargin
        layout.minimumInteritemSpacing = getHorizontalMargin()
        
        layout.sectionInset = .init(top: 0, left: getHorizontalMargin(), bottom: 0, right: getHorizontalMargin())
        layout.scrollDirection = .vertical
        
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: layout)
        collectionView.isScrollEnabled = false
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
    
    func getHeight() -> CGFloat {
        let height = itemSize.height
        if seatCount <= 5 {
            return height * 1 + verticalMargin * 0
        } else if seatCount <= 10 {
            return height * 2 + verticalMargin * 1
        } else if seatCount <= 15 {
            return height * 3 + verticalMargin * 2
        } else {
            return height * 4 + verticalMargin * 3
        }
    }
    
    func getHorizontalMargin() -> CGFloat {
        switch seatCount {
        case 3, 6, 9:
            return (UIScreen.main.bounds.width - itemSize.width * 3.0) / 4.0
        case 4, 8, 12, 16:
            return (UIScreen.main.bounds.width - itemSize.width * 4.0) / 5.0
        case 5, 10, 15:
            return (UIScreen.main.bounds.width - itemSize.width * 5.0) / 6.0
        default:
            return (UIScreen.main.bounds.width - itemSize.width * 5.0) / 6.0
        }
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

extension SeatListView: UICollectionViewDelegateFlowLayout {
    func collectionView(_ collectionView: UICollectionView, 
                        layout collectionViewLayout: UICollectionViewLayout,
                        minimumInteritemSpacingForSectionAt section: Int) -> CGFloat {
        return getHorizontalMargin()
    }
    
    func collectionView(_ collectionView: UICollectionView, 
                        layout collectionViewLayout: UICollectionViewLayout,
                        insetForSectionAt section: Int) -> UIEdgeInsets {
        return UIEdgeInsets(top: 0, left: getHorizontalMargin(), bottom: 0, right: getHorizontalMargin())
    }
}
