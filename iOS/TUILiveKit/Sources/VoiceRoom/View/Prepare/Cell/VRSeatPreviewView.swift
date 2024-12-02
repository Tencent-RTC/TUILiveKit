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

class VRSeatPreviewView: UIView {
    var itemSize = CGSize(width: 70, height: 105)
    var verticalMargin = 20.0
    
    var seatCount: Int = 10
    
    lazy var seatCollection: UICollectionView = {
        let layout = UICollectionViewFlowLayout()
        layout.itemSize = itemSize
        layout.minimumLineSpacing = verticalMargin
        layout.minimumInteritemSpacing = getHorizontalMargin()
        
        layout.sectionInset = .init(top: 0, left: getHorizontalMargin(), bottom: 0, right: getHorizontalMargin())
        layout.scrollDirection = .vertical
        
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: layout)
        collectionView.isScrollEnabled = false
        collectionView.register(VRSeatPreviewCell.self, forCellWithReuseIdentifier: VRSeatPreviewCell.identifier)
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
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }

}

extension VRSeatPreviewView: UICollectionViewDataSource {
    func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return seatCount
    }
    
    func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: VRSeatPreviewCell.identifier, for: indexPath)
        return cell
    }
}

extension VRSeatPreviewView: UICollectionViewDelegateFlowLayout {
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
