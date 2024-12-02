//
//  TUIGiftSideslipLayout.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import UIKit

class TUIGiftSideslipLayout: UICollectionViewFlowLayout {
    var rows: Int = 2
    private var layoutAttributes: [UICollectionViewLayoutAttributes] = []
    private var beginDiff: CGFloat = 0
    private var midDiff: CGFloat = 0
    private var cellRowCount: Int = 4
    private var maxLeft: CGFloat = 0

    override func prepare() {
        guard let collectionView = collectionView else { return }
        maxLeft = 0.0
        cellRowCount = 4
        beginDiff = 24
        midDiff = (collectionView.mm_w - itemSize.width * CGFloat(cellRowCount) - (beginDiff * 2)) / (CGFloat(cellRowCount) - 1.0)

        layoutAttributes = []

        let itemCount = collectionView.numberOfItems(inSection: 0)
        
        for i in 0 ..< itemCount {
            let indexPath = IndexPath(item: i, section: 0)
            let attribute = layoutAttributesForItem(at: indexPath)
            layoutAttributes.append(attribute!)
        }

        let pageCellCount = cellRowCount * rows
        let page = itemCount / pageCellCount
        maxLeft = CGFloat(page + 1) * (collectionView.mm_w)
    }

    override func layoutAttributesForItem(at indexPath: IndexPath) -> UICollectionViewLayoutAttributes? {
        let attribute = UICollectionViewLayoutAttributes(forCellWith: indexPath)
        let pageCellCount = cellRowCount * rows
        let page = indexPath.item / pageCellCount
        let index = indexPath.item % pageCellCount
        let indexRow = index % rows
        var x: CGFloat = CGFloat(index / rows) * (itemSize.width + midDiff) + beginDiff
        x += CGFloat(page) * (collectionView?.mm_w ?? 0)
        var y: CGFloat = 0
        if indexRow != 0 {
            y = itemSize.height
            attribute.frame = CGRect(x: x, y: y, width: itemSize.width, height: itemSize.height)
        } else {
            attribute.frame = CGRect(x: x, y: y, width: itemSize.width, height: itemSize.height)
        }
        return attribute
    }

    override func layoutAttributesForElements(in rect: CGRect) -> [UICollectionViewLayoutAttributes]? {
        return layoutAttributes
    }

    override var collectionViewContentSize: CGSize {
        return CGSize(width: maxLeft, height: collectionView?.mm_h ?? 0)
    }
}
