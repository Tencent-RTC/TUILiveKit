//
//  MatrixVideoRenderLayout.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/2.
//

import Foundation

class MatrixVideoRenderLayout: UICollectionViewFlowLayout {
    
    var renderCount: Int = 1
    
    private var collectionViewHeight: CGFloat {
        return collectionView?.bounds.height ?? UIScreen.main.bounds.height
    }

    private var collectionViewWidth: CGFloat {
        return collectionView?.bounds.width ?? UIScreen.main.bounds.width
    }

    private var isPortrait: Bool {
        return collectionViewHeight > collectionViewWidth
    }

    // save all items
    fileprivate var layoutAttributeArray: [UICollectionViewLayoutAttributes] = []

    override func prepare() {
        super.prepare()
        calculateEachCellFrame()
    }

    override func layoutAttributesForElements(in rect: CGRect) -> [UICollectionViewLayoutAttributes]? {
        return layoutAttributeArray
    }
}

// MARK: - layout

extension MatrixVideoRenderLayout {
    // calculate position and size of cell and save them
    private func calculateEachCellFrame() {
        if renderCount == 1 {
            fullScreenAttributes()
        } else if renderCount == 3 {
            thirdAttributes()
        } else if renderCount > 0 {
            divideAttributes()
        }
    }

    private func fullScreenAttributes() {
        let topDiff = 0.scale375Height()
        var leftDiff = 0.scale375Height()
        if !isPortrait {
            leftDiff = 73.scale375()
        }
        let cell = UICollectionViewLayoutAttributes(forCellWith: IndexPath(item: 0, section: 0))
        cell.frame = CGRect(x: leftDiff,
                            y: topDiff,
                            width: collectionViewWidth - leftDiff * 2,
                            height: collectionViewHeight - topDiff * 2)
        layoutAttributeArray.append(cell)
    }

    private func thirdAttributes() {
        var topDiff = 144.scale375Height()
        var leftDiff = 0.0
        var cellContentWidth = collectionViewWidth
        var cellContentHeight = (collectionViewWidth / 3.0) * 2.0
        if !isPortrait {
            cellContentHeight = collectionViewHeight
            cellContentWidth = cellContentHeight * 1.5
            topDiff = 0
            leftDiff = (collectionViewWidth - cellContentWidth) * 0.5
        }

        let rightCellWidth = cellContentHeight * 0.5
        let leftCellWidth = cellContentWidth - rightCellWidth

        var indexPath = IndexPath(item: 0, section: 0)
        var cell = UICollectionViewLayoutAttributes(forCellWith: indexPath)
        cell.frame = CGRect(x: leftDiff + 0, y: topDiff + 0, width: leftCellWidth, height: leftCellWidth)
        layoutAttributeArray.append(cell)
        let rightX = leftCellWidth
        indexPath = IndexPath(item: 1, section: 0)
        cell = UICollectionViewLayoutAttributes(forCellWith: indexPath)
        cell.frame = CGRect(x: leftDiff + rightX, y: topDiff + 0, width: rightCellWidth, height: rightCellWidth)
        layoutAttributeArray.append(cell)

        let rightY = rightCellWidth
        indexPath = IndexPath(item: 2, section: 0)
        cell = UICollectionViewLayoutAttributes(forCellWith: indexPath)
        cell.frame = CGRect(x: leftDiff + rightX, y: topDiff + rightY, width: rightCellWidth, height: rightCellWidth)
        layoutAttributeArray.append(cell)
    }

    private func divideAttributes() {
        let cellCount = renderCount
        let columnCount = getDivideColumn()
        let rowCount = ceil(CGFloat(cellCount) / CGFloat(columnCount))
        let frame = getDivideCellContentFrame()
        let topDiff = frame.origin.y
        let leftDiff = frame.origin.x
        let cellWidth = frame.size.width / CGFloat(columnCount)
        let cellHeight = frame.size.height / CGFloat(rowCount)
        var leftX = 0.0
        var leftY = 0.0
        for i in 0 ... (cellCount-1) {
            let indexPath = IndexPath(item: i, section: 0)
            let cell = UICollectionViewLayoutAttributes(forCellWith: indexPath)
            leftX = Double(i % columnCount) * cellWidth
            leftY = floor(CGFloat(i) / CGFloat(columnCount)) * cellHeight
            cell.frame = CGRect(x: leftDiff + leftX, y: topDiff + leftY, width: cellWidth, height: cellHeight)
            layoutAttributeArray.append(cell)
        }
    }

    private func getDivideColumn() -> Int {
        if renderCount > 4 {
            return 3
        }
        return 2
    }

    private func getDivideCellContentFrame() -> CGRect {
        let cellCount = renderCount
        let columnCount = getDivideColumn()
        let rowCount = ceil(CGFloat(cellCount) / CGFloat(columnCount))
        var top = 144.scale375Height()
        var left = 0.0
        var cellContentWidth = collectionViewWidth
        var cellContentHeight = (cellContentWidth / CGFloat(columnCount)) * CGFloat(rowCount)
        if cellCount == 2 {
            cellContentHeight = 355.scale375Height()
        }
        if !isPortrait {
            if cellCount == 2 {
                top = 79.scale375()
                cellContentWidth = collectionViewWidth
                cellContentHeight = collectionViewHeight - top * 2
            } else {
                cellContentHeight = collectionViewHeight
                cellContentWidth = cellContentHeight / CGFloat(rowCount) * CGFloat(columnCount)
                top = 0
            }
            left = (collectionViewWidth - cellContentWidth) * 0.5
        }
        return CGRect(x: left, y: top, width: cellContentWidth, height: cellContentHeight)
    }
}
