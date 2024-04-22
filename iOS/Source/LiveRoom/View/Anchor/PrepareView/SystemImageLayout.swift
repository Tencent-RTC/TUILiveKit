//
//  RenderLayout.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/9.
//

import Foundation

class SystemImageLayout: UICollectionViewFlowLayout {
    private let cellCount:Int
    private let midDiff = 14.scale375()
    private var contentHeight = 0.0

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

    init(cellCount: Int) {
        self.cellCount = cellCount
        super.init()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func prepare() {
        super.prepare()
        calculateEachCellFrame()
    }

    override func layoutAttributesForElements(in rect: CGRect) -> [UICollectionViewLayoutAttributes]? {
        return layoutAttributeArray
    }
    
    override var collectionViewContentSize: CGSize {
        return CGSize(width: collectionViewWidth, height: contentHeight + 80)
    }
}

// MARK: - layout

extension SystemImageLayout {
    private func calculateEachCellFrame() {
        let columnCount = getDivideColumn()
        let frame = getDivideCellContentFrame()
        let topDiff = frame.origin.y
        let leftDiff = frame.origin.x
        let cellWidth = (frame.size.width - CGFloat(columnCount-1)*midDiff)/CGFloat(columnCount)
        let cellHeight = cellWidth
        var leftX = 0.0
        var leftY = 0.0
        for i in 0 ... cellCount {
            let indexPath = IndexPath(item: i, section: 0)
            let cell = UICollectionViewLayoutAttributes(forCellWith: indexPath)
            leftX = Double(i % columnCount) * (cellWidth + midDiff)
            leftY = floor(CGFloat(i) / CGFloat(columnCount)) * (cellHeight + midDiff)
            cell.frame = CGRect(x: leftDiff + leftX, y: topDiff + leftY, width: cellWidth, height: cellHeight)
            layoutAttributeArray.append(cell)
            contentHeight = max(contentHeight,cell.frame.maxY)
        }
    }

    private func getDivideColumn() -> Int {
        return 3
    }
    
    private func getDivideCellContentFrame() -> CGRect {
        CGRect(x: 24, y: 0, width: collectionViewWidth - 48, height: collectionViewHeight)
    }
}
