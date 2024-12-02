//
//  SGSeatGridLayout.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/15.
//

import UIKit

class SeatGridViewLayout: UICollectionViewLayout {
    let rowSpacing: CGFloat
    let rowConfigs: [SGSeatViewLayoutRowConfig]
    private var layoutAttributes: [UICollectionViewLayoutAttributes] = []
    private var contentHeight: CGFloat = 0
    private var contentWidth: CGFloat {
        collectionView?.bounds.width ?? 0
    }
    
    init(rowSpacing: CGFloat = 22.0,
         rowConfigs: [SGSeatViewLayoutRowConfig] = [SGSeatViewLayoutRowConfig](repeating: SGSeatViewLayoutRowConfig(), count: 2)) {
        self.rowSpacing = rowSpacing
        self.rowConfigs = rowConfigs
        super.init()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func prepare() {
        super.prepare()
        calculateLayout()
    }
    
    private func calculateLayout() {
        layoutAttributes.removeAll()
        contentHeight = 0
        
        var yOffset: CGFloat = 0
        for rowConfig in rowConfigs {
            let itemWidth = rowConfig.seatSize.width
            let itemHeight = rowConfig.seatSize.height
            var itemSpacing = rowConfig.seatSpacing
            var xOffset: CGFloat = 0
            
            switch rowConfig.alignment {
                case .start:
                    xOffset = 0
                case .end:
                    xOffset = (contentWidth - CGFloat(rowConfig.count) * itemWidth - CGFloat(rowConfig.count - 1) * rowConfig.seatSpacing)
                case .center:
                    xOffset = (contentWidth - CGFloat(rowConfig.count) * itemWidth - CGFloat(rowConfig.count - 1) * rowConfig.seatSpacing) / 2
                case .spaceAround:
                    itemSpacing = (contentWidth - CGFloat(rowConfig.count) * itemWidth) / CGFloat(rowConfig.count)
                    xOffset = itemSpacing / 2
                case .spaceBetween:
                    xOffset = 0
                    itemSpacing = (contentWidth - CGFloat(rowConfig.count) * itemWidth) / CGFloat(rowConfig.count - 1)
                case .spaceEvenly:
                    xOffset = (contentWidth - CGFloat(rowConfig.count) * itemWidth) / CGFloat(rowConfig.count + 1)
                    itemSpacing = xOffset
            }
            
            for _ in 0 ..< rowConfig.count {
                let indexPath = IndexPath(row: layoutAttributes.count, section: 0)
                
                let frame = CGRect(x: xOffset, y: yOffset, width: itemWidth, height: itemHeight)
                
                let layoutAttribute = UICollectionViewLayoutAttributes(forCellWith: indexPath)
                layoutAttribute.frame = frame
                
                self.layoutAttributes.append(layoutAttribute)
                
                xOffset += (itemWidth + itemSpacing)
            }
            yOffset += (rowConfig.seatSize.height + rowSpacing)
        }
        contentHeight = yOffset
    }
}

extension SeatGridViewLayout {
    override var collectionViewContentSize: CGSize {
        return CGSize(width: contentWidth, height: contentHeight)
    }
    
    override func layoutAttributesForItem(at indexPath: IndexPath) -> UICollectionViewLayoutAttributes? {
        if indexPath.row >= layoutAttributes.count {
            return nil
        } else {
            return layoutAttributes[indexPath.row]
        }
    }
    
    override func layoutAttributesForElements(in rect: CGRect) -> [UICollectionViewLayoutAttributes]? {
        return layoutAttributes.filter { $0.frame.intersects(rect) }
    }
}
