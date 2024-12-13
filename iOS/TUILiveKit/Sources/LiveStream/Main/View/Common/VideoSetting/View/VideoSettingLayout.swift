//
//  VideoSettingLayout.swift
//  TUILiveKit
//
//  Created by jack on 2024/11/25.
//

import Foundation

class VideoSettingLayout: UICollectionViewFlowLayout {
    
    private var sectionAttrs: [UICollectionViewLayoutAttributes] = []
    
    override init() {
        super.init()
        register(ViewSettingDecorationView.self, forDecorationViewOfKind: ViewSettingDecorationView.ReusableID)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func prepare() {
        super.prepare()
        guard let numberOfSections = collectionView?.numberOfSections,
              let collection = collectionView else {
            return
        }
        sectionAttrs.removeAll()
        
        for section in 0..<numberOfSections {
            let numberOfItems = collection.numberOfItems(inSection: section)
            guard numberOfItems > 0,
                  let firstItem = layoutAttributesForItem(at: IndexPath(item: 0, section: section)),
                  let lastItem = layoutAttributesForItem(at: IndexPath(item: numberOfItems - 1, section: section)) else {
                continue
            }
            
            let sectionFrame = firstItem.frame.union(lastItem.frame)
            
            let decorations = UICollectionViewLayoutAttributes(forDecorationViewOfKind:ViewSettingDecorationView.ReusableID,
                                                               with: IndexPath(item: 0, section: section))
            decorations.frame = sectionFrame
            decorations.zIndex = -1
            
            sectionAttrs.append(decorations)
        }
        
    }
    
    override func layoutAttributesForElements(in rect: CGRect) -> [UICollectionViewLayoutAttributes]? {
        var array = super.layoutAttributesForElements(in: rect)
        array?.append(contentsOf: sectionAttrs.filter({
            return rect.intersects($0.frame)
        }))
        return array
    }
}
