//
//  AnchorVideoView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/12/15.
//

import UIKit

class AnchorVideoView: MatrixVideoRenderView {
    override init(store: any LiveStore) {
        super.init(store: store)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
}
