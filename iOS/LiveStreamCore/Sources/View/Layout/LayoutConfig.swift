//
//  LayoutConfig.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/31.
//

import UIKit

struct ViewInfo: Decodable {
    let x: CGFloat
    let y: CGFloat
    let width: CGFloat
    let height: CGFloat
    let zOrder: Int
    let backgroundColor: String
}

struct LayoutInfo: Decodable {
    let backgroundColor: String
    let viewInfoList: [ViewInfo]
}

typealias LayoutConfig = [Int: LayoutInfo]
