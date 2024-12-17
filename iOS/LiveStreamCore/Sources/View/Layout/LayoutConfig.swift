//
//  LayoutConfig.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/31.
//

import UIKit

struct ViewInfo: Decodable, Equatable {
    let x: CGFloat
    let y: CGFloat
    let width: CGFloat
    let height: CGFloat
    let zOrder: Int
    let backgroundColor: String
    let userId: String
    
    enum CodingKeys: String, CodingKey {
        case x = "x"
        case y = "y"
        case width = "width"
        case height = "height"
        case zOrder = "zOrder"
        case backgroundColor = "backgroundColor"
        case userId = "userId"
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        x = try container.decode(CGFloat.self, forKey: .x)
        y = try container.decode(CGFloat.self, forKey: .y)
        width = try container.decode(CGFloat.self, forKey: .width)
        height = try container.decode(CGFloat.self, forKey: .height)
        zOrder = try container.decode(Int.self, forKey: .zOrder)
        backgroundColor = try container.decode(String.self, forKey: .backgroundColor)
        userId = try container.decodeIfPresent(String.self, forKey: .userId) ?? ""
    }
    
    init(x: CGFloat, y: CGFloat, width: CGFloat, height: CGFloat, zOrder: Int, backgroundColor: String, userId: String) {
        self.x = x
        self.y = y
        self.width = width
        self.height = height
        self.zOrder = zOrder
        self.backgroundColor = backgroundColor
        self.userId = userId
    }
}

struct LayoutInfo: Decodable {
    let backgroundColor: String
    let viewInfoList: [ViewInfo]
}

typealias LayoutConfig = [Int: LayoutInfo]

struct VideoLayoutInfo: Decodable, Equatable {
    var layoutList: [ViewInfo]
    let canvas: VideoCanvasInfo
    let layoutType: Int
    
    var pixelScale: CGFloat {
        canvas.height / canvas.width
    }
}

struct VideoCanvasInfo: Decodable, Equatable {
    let width: CGFloat
    let height: CGFloat
    let backgroundColor: String
}
