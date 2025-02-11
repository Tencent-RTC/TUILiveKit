//
//  LayoutConfig.swift
//  LiveStreamCore
//
//  Created by jeremiawang on 2024/10/31.
//

import UIKit

public struct ViewInfo: Decodable, Equatable {
    public let x: CGFloat
    public let y: CGFloat
    public let width: CGFloat
    public let height: CGFloat
    public let zOrder: Int
    public let backgroundColor: String
    public let userId: String
    
    enum CodingKeys: String, CodingKey {
        case x = "x"
        case y = "y"
        case width = "width"
        case height = "height"
        case zOrder = "zOrder"
        case backgroundColor = "backgroundColor"
        case userId = "userId"
    }

    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        x = try container.decode(CGFloat.self, forKey: .x)
        y = try container.decode(CGFloat.self, forKey: .y)
        width = try container.decode(CGFloat.self, forKey: .width)
        height = try container.decode(CGFloat.self, forKey: .height)
        zOrder = try container.decode(Int.self, forKey: .zOrder)
        backgroundColor = try container.decode(String.self, forKey: .backgroundColor)
        userId = try container.decodeIfPresent(String.self, forKey: .userId) ?? ""
    }
    
    public init(x: CGFloat, y: CGFloat, width: CGFloat, height: CGFloat, zOrder: Int, backgroundColor: String, userId: String) {
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

public struct VideoLayoutInfo: Decodable, Equatable {
    var layoutList: [ViewInfo]
    let canvas: VideoCanvasInfo
    let layoutType: Int
    
    var pixelScale: CGFloat {
        canvas.height / canvas.width
    }
}

public struct VideoCanvasInfo: Decodable, Equatable {
    let width: CGFloat
    let height: CGFloat
    let backgroundColor: String
}
