//
//  TUIGift.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/2.
//

import Foundation
import RTCCommon

public class TUIGift: Codable {
    public let giftId: String
    public let giftName: String
    public let imageUrl: String
    public let animationUrl: String
    public let price: Int
    public var extInfo: [String: AnyCodable]
    
    public init() {
        self.giftId = ""
        self.giftName = ""
        self.imageUrl = ""
        self.animationUrl = ""
        self.price = 0
        self.extInfo = [:]
    }
    
    public init(giftId: String, giftName: String, imageUrl: String, animationUrl: String, price: Int, extInfo: [String: AnyCodable]) {
        self.giftId = giftId
        self.giftName = giftName
        self.imageUrl = imageUrl
        self.animationUrl = animationUrl
        self.price = price
        self.extInfo = extInfo
    }
    
    public func toString() -> String {
        return "TUIGift{giftId=\(giftId),giftName=\(giftName),imageUrl=\(imageUrl),animationUrl=\(animationUrl),price=\(price),extInfo:\(extInfo)}"
    }
    
    enum CodingKeys: String, CodingKey {
        case giftId
        case giftName
        case imageUrl
        case animationUrl
        case price
        case extInfo
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(giftId, forKey: .giftId)
        try container.encode(giftName, forKey: .giftName)
        try container.encode(imageUrl, forKey: .imageUrl)
        try container.encode(animationUrl, forKey: .animationUrl)
        try container.encode(price, forKey: .price)
        try container.encode(extInfo, forKey: .extInfo)
    }
    
    public required init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        giftId = try container.decode(String.self, forKey: .giftId)
        giftName = try container.decode(String.self, forKey: .giftName)
        imageUrl = try container.decode(String.self, forKey: .imageUrl)
        animationUrl = try container.decode(String.self, forKey: .animationUrl)
        price = try container.decode(Int.self, forKey: .price)
        extInfo = try container.decode([String: AnyCodable].self, forKey: .extInfo)
    }
}
