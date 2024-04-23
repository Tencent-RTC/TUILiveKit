//
//  TUIGift.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/2.
//

import Foundation

class TUIGift: Codable {
    let giftId: String
    let giftName: String
    let imageUrl: String
    let animationUrl: String
    let price: Int
    var extInfo: [String: AnyCodable]
    
    init() {
        self.giftId = ""
        self.giftName = ""
        self.imageUrl = ""
        self.animationUrl = ""
        self.price = 0
        self.extInfo = [:]
    }
    
    init(giftId: String, giftName: String, imageUrl: String, animationUrl: String, price: Int, extInfo: [String: AnyCodable]) {
        self.giftId = giftId
        self.giftName = giftName
        self.imageUrl = imageUrl
        self.animationUrl = animationUrl
        self.price = price
        self.extInfo = extInfo
    }
    
    func toString() -> String {
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

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(giftId, forKey: .giftId)
        try container.encode(giftName, forKey: .giftName)
        try container.encode(imageUrl, forKey: .imageUrl)
        try container.encode(animationUrl, forKey: .animationUrl)
        try container.encode(price, forKey: .price)
        try container.encode(extInfo, forKey: .extInfo)
    }
    
    required init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        giftId = try container.decode(String.self, forKey: .giftId)
        giftName = try container.decode(String.self, forKey: .giftName)
        imageUrl = try container.decode(String.self, forKey: .imageUrl)
        animationUrl = try container.decode(String.self, forKey: .animationUrl)
        price = try container.decode(Int.self, forKey: .price)
        extInfo = try container.decode([String: AnyCodable].self, forKey: .extInfo)
    }
}
