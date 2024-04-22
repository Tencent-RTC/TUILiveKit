//
//  TUIGiftWrapper.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/2.
//

import Foundation

struct TUIGiftWrapper: Codable {
    let version: String
    let businessID: String
    let platform: String
    let data: TUIGiftData

    init(version: String = "1.0", businessID: String = "TUIGift", platform: String = "iOS", data: TUIGiftData) {
        self.version = version
        self.businessID = businessID
        self.platform = platform
        self.data = data
    }

    enum CodingKeys: String, CodingKey {
        case version
        case businessID
        case platform
        case data
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(version, forKey: .version)
        try container.encode(businessID, forKey: .businessID)
        try container.encode(platform, forKey: .platform)
        try container.encode(data, forKey: .data)
    }
}


class TUIGiftData: Codable {
    var gift: TUIGift = TUIGift()
    var giftCount: Int = 0
    var receiver: TUIGiftUser = TUIGiftUser()
    var sender: TUIGiftUser = TUIGiftUser()

    init() {}
    
    init(gift: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser) {
        self.gift = gift
        self.giftCount = giftCount
        self.receiver = receiver
        self.sender = sender
        self.sender.level = "0"
    }

    enum CodingKeys: String, CodingKey {
        case gift
        case giftCount
        case receiver
        case sender
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(gift, forKey: .gift)
        try container.encode(giftCount, forKey: .giftCount)
        try container.encode(receiver, forKey: .receiver)
        try container.encode(sender, forKey: .sender)
    }

    required init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        gift = try container.decode(TUIGift.self, forKey: .gift)
        giftCount = try container.decode(Int.self, forKey: .giftCount)
        receiver = try container.decode(TUIGiftUser.self, forKey: .receiver)
        sender = try container.decode(TUIGiftUser.self, forKey: .sender)
    }
}

