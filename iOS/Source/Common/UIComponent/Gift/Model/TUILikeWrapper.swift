//
//  TUILikeWrapper.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/2.
//

import Foundation

struct TUILikeWrapper: Codable {
    let version: String
    let businessID: String
    let platform: String
    let data: TUILikeData

    init(version: String = "1.0", businessID: String = "TUIGift", platform: String = "iOS", data: TUILikeData) {
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


class TUILikeData: Codable {
    var sender: TUIGiftUser = TUIGiftUser()

    init() {}
    
    init(sender: TUIGiftUser) {
        self.sender = sender
    }

    enum CodingKeys: String, CodingKey {
        case sender
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(sender, forKey: .sender)
    }

    required init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        sender = try container.decode(TUIGiftUser.self, forKey: .sender)
    }
}

