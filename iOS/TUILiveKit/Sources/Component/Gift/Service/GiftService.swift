//
//  GiftService.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import Foundation
import TUICore
import RTCRoomEngine

protocol GiftServiceDelegate: AnyObject {
    func onReceiveGiftMessage(giftInfo: TUIGiftInfo, count: Int, sender: TUIUserInfo)
    func onReceiveLikeMessage(totalLikesReceived: Int, sender: TUIUserInfo)
}

class GiftServiceFactory: NSObject {
    static var giftServiceMap: [String : GiftService] = [:]
    
    static func getGiftService(roomId: String) -> GiftService {
        if let giftService = giftServiceMap[roomId] {
            return giftService
        }
        let giftService = GiftService(roomId: roomId)
        giftServiceMap[roomId] = giftService
        return giftService
    }
    
    static func destroyGiftService(roomId: String) {
        giftServiceMap.removeValue(forKey: roomId)
    }
}

class GiftService: NSObject {
    weak var delegate: GiftServiceDelegate?
    
    private let giftManager: TUILiveGiftManager
    private let roomId: String
    
    init(roomId: String, delegate: GiftServiceDelegate? = nil) {
        self.roomId = roomId
        self.delegate = delegate
        self.giftManager = TUIRoomEngine.sharedInstance().getExtension(extensionType: .liveGiftManager) as! TUILiveGiftManager
        super.init()
        addObserver()
        setCurrentLanguage(language: TUIGlobalization.getPreferredLanguage() ?? "en")
    }
    
    private func addObserver() {
        self.giftManager.addObserver(self)
    }
    
    private func removeObserver() {
        self.giftManager.removeObserver(self)
    }
    
    deinit {
        debugPrint("deinit: \(self)")
        removeObserver()
    }
}

// MARK: - Gift
extension GiftService {
    func sendGift(giftInfo: TUIGiftInfo, giftCount: Int) async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            giftManager.sendGift(roomId: roomId, giftId: giftInfo.giftId, count: UInt(giftCount)) {
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: InternalError(error: LiveError(rawValue: code.rawValue) ?? UnknownError(rawValue: code.rawValue), message: message))
            }
        }
    }
    
    func getGiftList() async throws {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            giftManager.getGiftList(roomId: roomId) { [weak self] giftCategoryList in
                guard let self = self else { return }
                var giftList: [TUIGiftInfo] = []
                for giftCategory in giftCategoryList {
                    giftList.append(contentsOf: giftCategory.giftList)
                }
                TUIGiftStore.shared.giftListMap.value.updateValue(giftList, forKey: roomId)
                continuation.resume()
            } onError: { code, message in
                continuation.resume(throwing: InternalError(error: LiveError(rawValue: code.rawValue) ?? UnknownError(rawValue: code.rawValue), message: message))
            }
        }
    }
    
    func getGiftCountByAnchor() async throws -> (UInt, UInt, UInt) {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            giftManager.getGiftCountByAnchor(roomId: roomId) { totalGiftsSent, totalGiftCoins, totalUniqueGiftSenders in
                continuation.resume(returning: (totalGiftsSent, totalGiftCoins, totalUniqueGiftSenders))
            } onError: { code, message in
                continuation.resume(throwing: InternalError(error: LiveError(rawValue: code.rawValue) ?? UnknownError(rawValue: code.rawValue), message: message))
            }
        }
    }
    
    private func setCurrentLanguage(language: String) {
        var i18nLanguage = language
        if i18nLanguage != "en" && i18nLanguage != "zh-Hans" && i18nLanguage != "zh-Hant" {
            i18nLanguage = "en"
        }
        let jsonDic: [String : Any] = ["api" : "setCurrentLanguage",
                                       "params" : [
                                        "language" : i18nLanguage
                                       ]]
        guard let jsonData = try? JSONSerialization.data(withJSONObject: jsonDic) else { return }
        
        guard let jsonStr = String(data: jsonData, encoding: .utf8) else { return }
        
        TUIRoomEngine.sharedInstance().callExperimentalAPI(jsonStr: jsonStr) { message in
        }
    }
}

// MARK: - TUILiveGiftObserver
extension GiftService: TUILiveGiftObserver {
    func onGiftCountChanged(roomId: String, totalGiftsSent: UInt, totalGiftCoins: UInt, totalUniqueGiftSenders: UInt) {
        guard roomId == self.roomId else {
            return
        }
    }
    
    func onReceiveGiftMessage(roomId: String, giftInfo: TUIGiftInfo, giftCount count: UInt, sender: TUIUserInfo) {
        guard roomId == self.roomId else {
            return
        }
        delegate?.onReceiveGiftMessage(giftInfo: giftInfo, count: Int(count), sender: sender)
    }
    
    func onReceiveLikesMessage(roomId: String, totalLikesReceived: UInt, sender: TUIUserInfo) {
        guard roomId == self.roomId else {
            return
        }
        
        delegate?.onReceiveLikeMessage(totalLikesReceived: Int(totalLikesReceived), sender: sender)
    }
}
