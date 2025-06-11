//
//  GiftCloudServer.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/3.
//

import Foundation
import TUICore
import RTCCommon
import TUILiveComponent

class GiftCloudServer: IGiftCloudServer {
    static var shared = GiftCloudServer()
    private init() {
        queryGiftInfoList { error, giftList in
            if error == .noError {
                TUIGiftStore.shared.giftList = giftList
            }
        }
    }
    
    func initialize() {
        
    }
    
    func rechargeBalance(callback: @escaping (TUIGiftServerError, Int) -> Void) {
        
    }

    func queryBalance(callback: @escaping (TUIGiftServerError, Int) -> Void) {
        
    }

    func queryGiftInfoList(callback: @escaping (TUIGiftServerError, [TUIGift]) -> Void) {
        DispatchQueue.global().async {
            let giftUrl = self.hasTCEffectPlayer() ? TE_GIFT_DATA_URL : GIFT_DATA_URL
            guard let url = URL(string: giftUrl) else { return }
            let session = URLSession.shared
            let task = session.dataTask(with: url) { data, _, error in
                if error != nil {
                    callback(.operationFailed, [])
                    return
                }
                do {
                    guard let data = data else { return }
                    let json = try JSONSerialization.jsonObject(with: data, options: []) as! [String: Any]
                    guard let giftList = json["giftList"] as? [Any] else { return }
                    let gifts = self.initTUIGifts(giftList: giftList)
                    callback(.noError, gifts)
                } catch let error as NSError {
                    print(error.localizedDescription)
                }
            }
            task.resume()
        }
    }

    func sendGift(sender: String, 
                  receiver: String,
                  giftModel: TUIGift,
                  giftCount: Int,
                  callback: @escaping (TUIGiftServerError, Int) -> Void) {
        
    }
    
    private func initTUIGifts(giftList: [Any]) -> [TUIGift] {
        var giftArray: [TUIGift] = []
        let language = TUIGlobalization.getPreferredLanguage() ?? "en"
        
        for gift in giftList {
            guard let dict = gift as? [String: Any] else { return [] }
            let giftId = (dict["giftId"] as? String) ?? ""
            let imageUrl = (dict["imageUrl"] as? String) ?? ""
            let animationUrl = (dict["animationUrl"] as? String) ?? ""
            let giftName = getGiftName(from: dict, language: language)
            let price = (dict["price"] as? Int) ?? 99
            let model = TUIGift(giftId: giftId, giftName: giftName, imageUrl: imageUrl, animationUrl: animationUrl, price: price, extInfo: [:])
            giftArray.append(model)
        }
        return giftArray
    }
    
    private func hasTCEffectPlayer() -> Bool {
        let service = TUICore.getService("TUIEffectPlayerService")
        return service != nil
    }
    
    private func getGiftName(from giftDict: [String: Any], language: String) -> String {
        switch language {
        case "en":
            return giftDict["giftName_en"] as? String ?? ""
        case "zh-Hans":
            return giftDict["giftName"] as? String ?? ""
        case "zh-Hant":
            return giftDict["giftName_zh_hant"] as? String ?? ""
        default:
            return giftDict["giftName_en"] as? String ?? ""
        }
    }
}
