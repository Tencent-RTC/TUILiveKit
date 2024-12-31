//
//  GiftCloudServer.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/3.
//

import Foundation
import TUICore
import RTCCommon

class GiftCloudServer: IGiftCloudServer {
    private var balance = 500

    func rechargeBalance(callback: @escaping (TUIGiftServerError, Int) -> Void) {
        balance += 100
        callback(.noError, balance)
    }

    func queryBalance(callback: @escaping (TUIGiftServerError, Int) -> Void) {
        callback(.noError, balance)
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
        guard giftCount > 0 else {
            callback(.paramError, balance)
            return
        }

        let newBalance = balance - giftCount * giftModel.price
        if newBalance >= 0 {
            balance = newBalance
            callback(.noError, newBalance)
        } else {
            callback(.balanceInsufficient, balance)
        }
    }
    
    private func initTUIGifts(giftList: [Any]) -> [TUIGift] {
        var giftArray: [TUIGift] = []
        
        for gift in giftList {
            guard let dict = gift as? [String: Any] else { return [] }
            let giftId = (dict["giftId"] as? String) ?? ""
            let imageUrl = (dict["imageUrl"] as? String) ?? ""
            let animationUrl = (dict["animationUrl"] as? String) ?? ""
            let giftName = LocalizedLanguage.isChinese ? ((dict["giftName"] as? String) ?? "") : ((dict["giftName_en"] as? String) ?? "")
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
}
