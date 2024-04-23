//
//  TUIGiftAnimationManager.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import Foundation

typealias TUIDequeueClosure = (TUIGiftData) -> Void



class TUIGiftAnimationManager {
    var queueArray: [TUIGiftData]
    var dequeueClosure: TUIDequeueClosure?
    var simulcastCount: Int
    var playCount: Int

    init() {
        simulcastCount = 99
        playCount = 0
        queueArray = []
    }

    init(simulcastCount: Int) {
        self.simulcastCount = simulcastCount
        playCount = 0
        queueArray = []
    }

    func enqueue(giftModel: TUIGift, sender: TUIGiftUser, receiver: TUIGiftUser, giftCount: Int) {
        let data = TUIGiftData(gift:giftModel, giftCount: giftCount, sender: sender, receiver: receiver)
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            if self.playCount < self.simulcastCount {
                self.playCount += 1
                var giftData = self.queueArray.first
                if giftData != nil {
                    self.queueArray.removeFirst()
                    self.queueArray.append(data)
                } else {
                    giftData = data
                }
                self.dequeueClosure?(giftData ?? data)
            } else {
                self.queueArray.append(data)
            }
        }
    }

    func dequeue(closure: @escaping TUIDequeueClosure) {
        dequeueClosure = closure
    }

    func finishPlay() {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            guard let gift = self.queueArray.first else {
                self.playCount -= 1
                self.playCount = max(0, self.playCount)
                return
            }
            self.queueArray.removeFirst()
            self.dequeueClosure?(gift)
        }
    }

    func clearData() {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            self.queueArray = []
            self.playCount = 0
        }
    }
}
