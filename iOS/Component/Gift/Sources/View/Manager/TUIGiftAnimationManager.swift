//
//  TUIGiftAnimationManager.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/1/2.
//

import Foundation
import TUICore
import RTCCommon

typealias TUIDequeueClosure = (TUIGiftData) -> Void

class TUIGiftAnimationManager {
    // Max count of dataSource, default is 3
    let queueArray = ListManager<TUIGiftData>(maxLength: 3)
    var dequeueClosure: TUIDequeueClosure?
    var simulcastCount: Int
    var playCount: Int

    init(simulcastCount: Int = 99) {
        self.simulcastCount = simulcastCount
        playCount = 0
    }

    func enqueue(giftData: TUIGiftData) {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            if self.playCount < self.simulcastCount {
                self.playCount += 1
                self.dequeueClosure?(giftData)
            } else {
                addGiftInQueue(giftData: giftData)
            }
        }
    }

    func finishPlay() {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            self.playCount = max(0, self.playCount - 1)
            if let gift = self.queueArray.popFirst() {
                self.playCount += 1
                self.dequeueClosure?(gift)
            }
        }
    }

    func clearData() {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            self.queueArray.removeAll()
            self.playCount = 0
        }
    }
}

extension TUIGiftAnimationManager {
    private func addGiftInQueue(giftData: TUIGiftData) {
        // Keep self in front
        var firstOtherIndex: Int = queueArray.count
        for (i, data) in queueArray().enumerated() {
            if !data.sender.isSelf {
                firstOtherIndex = i
                break
            }
        }
        if giftData.sender.isSelf {
            if firstOtherIndex == 0 {
                queueArray.insert(giftData, at: firstOtherIndex)
            } else {
                queueArray.remove(at: firstOtherIndex)
                queueArray.insert(giftData, at: firstOtherIndex)
            }
        } else {
            if firstOtherIndex == 0 || firstOtherIndex > 1 {
                queueArray.append(giftData)
            } else {
                queueArray.remove(at: firstOtherIndex)
                queueArray.append(giftData)
            }
        }
    }
}

fileprivate extension TUIGiftUser {
    var isSelf: Bool {
        userId == TUILogin.getUserID()
    }
}
