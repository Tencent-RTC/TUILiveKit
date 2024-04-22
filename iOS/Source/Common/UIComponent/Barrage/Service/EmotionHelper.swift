//
//  EmotionHelper.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/3.
//

import UIKit

class EmotionHelper {
    static let shared = {
        EmotionHelper()
    }()

    private init() {}

    var emotions: [Emotion] = []
    private var cacheTotalImageDictionary: [String: UIImage] = [:]
    private var cacheAttributedDictionary: [String: NSAttributedString] = [:]
    private var regularExpression: NSRegularExpression = try! NSRegularExpression(pattern: "\\[[a-zA-Z0-9\\u4e00-\\u9fa5]+\\]", options: [])

    func useDefaultEmotions() {
        createTotalEmotions()
        cacheTotalImage()
    }

    func setEmotions(emotions: [Emotion]) {
        self.emotions = emotions
        cacheTotalImageDictionary = [:]
        cacheTotalImage()
    }

    func cacheTotalImage() {
        if cacheTotalImageDictionary.count == 0 {
            var emotionImageDictionary: [String: UIImage] = [:]
            for emotion in emotions {
                if emotion.image.size.width != 0 {
                    emotion.image = UIImage(named: emotion.identifier, in: Bundle.liveBundle, compatibleWith: nil) ?? UIImage()
                }
                emotionImageDictionary[emotion.displayName] = emotion.image
            }
            cacheTotalImageDictionary = emotionImageDictionary
        }
    }

    func obtainImagesAttributedString(byText text: String, font: UIFont) -> NSMutableAttributedString {
        let matches = regularExpression.matches(in: text, range: NSRange(location: 0, length: text.count))
        let intactAttributedString = NSMutableAttributedString(string: text)
        for match in matches.reversed() {
            guard let emojiRange = Range(match.range, in: text) else { return NSMutableAttributedString(string: "") }
            let emojiKey = String(text[emojiRange])

            var useCache = true
            if #available(iOS 15.0, *) {
                // Cached NSAttributedString cannot be used on ios15, only one expression will appear, but it can be used on ios14 and before.
                useCache = false
            }
            let imageAttributedString = obtainImageAttributedString(byImageKey: emojiKey, font: font, useCache: useCache)
            intactAttributedString.replaceCharacters(in: match.range, with: imageAttributedString)
        }
        // Fixed an issue where font changed due to inserting AttributeString;
        // Prevents the textView font from getting smaller after inserting an expression
        intactAttributedString.addAttribute(.font, value: font, range: NSRange(location: 0, length: intactAttributedString.length))
        return intactAttributedString
    }

    func obtainImageAttributedString(byImageKey imageKey: String, font: UIFont, useCache: Bool) -> NSAttributedString {
        if !useCache {
            let image = cacheTotalImageDictionary[imageKey]
            if image == nil {
                return NSAttributedString(string: "")
            }
            let emotionAttachment = EmotionAttachment()
            emotionAttachment.displayText = imageKey
            emotionAttachment.image = image
            emotionAttachment.bounds = CGRect(x: 0, y: font.descender, width: font.lineHeight, height: font.lineHeight)
            let imageAttributedString = NSAttributedString(attachment: emotionAttachment)
            return imageAttributedString
        }

        let keyFont = String(format: "%@%.1f", imageKey, font.pointSize)
        var imageAttributedString = cacheAttributedDictionary[keyFont]
        if imageAttributedString == nil {
            return NSAttributedString(string: "")
        }
        let image = cacheTotalImageDictionary[imageKey]
        if image == nil {
            return NSAttributedString(string: "")
        }
        let emotionAttachment = EmotionAttachment()
        emotionAttachment.displayText = imageKey
        emotionAttachment.image = image
        emotionAttachment.bounds = CGRect(x: 0, y: font.descender, width: font.lineHeight, height: font.lineHeight)
        imageAttributedString = NSAttributedString(attachment: emotionAttachment)
        cacheAttributedDictionary[keyFont] = imageAttributedString
        return imageAttributedString ?? NSAttributedString(string: "")
    }

    private func createTotalEmotions() {
        emotions = []
        emotions.append(Emotion(identifier: "live_barrage_emoji_0", displayName: "[微笑]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_1", displayName: "[期待]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_2", displayName: "[眨眼]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_3", displayName: "[大笑]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_4", displayName: "[姨母笑]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_5", displayName: "[哈哈哈]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_6", displayName: "[愉快]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_7", displayName: "[无语]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_8", displayName: "[惊讶]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_9", displayName: "[悲伤]"))

        emotions.append(Emotion(identifier: "live_barrage_emoji_10", displayName: "[得意]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_11", displayName: "[傻了]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_12", displayName: "[色]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_13", displayName: "[憨笑]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_14", displayName: "[亲亲]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_15", displayName: "[大哭]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_16", displayName: "[哭笑]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_17", displayName: "[困]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_18", displayName: "[口罩]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_19", displayName: "[恐惧]"))

        emotions.append(Emotion(identifier: "live_barrage_emoji_20", displayName: "[龇牙]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_21", displayName: "[发怒]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_22", displayName: "[打哈欠]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_23", displayName: "[机智]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_24", displayName: "[星星眼]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_25", displayName: "[闭嘴]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_26", displayName: "[叹气]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_27", displayName: "[呵呵]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_28", displayName: "[收声]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_29", displayName: "[惊喜]"))

        emotions.append(Emotion(identifier: "live_barrage_emoji_30", displayName: "[白眼]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_31", displayName: "[OK]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_32", displayName: "[便便]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_33", displayName: "[怪兽]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_34", displayName: "[恶魔]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_35", displayName: "[恶魔怒]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_36", displayName: "[衰]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_37", displayName: "[猪]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_38", displayName: "[牛]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_39", displayName: "[AI]"))

        emotions.append(Emotion(identifier: "live_barrage_emoji_40", displayName: "[骷髅]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_41", displayName: "[炸弹]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_42", displayName: "[咖啡]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_43", displayName: "[蛋糕]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_44", displayName: "[啤酒]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_45", displayName: "[花]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_46", displayName: "[瓜]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_47", displayName: "[壕]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_48", displayName: "[爱心]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_49", displayName: "[月亮]"))

        emotions.append(Emotion(identifier: "live_barrage_emoji_50", displayName: "[太阳]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_51", displayName: "[星星]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_52", displayName: "[红包]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_53", displayName: "[庆祝]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_54", displayName: "[福]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_55", displayName: "[发]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_56", displayName: "[服]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_57", displayName: "[禁]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_58", displayName: "[666]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_59", displayName: "[857]"))

        emotions.append(Emotion(identifier: "live_barrage_emoji_60", displayName: "[刀]"))
        emotions.append(Emotion(identifier: "live_barrage_emoji_61", displayName: "[赞]"))

        for emotion in emotions {
            emotion.image = UIImage(named: emotion.identifier, in: Bundle.liveBundle, compatibleWith: nil) ?? UIImage()
        }
    }
}
