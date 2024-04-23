//
//  CustomBarrageCell.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/4/8.
//

import UIKit

class CustomBarrageCell {
    static func getCustomCell(barrage: TUIBarrage) -> UIView {
        let attributedString = getUserNameAttributedString(barrage: barrage)
        attributedString.append(getBarrageContentAttributedString(barrage: barrage))
        let label = UILabel()
        label.mm_w = 50.scale375()
        label.attributedText = attributedString
        label.sizeToFit()
        return label
    }
    
    private static func getUserNameAttributedString(barrage: TUIBarrage)
        -> NSMutableAttributedString {
        let userName = barrage.user.userName
        let userNameAttributes: [NSAttributedString.Key: Any] =
            [.foregroundColor: UIColor.lightBlueColor, .font: UIFont.systemFont(ofSize: 12)]
        return NSMutableAttributedString(string: userName, attributes: userNameAttributes)
    }

    private static func getBarrageContentAttributedString(barrage: TUIBarrage)
        -> NSMutableAttributedString {
        let colors: [UIColor] = [.red,.blue,.yellow]
        guard let giftName = barrage.extInfo["gift_name"]?.value as? String,
              let giftCount = barrage.extInfo["gift_count"]?.value as? Int,
              let giftIconUrl = URL(string:barrage.extInfo["gift_icon_url"]?.value as? String ?? ""),
              let receiver = barrage.extInfo["gift_receiver_username"]?.value as? String
        else { return NSMutableAttributedString()}
        let mutableAttributedString = NSMutableAttributedString(string: "")
            let sendAttributedText = NSAttributedString(string: .sendText, attributes: [.foregroundColor: UIColor.white])
        mutableAttributedString.append(sendAttributedText)
            
        let receiverAttributedText = NSAttributedString(string: receiver, attributes: [.foregroundColor: UIColor.lightBlueColor])
        mutableAttributedString.append(receiverAttributedText)
            
        let random = arc4random_uniform(3)
        let giftNameAttributedString = NSAttributedString(string: giftName,attributes: [.foregroundColor: colors[Int(random)]])
        mutableAttributedString.append(giftNameAttributedString)
            
        let giftIconAttachment = NSTextAttachment()
        loadImageFromUrl(url: giftIconUrl) { image in
            if let image = image {
                giftIconAttachment.image = image
            } else {
                giftIconAttachment.image = nil
            }
        }
        giftIconAttachment.bounds = CGRect(x: 0, y: 0, width: 12, height: 12)
        let giftImageAttributedString = NSAttributedString(attachment: giftIconAttachment)
        mutableAttributedString.append(giftImageAttributedString)
        
        let giftCountAttributedString = NSAttributedString(string: "x\(giftCount)", attributes: [.foregroundColor: UIColor.white])
        mutableAttributedString.append(giftCountAttributedString)

        mutableAttributedString.addAttribute(.font,
                                             value: UIFont.systemFont(ofSize: 12),
                                             range: NSRange(location: 0, length: mutableAttributedString.length))
        return mutableAttributedString
    }
    
    private static func loadImageFromUrl(url: URL, completion: @escaping (UIImage?) -> Void) {
        let imageView = UIImageView()
        imageView.kf.setImage(with: url) { result in
            switch result {
            case .success(let imageResult):
                completion(imageResult.image)
            case .failure(_):
                completion(nil)
                break
            }
        }
    }
}

private extension String {
    static let sendText = localized("live.customBarrageCell.send")
}
