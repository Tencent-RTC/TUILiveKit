//
//  TCUtil.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/7/15.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation
import Accelerate

class TCUtil: NSObject {
    class func transImageURL2HttpsURL(_ httpURL: String) -> String? {
        if URL(string: httpURL ) == nil {
            return nil
        }
        var httpsURL = httpURL
        if httpURL.hasPrefix("http:"){
            httpsURL = httpURL.replacingOccurrences(of: "http:", with: "https:")
        } else {
            if !(httpURL.hasPrefix("https:")) {
                httpsURL = "https:\(httpURL)"
            }
        }
        return httpsURL
    }
    
    class func gsImage(_ image: UIImage, withGsNumber blurAmount: CGFloat) -> UIImage? {
        guard let ciImage = CIImage(image: image) else { return nil }
        guard let blurFilter = CIFilter(name: "CIGaussianBlur") else {
            return nil
        }
        blurFilter.setValue(image, forKey: "inputImage")
        blurFilter.setValue(blurAmount, forKey: "inputRadius")
        guard let outputImage = blurFilter.outputImage else { return nil }
        let context = CIContext(options: nil)
        guard let cgImage = context.createCGImage(outputImage, from: ciImage.extent) else { return nil }
        return UIImage(cgImage: cgImage)
    }
    
    class func scale(_ image: UIImage, scaleTo size: CGSize) -> UIImage? {
        UIGraphicsBeginImageContextWithOptions(size,false,UIScreen.main.scale)
        image.draw(in: cgRectMake_Auto(0, 0, size.width, size.height))
        let reSizeImage:UIImage = UIGraphicsGetImageFromCurrentImageContext() ?? UIImage()
        return reSizeImage
    }
    
    class func clipImage(_ image: UIImage?, in rect: CGRect) -> UIImage? {
        guard let image = image else {
            return nil
        }
        var newRect = rect
        newRect.origin.x *= image.scale
        newRect.origin.y *= image.scale
        newRect.size.width *= image.scale
        newRect.size.height *= image.scale
        guard let cgImage = image.cgImage?.cropping(to: newRect) else {
            return nil
        }
        let resultImage = UIImage(cgImage: cgImage, scale: image.scale, orientation: image.imageOrientation)
        return resultImage
    }
    
    class func height(forString textView: UITextView?, andWidth width: Float) -> Float {
        let sizeToFit = textView?.sizeThatFits(CGSize(width: CGFloat(width), height: CGFloat(MAXFLOAT)))
        return Float(sizeToFit?.height ?? 0.0)
    }
    
    class func toastTip(_ toastInfo: String?, parentView: UIView?) {
        var frameRC = UIScreen.main.bounds
        frameRC.origin.y = frameRC.size.height - (IPHONE_X ? 144 : 110)
        
        frameRC.size.height -= 110
        let toastView = UITextView()
        
        toastView.isEditable = false
        toastView.isSelectable = false
        
        frameRC.size.height = CGFloat(self.height(forString: toastView, andWidth: Float(frameRC.size.width)))
        
        toastView.frame = frameRC
        toastView.text = toastInfo
        toastView.textColor = UIColor.white
        toastView.backgroundColor = UIColor.white
        toastView.backgroundColor = UIColor(hex: "29CC85")
        parentView?.addSubview(toastView)
        let popTime = DispatchTime.now() + 2
        
        DispatchQueue.main.asyncAfter(deadline: popTime, execute: {
            toastView.removeFromSuperview()
        })
    }
    
    class func subString(_ string: String?, length count: Int) -> String? {
        if let data = string?.data(using: .utf8) {
            if data.count <= count{
                return string
            } else {
                let nsData = data as NSData
                let rawPtr = nsData.bytes
                let resultData = Data(bytes: rawPtr, count: count)
                if let res = String(data: resultData, encoding: .utf8) {
                    return res
                } else {
                    return ""
                }
            }
        }
        return ""
    }
    
}

class TCFrequeControl: NSObject{
    var countsLimit = 0
    var curCounts = 0
    var secondsLimit: TimeInterval = 0.0
    var preTime: TimeInterval = 0.0
    init(counts: Int, andSeconds seconds: TimeInterval) {
        super.init()
        countsLimit = counts
        secondsLimit = seconds
        curCounts = 0
        preTime = 0
    }
    func canTrigger() -> Bool {
        let time = Date().timeIntervalSince1970
        if preTime == 0 || time - preTime > secondsLimit {
            preTime = time
            curCounts = 0
        }
        if curCounts >= countsLimit {
            return false
        }
        curCounts += 1
        return true
    }
    
}


