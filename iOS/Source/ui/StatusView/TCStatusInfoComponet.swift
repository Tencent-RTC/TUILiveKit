//
//  TCStatusInfoComponet.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/21.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation
import UIKit

class TCStatusInfoComponet: NSObject{
    var userID    : String?
    var videoView : UIView?
    var btnKickout: UIButton?
    
    var playUrl   = ""
    var eventMsg  = ""
    var linkFrame = CGRect.zero
    var pending   = false
    lazy var loadingBackground : UIView = {
        let loadingBackground = UIView()
        loadingBackground.isHidden = true
        loadingBackground.backgroundColor = UIColor.black
        loadingBackground.alpha = 0.5
        
        return loadingBackground
    }()
    lazy var loadingImageView: UIImageView = {
        var array = [UIImage]()
        for i in 0...14 {
            if let image = UIImage(named: "loading_image\(i).png", in: liveRoomBundle(), compatibleWith: nil) {
                array.append(image)
            }
        }
        let loadingImageView = UIImageView()
        loadingImageView.animationImages = array
        loadingImageView.animationDuration = 1
        loadingImageView.isHidden = true
        return loadingImageView
    }()
    
    
    func setVideoView(_ videoView: UIView?) {
        self.videoView = videoView
        initLoading(videoView)
    }
    
    func initLoading(_ view: UIView?) {
        guard let view  = view else {
            return
        }
        let rect = view.frame
        loadingBackground.frame = CGRect(x: 0, y: 0, width: rect.width , height: rect.height)
        view.addSubview(loadingBackground)
        view.addSubview(loadingImageView)
        loadingBackground.snp.remakeConstraints({ make in
            make.edges.equalToSuperview()
        })
        loadingImageView.snp.makeConstraints({ make in
            make.width.height.equalTo(50)
            make.center.equalToSuperview()
        })
        
    }
    
    func emptyPlayInfo() {
        pending = false
        userID = ""
        playUrl = ""
        eventMsg = ""
    }
    
    func startLoading() {
        loadingBackground.isHidden = false
        loadingImageView.isHidden = false
        loadingImageView.startAnimating()
    }
    
    func stopLoading() {
        loadingBackground.isHidden = true
        loadingImageView.isHidden = true
        loadingImageView.stopAnimating()
    }
    
    func startPlay(_ playUrl: String?) {
        if let btnKickout = btnKickout {
            btnKickout.isHidden = true
        }
    }
    
    func stopPlay() {
        stopLoading()
        if let btnKickout = btnKickout {
            btnKickout.isHidden = true
        }
    }
}
