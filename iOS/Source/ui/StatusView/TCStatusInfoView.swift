//
//  TCStatusInfoView.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/21.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation
import UIKit

class TCStatusInfoView: NSObject{
    var pending = false
    var userID: String?
    var playUrl: String?
    var videoView: UIView?
    var btnKickout: UIButton?
    var linkFrame = CGRect.zero
    
    lazy var loadingBackground: UIView = {
        let loadingBackground = UIView(frame: CGRect.zero)
        loadingBackground.isHidden = true
        loadingBackground.backgroundColor = UIColor.black
        loadingBackground.alpha = 0.5
        loadingBackground.snp.remakeConstraints({ make in
            make.edges.equalToSuperview()
        })
        return loadingBackground
    }()
    
    lazy var loadingImageView: UIImageView = {
        var array = [UIImage]()
        for i in 0...14 {
            array.append(UIImage(named: "loading_image\(i).png") ?? UIImage())
        }
        let loadingImageView = UIImageView()
        loadingImageView.animationImages = array
        loadingImageView.animationDuration = 1
        loadingImageView.isHidden = true
        loadingImageView.snp.makeConstraints({ make in
            make.width.height.equalTo(50)
            make.center.equalToSuperview()
        })
        return loadingImageView
    }()
    private var eventMsg: String?
    func setVideoView(_ videoView: UIView?) {
        self.videoView = videoView
        initLoading(videoView)
    }
    
    func initLoading(_ view: UIView?) {
        guard let view  = view else {
            return
        }
        let rect = view.frame
        loadingBackground = UIView(frame: CGRect(x: 0, y: 0, width: rect.width , height: rect.height ))
        view.addSubview(loadingBackground)
        view.addSubview(loadingImageView)
        
    }
    
    func emptyPlayInfo() {
        pending = false
        userID = ""
        playUrl = ""
        eventMsg = nil
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
        if btnKickout != nil {
            btnKickout?.isHidden = true
        }
    }
    
    func stopPlay() {
        stopLoading()
        if btnKickout != nil {
            btnKickout?.isHidden = true
        }
    }
}
