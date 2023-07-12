//
//  TRTCMeetingMoreControllerUI.swift
//  TRTCScenesDemo
//
//  Created by J J on 2020/5/14.
//  Copyright © 2022 Tencent. All rights reserved.
//

import UIKit
final class TRTCLiveRoomMoreControllerUI: TRTCLiveRoomMoreViewController {
    
    var volumePromptCallback: ((Bool) -> Void)? = nil
    
    let screenHeight = UIScreen.main.bounds.size.height
    let screenWidth = UIScreen.main.bounds.size.width
    
    var selectIndex = 0
    
    lazy var segView:CenterSegmentView = {
        let nameArray : [String] = [.videoText]
        let vcVideo = TRTCLiveRoomMoreViewVideoVC()
        let controllers = [vcVideo]
        let view = CenterSegmentView(frame: CGRect(x: 0, y: 16, width: self.view.bounds.size.width, height: self.view.bounds.size.height), controllers: controllers, titleArray: nameArray, selectIndex: self.selectIndex, lineHeight: 4)
        
        view.lineSelectedColor = UIColor(hex: "006EFF") ?? .blue
        view.titleSelectColor = UIColor(hex: "006EFF") ?? .blue
        view.lineHeight = 4
        
        return view
    }()
    
    override var controllerHeight: CGFloat{
        return screenHeight / 2.0 - 50
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        view.layer.masksToBounds = true
        view.layer.cornerRadius = 10
        view.backgroundColor = .white
        view.addSubview(self.segView)
    }
}

/// MARK: - internationalization string
fileprivate extension String {
    static var videoText: String {
        liveRoomLocalize("Demo.TRTC.LiveRoom.video")
    }
}
