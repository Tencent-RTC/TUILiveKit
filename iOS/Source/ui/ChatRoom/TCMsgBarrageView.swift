//
//  TCMsgBarrageView.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/22.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation

/// TCMsgBarrageView
class TCMsgBarrageView: UIView {
    
    lazy var lastAnimateView: UIView = {
        return UIView()
    }()
    lazy var unUsedAnimateViewArray: [UIView] = {
        return []
    }()
    private var msgModelArray: [TCMsgModel] = [TCMsgModel]()
    private var nextAnimateViewStartTime: CGFloat = 0.0
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        unUsedAnimateViewArray = [UIView]()
        msgModelArray = [TCMsgModel]()
        nextAnimateViewStartTime = 1
        initUI()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    func initUI() {
        let view = creatAnimateView()
        unUsedAnimateViewArray.append(view)
        startAnimation()
    }
    
    func creatAnimateView() -> UIView {
        let view = UIView(frame: CGRect(x: SCREEN_WIDTH + CGFloat(MSG_ANIMATE_VIEW_SPACE), y: 0, width: 0, height: height))
        let headImageView = UIImageView(frame: CGRect(x: 2, y: 2, width: height - 4, height: height - 4))
        headImageView.layer.cornerRadius = headImageView.width / 2
        headImageView.layer.masksToBounds = true
        let userNameLabel = UILabel(frame: CGRect(x: headImageView.right + 2, y: 0, width: 0, height: 14))
        userNameLabel.backgroundColor = UIColor.clear
        let userMsgLabel = UILabel(frame: CGRect(x: userNameLabel.left, y: userNameLabel.bottom, width: 0, height: height - userNameLabel.height))
        userMsgLabel.backgroundColor = UIColor.clear
        let backImageView = UIImageView(frame: CGRect(x: 0, y: 0, width: 0, height: height))
        let image = UIImage(named: "Barrage", in: liveRoomBundle(), compatibleWith: nil)
        let newImage = image?.resizableImage(withCapInsets: UIEdgeInsets(top: 24, left: 35, bottom: 5, right: 10), resizingMode: .stretch)
        backImageView.image = newImage
        view.insertSubview(backImageView, at: 0)
        view.insertSubview(headImageView, at: 1)
        view.insertSubview(userNameLabel, at: 2)
        view.insertSubview(userMsgLabel, at: 3)
        addSubview(view)
        return view   
    }
    
    func startAnimation() {
        if  msgModelArray.count > 0{
            if let msgModel = msgModelArray.last {
                if unUsedAnimateViewArray.count > 0 {
                    if let view = unUsedAnimateViewArray.last {
                        animate(view, msg: msgModel)
                    }
                } else {
                    let view = creatAnimateView()
                    animate(view, msg: msgModel)
                }
            }
            msgModelArray.removeLast()
        }
        DispatchQueue.main.asyncAfter(deadline:.now() + Double(nextAnimateViewStartTime), execute: { [weak self] in
            self?.startAnimation()
        })
    }
    func stopAnimation() {
        removeAllSubViews()
    }
    
    func bulletNewMsg(_ msgModel: TCMsgModel?) {
        if let msgModel = msgModel {
            msgModelArray.insert(msgModel, at: 0)
        }
    }
    func animate(_ aView: UIView, msg msgModel: TCMsgModel) {
        guard let newView = resetViewFrame(aView, msg: msgModel) else { return }
        
        let duration0: Float = Float(SCREEN_WIDTH) + Float(MSG_ANIMATE_VIEW_SPACE) + Float(newView.width ?? 0)
        let duration1: Float = Float(2 * SCREEN_WIDTH)
        let duration: Float =  Float(MSG_ANIMATE_DURANTION) * duration0 / duration1
        let duration2  = Float(newView.width ?? 0) + Float(MSG_ANIMATE_VIEW_SPACE)
        let duration3 = Float(SCREEN_WIDTH) + Float(newView.width ?? 0) + Float(MSG_ANIMATE_VIEW_SPACE)
        nextAnimateViewStartTime = CGFloat(duration * (duration2 / duration3))
        lastAnimateView = newView ?? UIView()
        
        UIView.animate(withDuration: TimeInterval(duration), delay: 0, options: .curveLinear, animations: {
            let frame = newView.frame
            newView.frame = CGRect(x: -frame.size.width, y: 0, width: frame.size.width, height: frame.size.height)
        }) { [weak self] finished in
            guard let `self` = self else { return }
            newView.frame = CGRect(x: SCREEN_WIDTH + CGFloat(MSG_ANIMATE_VIEW_SPACE), y: 0, width: 0, height: self.height)
            self.unUsedAnimateViewArray.insert(newView, at: 0)
        }
    }
    
    func resetViewFrame(_ aView: UIView?, msg msgModel: TCMsgModel?) -> UIView? {
        guard let aView = aView else { return nil }
        let userName = getAttributedUserName(from: msgModel)
        let nameRect = userName?.boundingRect(with: CGSize(width: CGFloat.greatestFiniteMagnitude, height: 14), options:
         .usesLineFragmentOrigin, context: nil)
        
        let userMsg = getAttributedUserMsg(from: msgModel)
        let msgRect = userMsg?.boundingRect(with: CGSize(width: CGFloat.greatestFiniteMagnitude, height: height - 14), options:
         .usesLineFragmentOrigin, context: nil)
        let viewArray = aView.subviews
        if viewArray.count >= 4 {
            let headImageView:UIImageView = viewArray[1] as? UIImageView ?? UIImageView()
            if let userHeadImageUrl = msgModel?.userHeadImageUrl {
                headImageView.kf.setImage(with:  URL(string: TCUtil.transImageURL2HttpsURL(userHeadImageUrl) ?? ""), placeholder:
                 UIImage(named: "default_user", in: liveRoomBundle(), compatibleWith: nil), options: nil, progressBlock: nil,
                 completionHandler: nil)
            }
            let userNamelabel:UILabel = viewArray[2] as? UILabel ?? UILabel()
            userNamelabel.attributedText = userName
            if let nameRect = nameRect {
                userNamelabel.width = nameRect.size.width
            }
            
            let userMsgLabel:UILabel = viewArray[3] as? UILabel ?? UILabel()
            userMsgLabel.attributedText = userMsg
            if let msgRect = msgRect {
                userMsgLabel.width = msgRect.size.width
            }
            aView.width = headImageView.width + 4 + 10 + ((userNamelabel.width > userMsgLabel.width ? userNamelabel.width : userMsgLabel.width))
            let backImageView = viewArray[0]
            backImageView.width = aView.width
        }
        return aView
    }
    
    func getAttributedUserName(from msgModel: TCMsgModel?) -> NSAttributedString? {
        let attribute = NSMutableAttributedString()
        let userName = NSMutableAttributedString(string: msgModel?.userName ?? "")
        attribute.append(userName)
        
        attribute.addAttribute(.font, value: UIFont.systemFont(ofSize: 12), range: NSRange(location: 0, length: attribute.length))
        attribute.addAttribute(.foregroundColor, value: UIColor.white, range: NSRange(location: 0, length: userName.length))
        return attribute
    }
    
    func getAttributedUserMsg(from msgModel: TCMsgModel?) -> NSAttributedString? {
        
        let attribute = NSMutableAttributedString()
        let userMsg = NSMutableAttributedString(string: msgModel?.userMsg ?? "")
        attribute.append(userMsg)
        attribute.addAttribute(.font, value: UIFont.systemFont(ofSize: 12), range: NSRange(location: 0, length: attribute.length))
        attribute.addAttribute(.foregroundColor, value: UIColor(hex: "0x89eede"), range: NSRange(location: 0, length: userMsg.length))
        return attribute
    }
    
}
