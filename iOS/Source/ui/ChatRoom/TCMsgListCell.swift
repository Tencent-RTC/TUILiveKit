//
//  TCMsgListCell.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/21.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation
import UIKit
import Kingfisher

let _arryColor = [UIColor.green, UIColor.red, UIColor.blue]
var _index = 0
typealias TCLiveTopClick = () -> Void
///  TCMsgListCell
class TCMsgListCell: UITableViewCell {
    lazy var msgView: UIView = {
        let msgView = UIView(frame: CGRect.zero)
        msgView.backgroundColor = UIColor.black.withAlphaComponent(0.3)
        msgView.layer.cornerRadius = 8
        return msgView
    }()
    lazy var msgLabel: UILabel = {
        let msgLabel = UILabel(frame: CGRect.zero)
        msgLabel.numberOfLines = 0
        msgLabel.font = UIFont.systemFont(ofSize: CGFloat(MSG_TABLEVIEW_LABEL_FONT))
        return msgLabel
    }()
    private var msgBkView: UIImageView?
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        msgView.addSubview(msgLabel)
        contentView.addSubview(msgView)
    }
    public required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    func refresh(with msgModel: TCMsgModel?) {
        msgLabel.attributedText = msgModel?.msgAttribText
        msgLabel.width = CGFloat(MSG_TABLEVIEW_WIDTH - 20)
        msgLabel.sizeToFit()
    }
    
    class func getAttributedString(from msgModel: TCMsgModel) -> NSAttributedString? {
        let attribute = NSMutableAttributedString()
        if msgModel.msgType == .normal || msgModel.msgType == .danmaMsg  {
            let userName = NSMutableAttributedString(string: msgModel.userName ?? "")
            attribute.append(userName)
            let userMsg = NSMutableAttributedString(string: msgModel.userMsg ?? "")
            attribute.append(NSMutableAttributedString(string: "："))
            attribute.append(userMsg)
            attribute.addAttribute(.font, value: UIFont.systemFont(ofSize: CGFloat(MSG_TABLEVIEW_LABEL_FONT)), range: NSRange(location:
             0, length: attribute.length))
            _index = _index % _arryColor.count
            attribute.addAttribute(.foregroundColor, value: UIColor.white, range: NSRange(location: 0, length: attribute.length))
            attribute.addAttribute(.foregroundColor, value: _arryColor[_index], range: NSRange(location: 0, length: userName.length))
            _index += 1
            
        } else {
            let msgShow = NSMutableAttributedString(string: "\(liveRoomLocalize("Demo.TRTC.LiveRoom.notice"))" +
             "\(msgModel.userName ?? "")\(msgModel.userMsg ?? "")")
            attribute.append(msgShow)
            attribute.addAttribute(.font, value: UIFont.systemFont(ofSize: CGFloat(MSG_TABLEVIEW_LABEL_FONT)), range: NSRange(location:
             0, length: attribute.length))
            attribute.addAttribute(.foregroundColor, value: UIColor(red: 241 / 255.0, green: 43 / 255.0, blue: 91 / 255.0, alpha: 1),
             range: NSRange(location: 0, length: msgShow.length))
        }
        return attribute
    }
}

public class TCShowLiveTopView: UIView {
    var clickHead: TCLiveTopClick?
    private var hostImage: UIImageView
    private var durationImage: UIImageView?
    private var durationLabel: UILabel
    private var roomIdLabel: UILabel
    
    private var audienceLabel: UILabel
    private var timer: Timer?
    private var startTime = 0
    private var liveDuration = 0
    private var audienceCount = 0
    private var likeCount = 0
    private var totalViewerCount = 0
    private var isHost = false
    private var roomName: String
    private var hostFaceUrl: String
    init(frame: CGRect, isHost: Bool, roomName: String, audienceCount: Int, likeCount: Int, hostFaceUrl: String) {
        hostImage = UIImageView()
        durationLabel = UILabel()
        audienceLabel = UILabel()
        roomIdLabel = UILabel()
        self.audienceCount = audienceCount
        self.totalViewerCount = audienceCount
        self.likeCount = likeCount
        liveDuration = 0
        self.isHost = isHost
        self.roomName = roomName
        self.hostFaceUrl = hostFaceUrl
        super.init(frame: frame)
        backgroundColor = UIColor.white.withAlphaComponent(0.2)
        layer.cornerRadius = frame.size.height / 2
        layer.masksToBounds = true
        initUI()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    func initUI() {
        hostImage.layer.cornerRadius = 16
        hostImage.layer.masksToBounds = true
        let url = URL(string: TCUtil.transImageURL2HttpsURL(hostFaceUrl) ?? "")
        hostImage.kf.setImage(with: url, placeholder: UIImage(named: "default_user", in: liveRoomBundle(), compatibleWith: nil), options:
         nil, progressBlock: nil, completionHandler: nil)
        hostImage.isUserInteractionEnabled = true
        let tapGesture = UITapGestureRecognizer(target: self, action: #selector(headTap(_:)))
        addSubview(hostImage)
        hostImage.addGestureRecognizer(tapGesture)
       
        if isHost {
            durationImage = UIImageView()
            durationImage?.image = UIImage(named: "dot", in: liveRoomBundle(), compatibleWith: nil)
            addSubview(durationImage ?? UIImageView())
            
            durationLabel.text = "00:00:00"
        } else {
            durationLabel.text = roomName
        }
        durationLabel.font = UIFont.boldSystemFont(ofSize: 10)
        durationLabel.textColor = UIColor.white
        addSubview(durationLabel)
        audienceLabel.text = String(format: "%ld", audienceCount)
        audienceLabel.font = UIFont.boldSystemFont(ofSize: 10)
        audienceLabel.textColor = UIColor.white
        addSubview(audienceLabel)
        hostImage.size(with: CGSize(width: 32, height: 32))
        hostImage.center = CGPoint(x:16 , y:self.height/2)
        hostImage.left = 8
        roomIdLabel.text = liveRoomLocalize("Demo.TRTC.LiveRoom.roomId")
        roomIdLabel.font = UIFont.boldSystemFont(ofSize: 10)
        roomIdLabel.textColor = UIColor.white
        roomIdLabel.adjustsFontSizeToFitWidth = true
        addSubview(roomIdLabel)
        if isHost {
            durationImage!.size(with: CGSize(width: 5, height: 5))
            durationImage!.alignParentTop(withMargin: 12.5)
            durationImage!.layout(toRightOf: hostImage, margin: 5)
            durationLabel.size(with: CGSize(width: 48, height: 10))
            durationLabel.alignParentTop(withMargin: 10)
            durationLabel.layout(toRightOf: durationImage, margin: 2.5)
        } else {
            durationLabel.size(with: CGSize(width: 110, height: 10))
            durationLabel.alignParentTop(withMargin: 12)
            if let durationImage = durationImage {
                durationLabel.layout(toRightOf: durationImage, margin:2.5)
            } else {
                durationLabel.layout(toRightOf: hostImage, margin: 8)
            }
        }
        roomIdLabel.size(with: CGSize(width: 110, height: 10))
        roomIdLabel.alignParentBottom(withMargin: 10)
        roomIdLabel.layout(toRightOf: hostImage, margin: 8)
    }
    
    @objc func headTap(_ tap: UITapGestureRecognizer?) {
        if let clickHead = clickHead {
            clickHead()
        }
    }
    
    func setViewerCount( _viewerCount: Int, _likeCount: Int) {
        audienceCount = _viewerCount
        totalViewerCount = _viewerCount
        likeCount = _likeCount
        audienceLabel.text = "\(audienceCount)"
    }
    
    func startLive() {
        if isHost {
            startTime = Int(Date().timeIntervalSince1970)
            
            if timer != nil {
                timer?.invalidate()
            }
            timer = Timer.scheduledTimer(timeInterval: 1, target: self, selector: #selector(onLiveTimer), userInfo: nil, repeats: true)
            guard let timer = timer else { return }
            RunLoop.current.add(timer, forMode: .common)
        }
    }
    
    func pauseLive() {
        if timer != nil {
            timer?.invalidate()
            timer = nil
        }
    }
    
    func resumeLive() {
        startLive()
    }
    
    func getViewerCount() -> Int {
        return audienceCount
    }
    
    func getLikeCount() -> Int {
        return likeCount
    }
    
    func getTotalViewerCount() -> Int {
        return totalViewerCount
    }
    
    func getLiveDuration() -> Int {
        return liveDuration
    }
    
    @objc func onLiveTimer() {
        let curTime = Int(Date().timeIntervalSince1970)
        let dur = curTime - startTime
        
        var durStr: String? = nil
        let h = dur / 3600
        let m = (dur - h * 3600) / 60
        let s = dur % 60
        durStr = String(format: "%02d:%02d:%02d", h, m, s)
        
        liveDuration = dur
        durationLabel.text = durStr
    }
    
    func onUserEnterLiveRoom() {
        audienceCount += 1
        totalViewerCount += 1
        audienceLabel.text = String(format: "%ld", audienceCount)
    }
    
    func onUserExitLiveRoom() {
        if audienceCount > 0 {
            audienceCount -= 1
        }
        audienceLabel.text = String(format: "%ld", audienceCount)
    }
    
    func onUserSendLikeMessage() {
        likeCount += 1
    }
    
    func setRoomId(_ roomId: String?) {
        
        roomIdLabel.text = liveRoomLocalize("Demo.TRTC.LiveRoom.roomId").appending(roomId ?? "")
    }
    
}

class TCAudienceListCell: UITableViewCell {
    var audienceimageView: UIImageView
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        audienceimageView = UIImageView(frame: CGRect.zero)
        audienceimageView.transform = CGAffineTransform(rotationAngle: .pi / 2)
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        contentView.addSubview(audienceimageView)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    override func layoutSubviews() {
        audienceimageView.frame = CGRect(x: 0, y: 0, width: IMAGE_SIZE, height: IMAGE_SIZE)
        audienceimageView.layer.cornerRadius = audienceimageView.size.width / 2
        audienceimageView.clipsToBounds = true
    }
    
    func refresh(withModel msgModel: TRTCLiveUserInfo) {
        audienceimageView.kf.setImage(with:  URL(string: TCUtil.transImageURL2HttpsURL(msgModel.avatarURL) ?? ""), placeholder: UIImage(named: "face", in: liveRoomBundle(), compatibleWith: nil), options: nil, progressBlock: nil, completionHandler: nil)
    }
}


