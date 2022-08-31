//
//  TCAudienceToolbarView.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/23.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation
protocol TCAudienceToolbarDelegate: NSObjectProtocol {
    func closeVC(_ popViewController: Bool)
    func clickScreen(_ position: CGPoint)
    func clickPlayVod()
    func clickLog()
    func clickLike()
    func clickChat()
    func onSeek(_ slider: UISlider?)
    func onSeekBegin(_ slider: UISlider?)
    func onDrag(_ slider: UISlider?)
    func onRecvGroupDeleteMsg()
}

/// TCAudienceToolbarView
public class TCAudienceToolbarView: UIView, TCAudienceListDelegate, UITextFieldDelegate ,UIAlertViewDelegate{
    weak var delegate: TCAudienceToolbarDelegate?
    weak var liveRoom: TRTCLiveRoom?
    var playDuration: UILabel = UILabel()
    var playProgress: UISlider = UISlider()
    var playLabel: UILabel = UILabel()
    var playBtn: UIButton = UIButton(type: .custom)
    var closeBtn: UIButton = UIButton(type: .custom)
    var btnChat: UIButton = UIButton(type: .custom)
    lazy var reportBtn: UIButton  = {
        let btn = UIButton(type: .custom)
        btn.setBackgroundImage(UIImage(named: "livevideo_report", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        btn.imageView?.contentMode = .scaleAspectFill
        return btn
    }()
    var cover: UIView = UIButton(type: .custom)
    var statusView: UITextView?
    var logViewEvt: UITextView?
    var audienceTableView: TCAudienceListTableView?
    var liveInfo: TRTCLiveRoomInfo?
    var likeBtn: UIButton = UIButton(type: .custom)
    var touchBeginLocation = CGPoint.zero
    var bulletBtnIsOn = false
    var viewsHidden = false
    
    lazy var topView: TCShowLiveTopView  = {
        return TCShowLiveTopView(frame: CGRect(x: 5, y: Int(StatusBarHeight) + 5, width: 180, height: 48),
                                 isHost: false,
                                 roomName: (liveInfo?.roomName ?? ""),
                                 audienceCount: 0,
                                 likeCount: 0,
                                 hostFaceUrl: (liveInfo?.coverUrl ?? ""))
    }()
    
    init(frame: CGRect, live liveInfo: TRTCLiveRoomInfo?, withLinkMic linkmic: Bool) {
        super.init(frame: frame)
        self.liveInfo = liveInfo
        let tap = UITapGestureRecognizer(target: self, action: #selector(clickScreenTap(_:)))
        addGestureRecognizer(tap)
        initUI(linkmic)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        NotificationCenter.default.removeObserver(self)
    }
    
    func setViewerCount(viewCount: Int,like likeCount: Int) -> Void {
        liveInfo?.memberCount = viewCount
        topView.setViewerCount(_viewerCount: viewCount, _likeCount: likeCount)
    }
    
    func isAlready(inAudienceList model: TCMsgModel) -> Bool {
        guard let audienceTableView = audienceTableView else {
            return false
        }
        return audienceTableView.isAlready(inAudienceList: model)
    }
    
    func setRoomId(_ roomId: String?) {
        topView.setRoomId(roomId)
    }
    
    func onFetchGroupMemberList(_ errCode: Int, memberCount: Int) {
        guard 0 == errCode else {
            return
        }
        topView.setViewerCount(_viewerCount: memberCount, _likeCount: Int(topView.getLikeCount()))
    }
    
    func initAudienceList(_ audienceList: [TRTCLiveUserInfo]) {
        let audience_width: CGFloat = width - 25 - topView.right
        let x = topView.right + 10 + (audience_width/2) - CGFloat((IMAGE_SIZE/2))
        let y = topView.center.y - audience_width/2
        let frame = CGRect(x: x, y: y, width: topView.height, height: audience_width)
        guard let liveInfo = liveInfo else {
            return
        }
        audienceTableView = TCAudienceListTableView(frame: frame, style: UITableView.Style.grouped, live: liveInfo)
        
        guard let audienceTableView = audienceTableView else {
            return
        }
        audienceTableView.transform = CGAffineTransform(rotationAngle: -.pi / 2)
        audienceTableView.audienceListDelegate = self
        addSubview(audienceTableView)
        for user in audienceList {
            var msgModel = TCMsgModel()
            msgModel.msgType = .memberEnterRoom
            msgModel.userId = user.userId
            msgModel.userName = user.userName
            msgModel.userHeadImageUrl = user.avatarURL
            audienceTableView.refreshAudienceList(msgModel)
            topView.onUserEnterLiveRoom()
        }
    }
    
    func initUI(_ linkmic: Bool) -> Void {
        closeBtn.setBackgroundImage(UIImage(named: "live_exit", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        closeBtn.addTarget(self, action: #selector(closeVC), for: .touchUpInside)
        addSubview(closeBtn)
        addSubview(topView)
        topView.clickHead = { [weak self] in
            guard let `self` = self else { return }
            guard let delegate = self.delegate else {
                return
            }
            delegate.clickLog()
        }
        let iconSize = CGFloat(BOTTOM_BTN_ICON_WIDTH)
        let startSpace: CGFloat = 10
        let iconCenterY = CGFloat(height - iconSize / 2) - startSpace
        
        let iconCount: CGFloat = linkmic == true ? 7 : 6
        let iconCenterInterval = (width - 2 * startSpace - CGFloat(iconSize)) / (iconCount - 1)
        let firstIconCenterX = startSpace + CGFloat(iconSize / 2)
        
        btnChat.center = CGPoint(x: firstIconCenterX + iconSize / 2.0, y: iconCenterY)
        btnChat.bounds = CGRect(x: 0, y: 0, width: iconSize, height: iconSize)
        btnChat.setBackgroundImage(UIImage(named: "comment", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        btnChat.addTarget(self, action: #selector(clickChat(_:)), for: .touchUpInside)
        addSubview(btnChat)
        closeBtn.snp.makeConstraints {  make in
            make.centerY.equalTo(btnChat.snp.centerY)
            make.right.equalTo(self).offset(-iconCenterInterval * 0.7)
            make.width.height.equalTo(iconSize)
        }

        likeBtn.frame = CGRect(x: 0, y: 0, width: iconSize, height: iconSize)
        likeBtn.setImage(UIImage(named: "like_hover", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        likeBtn.addTarget(self, action: #selector(clickLike(_:)), for: .touchUpInside)
        addSubview(likeBtn)
        likeBtn.snp.makeConstraints({ make in
            make.centerY.equalTo(closeBtn.snp.centerY)
            make.centerX.equalTo(closeBtn).offset(-iconCenterInterval * 1.2)
            make.width.height.equalTo(iconSize)
        })
#if RTCube_APPSTORE
        addSubview(reportBtn)
        reportBtn.snp.makeConstraints({ make in
            make.centerY.equalTo(btnChat.snp.centerY)
            make.centerX.equalTo(btnChat).offset(iconCenterInterval * 1.2)
            make.width.height.equalTo(iconSize)
        })
        reportBtn.addTarget(self, action: #selector(clickReport), for: .touchUpInside)
#endif
        //LOG UI
        cover.frame = CGRect(x: 10.0, y: 55 + 2 * iconSize, width: width - 20, height: height - 110 - 3 * iconSize)
        cover.backgroundColor = UIColor.white
        cover.alpha = 0.5
        cover.isHidden = true
        addSubview(cover)
        
        let logheadH = 65
        statusView = UITextView(frame: CGRect(x: 10.0, y: 55 + 2 * iconSize, width: width - 20, height: CGFloat(logheadH)))
        guard let statusView = statusView else {
            return
        }
        statusView.backgroundColor = UIColor.clear
        statusView.alpha = 1
        statusView.textColor = UIColor.black
        statusView.isEditable = false
        statusView.isHidden = true
        addSubview(statusView)
        
        logViewEvt = UITextView(frame: CGRect(x: 10.0, y: 55 + 2 * iconSize + CGFloat(logheadH), width:
         width - 20, height: height - 110 - 3 * iconSize - CGFloat(logheadH)))
        guard let logViewEvt = logViewEvt else {
            return
        }
        logViewEvt.backgroundColor = UIColor.clear
        logViewEvt.alpha = 1
        logViewEvt.textColor = UIColor.black
        logViewEvt.isEditable = false
        logViewEvt.isHidden = true
        addSubview(logViewEvt)
    }
    
    func getLocation(_ bulletView: TCMsgBarrageView) -> CGFloat {
        let view = bulletView.lastAnimateView
        let rect = view.layer.presentation()?.frame
        return (rect?.origin.x ?? 0.0) + (rect?.size.width ?? 0.0)
    }
    
    
    @objc func clickChat(_ button: UIButton?) {
        delegate?.clickChat()
    }
    
    @objc func clickLike(_ button: UIButton) {
        delegate?.clickLike()
    }
    
    @objc func clickReport() {
        let selector = NSSelectorFromString("showReportAlertWithRoomId:ownerId:")
        if responds(to: selector) {
            guard let liveInfo = liveInfo else { return }
            perform(selector, with: liveInfo.roomId, with: liveInfo.ownerId)
        }
    }
    
    func onLogout(_ notice: Notification?) {
        closeVC()
    }
    
    // MARK: TCAudienceToolbarDelegate
    @objc func closeVC() {
        guard let delegate = delegate else {
            return
        }
        NotificationCenter.default.removeObserver(self)
        delegate.closeVC(true)
    }
    
    @objc func clickScreenTap(_ gestureRecognizer: UITapGestureRecognizer?) {
        guard let delegate = delegate else {
            return
        }
        if let position = gestureRecognizer?.location(in: self) {
            delegate.clickScreen(position)
        }
    }
    
    @objc func clickPlayVod() {
        guard let delegate = delegate else {
            return
        }
        if delegate.responds(to: #selector(clickPlayVod)) {
            delegate.clickPlayVod()
        }
    }
    
    @objc func onSeek(_ slider: UISlider?) {
        guard let delegate = delegate else {
            return
        }
        if delegate.responds(to: #selector(onSeek(_:))) {
            delegate.onSeek(slider)
        }
    }
    
    @objc func onSeekBegin(_ slider: UISlider?) {
        guard let delegate = delegate else {
            return
        }
        if delegate.responds(to: #selector(onSeekBegin(_:))) {
            delegate.onSeekBegin(slider)
        }
    }
    
    @objc func onDrag(_ slider: UISlider?) {
        guard let delegate = delegate else {
            return
        }
        if delegate.responds(to: #selector(onDrag(_:))) {
            delegate.onDrag(slider)
        }
    }
    
    func handleIMMessage(_ info: IMUserAble?, msgText: String?) {
        guard let info = info else {
            return
        }
        switch info.cmdType {
        case .memberEnterRoom:
            var msgModel = TCMsgModel()
            msgModel.userId = info.imUserId
            msgModel.userName = info.imUserName
            msgModel.userMsg = liveRoomLocalize("Demo.TRTC.LiveRoom.joininteraction")
            msgModel.userHeadImageUrl = info.imUserIconUrl
            msgModel.msgType = .memberEnterRoom
            if !isAlready(inAudienceList: msgModel) {
                topView.onUserEnterLiveRoom()
            }
            break
        case .memberQuitRoom:
            var msgModel = TCMsgModel()
            msgModel.userId = info.imUserId
            msgModel.userName = info.imUserName
            msgModel.userMsg = liveRoomLocalize("Demo.TRTC.LiveRoom.exitinteraction")
            msgModel.userHeadImageUrl = info.imUserIconUrl
            msgModel.msgType = .memberQuitRoom
            topView.onUserExitLiveRoom()
            break
        case .praise:
            var msgModel = TCMsgModel()
            msgModel.userName = info.imUserName
            msgModel.userMsg = liveRoomLocalize("Demo.TRTC.LiveRoom.clicklike")
            msgModel.userHeadImageUrl = info.imUserIconUrl
            msgModel.msgType = .praise
            topView.onUserSendLikeMessage()
            break
        case .danmaMsg:
            var msgModel = TCMsgModel()
            msgModel.userName = info.imUserName
            msgModel.userMsg = msgText
            msgModel.userHeadImageUrl = info.imUserIconUrl
            msgModel.msgType = .danmaMsg
            break
        default:
            break
        }
    }
    
    func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent) {
        let touch = event.allTouches?.first
        touchBeginLocation = (touch?.location(in: self))!
    }
    
    func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent) {
        let touch = event.allTouches?.first
        let location = touch?.location(in: self)
        endMove((location?.x ?? 0.0) - touchBeginLocation.x)
    }
    
    func resetViewAlpha(_ view: UIView) {
        let rect = view.frame
        if (rect.origin.x ) >= SCREEN_WIDTH || (rect.origin.x ) < 0 {
            view.alpha = 0
            viewsHidden = true
        } else {
            view.alpha = 1
            viewsHidden = false
        }
        if view == cover {
            cover.alpha = 0.5
        }
    }
    
    func endMove(_ moveX: CGFloat) {
        UIView.animate(withDuration: 0.2, animations: { [weak self] in
            guard let `self` = self else { return }
            if moveX > 10 {
                for view in self.subviews {
                    if view != self.closeBtn {
                        var rect = view.frame
                        if rect.origin.x >= 0 && rect.origin.x < SCREEN_WIDTH {
                            rect = rect.offsetBy(dx: self.width, dy: 0)
                            view.frame = rect
                            self.resetViewAlpha(view)
                        }
                    }
                }
            } else if moveX < -10 {
                for view in self.subviews {
                    if view != self.closeBtn {
                        var rect = view.frame
                        if rect.origin.x >= SCREEN_WIDTH {
                            rect = rect.offsetBy(dx: -self.width, dy: 0)
                            view.frame = rect
                            self.resetViewAlpha(view)
                        }
                    }
                }
            }
        })
    }
}
