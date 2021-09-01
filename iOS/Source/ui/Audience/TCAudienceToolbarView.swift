//
//  TCAudienceToolbarView.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/23.
//

import Foundation
protocol TCAudienceToolbarDelegate: NSObjectProtocol {
    func closeVC(_ popViewController: Bool)
    func clickScreen(_ position: CGPoint)
    func clickPlayVod()
    func clickLog()
    func onSeek(_ slider: UISlider?)
    func onSeekBegin(_ slider: UISlider?)
    func onDrag(_ slider: UISlider?)
    func onRecvGroupDeleteMsg()
}

///  播放模块逻辑view，里面展示了消息列表，弹幕动画，观众列表等UI，其中与SDK的逻辑交互需要交给主控制器处理
public class TCAudienceToolbarView: UIView, TCAudienceListDelegate, UITextFieldDelegate ,UIAlertViewDelegate{
    weak var delegate: TCAudienceToolbarDelegate?
    weak var liveRoom: TRTCLiveRoom?
    var playDuration: UILabel = UILabel()
    var playProgress: UISlider = UISlider()
    var playLabel: UILabel = UILabel()
    var playBtn: UIButton = UIButton(type: .custom)
    var closeBtn: UIButton = UIButton(type: .custom)
    var btnChat: UIButton = UIButton(type: .custom)
    var cover: UIView = UIButton(type: .custom)
    var statusView: UITextView?
    var logViewEvt: UITextView?
    var audienceTableView: TCAudienceListTableView?
    var msgTableView: TCMsgListTableView?
    var bulletViewOne: TCMsgBarrageView?
    var bulletViewTwo: TCMsgBarrageView?
    var liveInfo: TRTCLiveRoomInfo?
    var likeBtn: UIButton = UIButton(type: .custom)
    var msgInputView: UIView = UIView()
    var msgInputFeild: UITextField = UITextField()
    var touchBeginLocation = CGPoint.zero
    var bulletBtnIsOn = false
    var viewsHidden = false
    
    lazy var topView: TCShowLiveTopView  = {
        return TCShowLiveTopView.init(frame: CGRect(x: 5, y: Int(StatusBarHeight) + 5, width: 180, height: 48), isHost: false, hostNickName: liveInfo?.ownerName ?? "", audienceCount: 0, likeCount: 0, hostFaceUrl: liveInfo?.coverUrl ?? "")
    }()
    lazy var frequeControl :TCFrequeControl = {
        return TCFrequeControl(counts: 10, andSeconds: 1)
    }()
    
    init(frame: CGRect, live liveInfo: TRTCLiveRoomInfo?, withLinkMic linkmic: Bool) {
        super.init(frame: frame)
        self.liveInfo = liveInfo
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardFrameDidChange(_:)), name: UIResponder.keyboardWillChangeFrameNotification, object: nil)
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
        closeBtn.setBackgroundImage(UIImage(named: "live_exit", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
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
        let startSpace: CGFloat = 15
        let iconCenterY = CGFloat(height - iconSize / 2) - startSpace
        
        let iconCount: CGFloat = linkmic == true ? 7 : 6
        let iconCenterInterval = (width - 2 * startSpace - CGFloat(iconSize)) / (iconCount - 1)
        let firstIconCenterX = startSpace + CGFloat(iconSize / 2)
        
        btnChat.center = CGPoint(x: firstIconCenterX + iconSize / 2.0, y: iconCenterY)
        btnChat.bounds = CGRect(x: 0, y: 0, width: iconSize, height: iconSize)
        btnChat.setBackgroundImage(UIImage(named: "comment", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        btnChat.addTarget(self, action: #selector(clickChat(_:)), for: .touchUpInside)
        addSubview(btnChat)
        closeBtn.snp.makeConstraints {  make in
            make.centerY.equalTo(btnChat.snp.centerY)
            make.right.equalTo(self).offset(-iconCenterInterval * 0.7)
            make.width.height.equalTo(iconSize)
        }
        //点赞
        likeBtn.frame = CGRect(x: 0, y: 0, width: iconSize, height: iconSize)
        likeBtn.setImage(UIImage(named: "like_hover", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        likeBtn.addTarget(self, action: #selector(clickLike(_:)), for: .touchUpInside)
        addSubview(likeBtn)
        likeBtn.snp.makeConstraints({ make in
            make.centerY.equalTo(closeBtn.snp.centerY)
            make.centerX.equalTo(closeBtn).offset(-iconCenterInterval * 1.2)
            make.width.height.equalTo(iconSize)
        })
        //弹幕
        msgTableView = TCMsgListTableView(frame: CGRect(x: 15, y: Int(btnChat.top) - MSG_TABLEVIEW_HEIGHT - MSG_TABLEVIEW_BOTTOM_SPACE, width: MSG_TABLEVIEW_WIDTH, height: MSG_TABLEVIEW_HEIGHT), style: .grouped)
        guard let msgTableView = msgTableView else { return }
        addSubview(msgTableView)
        //弹幕
        msgTableView.snp.makeConstraints({ make in
            make.width.equalTo(MSG_TABLEVIEW_WIDTH)
            make.height.equalTo(MSG_TABLEVIEW_HEIGHT)
            make.leading.equalTo(self).offset(15)
            make.bottom.equalTo(btnChat.top).offset(-MSG_TABLEVIEW_BOTTOM_SPACE-35)
            
        })
        
        bulletViewOne = TCMsgBarrageView(frame: CGRect(x: 0, y: Int(msgTableView.top) - MSG_UI_SPACE - MSG_BULLETVIEW_HEIGHT, width: Int(SCREEN_WIDTH), height: MSG_BULLETVIEW_HEIGHT))
        guard let bulletViewOne = bulletViewOne else { return }
        insertSubview(bulletViewOne, belowSubview: closeBtn)
        bulletViewOne.snp.makeConstraints({ make in
            make.leading.equalTo(self)
            make.bottom.equalTo(msgTableView.top).offset(-MSG_UI_SPACE)
            make.width.equalTo(SCREEN_WIDTH)
            make.height.equalTo(MSG_BULLETVIEW_HEIGHT)
        })
        bulletViewTwo = TCMsgBarrageView(frame: CGRect(x: 0, y: bulletViewOne.top - CGFloat( MSG_BULLETVIEW_HEIGHT), width: SCREEN_WIDTH, height: CGFloat(MSG_BULLETVIEW_HEIGHT)))
        guard let bulletViewTwo = bulletViewTwo else { return }
        insertSubview(bulletViewTwo, belowSubview: closeBtn)
        bulletViewTwo.snp.makeConstraints({ make in
            make.leading.equalTo(self)
            make.bottom.equalTo(msgTableView.top)
            make.width.equalTo(SCREEN_WIDTH)
            make.height.equalTo(MSG_BULLETVIEW_HEIGHT)
        })
        
        //输入框
        let InputViewFrame = CGRect(x: 0, y: height, width: width, height: CGFloat(MSG_TEXT_SEND_VIEW_HEIGHT))
        msgInputView.frame = InputViewFrame
        msgInputView.backgroundColor = UIColor.clear
        let imageView = UIImageView(frame: CGRect(x: 0, y: 0, width: msgInputView.width, height: msgInputView.height))
        imageView.image = UIImage(named: "input_comment", in: LiveRoomBundle(), compatibleWith: nil)
        
        let bulletBtn = UIButton(type: .custom)
        bulletBtn.frame = CGRect(x: 10, y: (Int(msgInputView.height) - MSG_TEXT_SEND_FEILD_HEIGHT) / 2, width: MSG_TEXT_SEND_BULLET_BTN_WIDTH, height: MSG_TEXT_SEND_FEILD_HEIGHT)
        bulletBtn.setImage(UIImage(named: "Switch_OFF", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        bulletBtn.setImage(UIImage(named: "Switch_ON", in: LiveRoomBundle(), compatibleWith: nil), for: .selected)
        bulletBtn.addTarget(self, action: #selector(clickBullet(_:)), for: .touchUpInside)
        
        let sendBtn = UIButton(type: .custom)
        sendBtn.frame = CGRect(x: Int(width) - 15 - MSG_TEXT_SEND_BTN_WIDTH, y: (Int(msgInputView.height) - MSG_TEXT_SEND_FEILD_HEIGHT) / 2, width: MSG_TEXT_SEND_BTN_WIDTH, height: MSG_TEXT_SEND_FEILD_HEIGHT)
        sendBtn.setTitle(LiveRoomLocalize("Demo.TRTC.LiveRoom.send"), for: .normal)
        sendBtn.titleLabel?.font = UIFont.systemFont(ofSize: 16)
        sendBtn.setTitleColor(UIColor(hex: "0x0ACCAC"), for: .normal)
        sendBtn.backgroundColor = UIColor.clear
        sendBtn.addTarget(self, action: #selector(clickSend), for: .touchUpInside)
        
        let msgInputFeildLine1 = UIImageView(image: UIImage(named: "vertical_line", in: LiveRoomBundle(), compatibleWith: nil))
        msgInputFeildLine1.frame = CGRect(x: bulletBtn.right + 10, y: sendBtn.y, width: 1, height: CGFloat(MSG_TEXT_SEND_FEILD_HEIGHT))
        let msgInputFeildLine2 = UIImageView(image: UIImage(named: "vertical_line", in: LiveRoomBundle(), compatibleWith: nil))
        msgInputFeild.frame = CGRect(x: msgInputFeildLine1.right + 100, y: sendBtn.y, width: msgInputFeildLine2.left - msgInputFeildLine1.right - 20, height: CGFloat(MSG_TEXT_SEND_FEILD_HEIGHT))
        msgInputFeild.backgroundColor = UIColor.clear
        msgInputFeild.returnKeyType = .send
        msgInputFeild.placeholder = LiveRoomLocalize("Demo.TRTC.LiveRoom.saysomething")
        msgInputFeild.delegate = self
        msgInputFeild.textColor = UIColor.black
        msgInputFeild.font = UIFont.systemFont(ofSize: 14)
        msgInputView.addSubview(msgInputFeildLine1)
        msgInputView.addSubview(msgInputFeildLine2)
        msgInputView.addSubview(imageView)
        msgInputView.addSubview(msgInputFeild)
        msgInputView.addSubview(bulletBtn)
        msgInputView.addSubview(sendBtn)
        
        msgInputView.isHidden = true
        addSubview(msgInputView)
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
        
        logViewEvt = UITextView(frame: CGRect(x: 10.0, y: 55 + 2 * iconSize + CGFloat(logheadH), width: width - 20, height: height - 110 - 3 * iconSize - CGFloat(logheadH)))
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
    
    func bulletMsg(_ msgModel: TCMsgModel?) {
        msgTableView?.bulletNewMsg(msgModel)
        if msgModel?.msgType == .danmaMsg {
            if getLocation(bulletViewOne!) >= getLocation(bulletViewTwo!) {
                bulletViewTwo?.top = CGFloat(Int(msgTableView!.top) - MSG_UI_SPACE )
                bulletViewTwo?.bulletNewMsg(msgModel)
            } else {
                bulletViewOne?.top = CGFloat(Int(msgTableView!.top) - MSG_UI_SPACE - MSG_BULLETVIEW_HEIGHT)
                bulletViewOne?.bulletNewMsg(msgModel)
            }
        }
        if msgModel?.msgType == .memberEnterRoom || msgModel?.msgType == .memberQuitRoom {
            audienceTableView?.refreshAudienceList(msgModel)
        }
    }
    
    @objc func clickBullet(_ btn: UIButton?) {
        bulletBtnIsOn = !bulletBtnIsOn
        btn?.isSelected = bulletBtnIsOn
    }
    
    @objc func clickChat(_ button: UIButton?) {
        self.msgInputFeild.becomeFirstResponder()
    }
    
    @objc func clickSend() {
        
        _ = textFieldShouldReturn(msgInputFeild )
    }
    
    @objc func clickLike(_ button: UIButton) {
        liveRoom?.sendRoomCustomMsg(cmd: "4", message: "", callback: { code, error in
        })
        topView.onUserSendLikeMessage()
        showLikeHeartStart(button.frame)
    }
    
    func showLikeHeart() {
        showLikeHeartStart(likeBtn.frame)
    }
    
    func showLikeHeartStart(_ frame: CGRect) {
        guard frequeControl.canTrigger() else {
            return
        }
        if viewsHidden {
            return
        }
    }
    // MARK: UITextFieldDelegate
    public func textFieldShouldBeginEditing(_ textField: UITextField) -> Bool {
        msgInputFeild.text = ""
        return true
    }
    
    public func textFieldDidEndEditing(_ textField: UITextField) {
        msgInputFeild.text = textField.text
    }
    
    public func textFieldShouldReturn(_ textField: UITextField) -> Bool {
        let textMsg = textField.text?.trimmingCharacters(in: CharacterSet.whitespaces)
        if textMsg?.count ?? 0 <= 0 {
            textField.text = ""
            HUDHelper.alert(msg: LiveRoomLocalize("Demo.TRTC.LiveRoom.messagecantbeempty"))
            return true
        }
        var msgModel = TCMsgModel()
        msgModel.userName = LiveRoomLocalize("Demo.TRTC.LiveRoom.me")
        msgModel.userMsg = textMsg
        msgModel.userHeadImageUrl = TUILiveRoomProfileManager.sharedManager().avatar
        if bulletBtnIsOn {
            msgModel.msgType = .danmaMsg
            let cmd :String = "5"//弹幕
            guard let liveRoom = liveRoom else { return true }
            liveRoom.sendRoomCustomMsg(cmd: cmd, message: textMsg ?? "", callback: { code, error in
                
            })
        } else {
            msgModel.msgType = .normal
            liveRoom?.sendRoomTextMsg(message: textMsg ?? "", callback: { code, error in })
        }
        bulletMsg(msgModel)
        msgInputFeild.resignFirstResponder()
        return true
    }
    
    @objc func keyboardFrameDidChange(_ notice: Notification?) {
        let userInfo = notice?.userInfo
        let endFrameValue = userInfo?[UIResponder.keyboardFrameEndUserInfoKey] as? NSValue
        let endFrame = endFrameValue?.cgRectValue
        let shouldHidden = (endFrame?.minY ?? 0.0) >= UIScreen.main.bounds.size.height
        if !shouldHidden {
            msgInputView.isHidden = false
        }
        guard var endFrametem = endFrame else {
            return
        }
        endFrametem = msgInputView.superview!.convert(endFrametem, from: nil)
        UIView.animate(withDuration: 0.25) {
            [weak self] in
            guard let `self` = self else { return }
            if endFrametem.origin.y >= self.height {
                self.msgInputView.y = endFrametem.origin.y
            } else {
                self.msgInputView.y = endFrametem.origin.y - self.msgInputView.height
                
            }
        } completion: { finish in
            if shouldHidden {
                self.msgInputView.isHidden = true
            }
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
        bulletViewOne?.stopAnimation()
        bulletViewTwo?.stopAnimation()
        NotificationCenter.default.removeObserver(self)
        delegate.closeVC(true)
    }
    
    @objc func clickScreenTap(_ gestureRecognizer: UITapGestureRecognizer?) {
        msgInputFeild.resignFirstResponder()
        guard let delegate = delegate else {
            return
        }
        let position = gestureRecognizer?.location(in: self)
        delegate.clickScreen(position!)
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
        case .normal:
            var msgModel = TCMsgModel()
            msgModel.userName = info.imUserName
            msgModel.userMsg = msgText
            msgModel.userHeadImageUrl = info.imUserIconUrl
            msgModel.msgType = .normal
            bulletMsg(msgModel)
            break
        case .memberEnterRoom:
            var msgModel = TCMsgModel()
            msgModel.userId = info.imUserId
            msgModel.userName = info.imUserName
            msgModel.userMsg = LiveRoomLocalize("Demo.TRTC.LiveRoom.joininteraction")
            msgModel.userHeadImageUrl = info.imUserIconUrl
            msgModel.msgType = .memberEnterRoom
            //收到新增观众消息，判断只有没在观众列表中，数量才需要增加1
            if !isAlready(inAudienceList: msgModel) {
                topView.onUserEnterLiveRoom()
            }
            bulletMsg(msgModel)
            break
        case .memberQuitRoom:
            var msgModel = TCMsgModel()
            msgModel.userId = info.imUserId
            msgModel.userName = info.imUserName
            msgModel.userMsg = LiveRoomLocalize("Demo.TRTC.LiveRoom.exitinteraction")
            msgModel.userHeadImageUrl = info.imUserIconUrl
            msgModel.msgType = .memberQuitRoom
            bulletMsg(msgModel)
            topView.onUserExitLiveRoom()
            
            break
        case .praise:
            var msgModel = TCMsgModel()
            msgModel.userName = info.imUserName
            msgModel.userMsg = LiveRoomLocalize("Demo.TRTC.LiveRoom.clicklike")
            msgModel.userHeadImageUrl = info.imUserIconUrl
            msgModel.msgType = .praise
            bulletMsg(msgModel)
            showLikeHeart()
            topView.onUserSendLikeMessage()
            break
        case .danmaMsg:
            var msgModel = TCMsgModel()
            msgModel.userName = info.imUserName
            msgModel.userMsg = msgText
            msgModel.userHeadImageUrl = info.imUserIconUrl
            msgModel.msgType = .danmaMsg
            
            bulletMsg(msgModel)
            
            break
        default:
            break
        }
    }
    
    // MARK: - 滑动隐藏界面UI
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
