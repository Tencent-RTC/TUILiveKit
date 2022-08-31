//
//  TCAnchorToolbarView.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/28.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation

typealias ShowResultComplete = () -> Void

protocol TCAnchorToolbarDelegate: NSObjectProtocol {
    func closeRTMP()
    func closeVC()
    func clickScreen(_ gestureRecognizer: UITapGestureRecognizer?)
    func clickCamera(_ button: UIButton?)
    func clickBeauty(_ button: UIButton?)
    func clickMusic(_ button: UIButton?)
    func clickChat(_ button: UIButton?)
    func clickPK(_ button: UIButton?)
    func pk(withRoom room: TRTCLiveRoomInfo?)
    func clickLog()
}

class TCPushShowResultView: UIView {
    
    var titleLabel: UILabel = UILabel()
    var durationLabel: UILabel = UILabel()
    var durationTipLabel: UILabel = UILabel()
    var viewerCountLabel: UILabel = UILabel()
    var viewerCountTipLabel: UILabel = UILabel()
    var praiseLabel: UILabel = UILabel()
    var praiseTipLabel: UILabel = UILabel()
    var backBtn: UIButton = UIButton(type: .custom)
    var line: UILabel = UILabel()
    var backHomepage: ShowResultComplete?
    var resultData: TCShowLiveTopView?
    
    init(frame: CGRect, resultData: TCShowLiveTopView?, backHomepage: @escaping ShowResultComplete) {
        super.init(frame: frame)
        self.resultData = resultData
        self.backHomepage = backHomepage
        initUI()
        backBtn.addTarget(self, action: #selector(clickBackBtn), for: .touchUpInside)
    }
    
    @objc func clickBackBtn()  {
        self.backHomepage?()
    }
    
    required init?(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)
    }
    
    func initUI() {
        let duration = Int(resultData?.getLiveDuration() ?? 0)
        let hour = duration / 3600
        let min = (duration - hour * 3600) / 60
        let sec = duration - hour * 3600 - min * 60
        titleLabel.textAlignment = .center
        titleLabel.font = UIFont.boldSystemFont(ofSize: 15)
        titleLabel.textColor = UIColor.black
        titleLabel.text = liveRoomLocalize("Demo.TRTC.LiveRoom.interactionitsover")
        addSubview(titleLabel)
        durationLabel.textAlignment = .center
        durationLabel.font = UIFont.boldSystemFont(ofSize: 15)
        durationLabel.textColor = UIColor.black
        durationLabel.text = String(format: "%02d:%02d:%02d", hour, min, sec)
        addSubview(durationLabel)
        durationTipLabel.textAlignment = .center
        durationTipLabel.font = UIFont.boldSystemFont(ofSize: 12)
        durationTipLabel.textColor = UIColor.gray
        durationTipLabel.text = liveRoomLocalize("Demo.TRTC.LiveRoom.interactionduration")
        addSubview(durationTipLabel)
        viewerCountLabel.textAlignment = .center
        viewerCountLabel.font = UIFont.boldSystemFont(ofSize: 12)
        viewerCountLabel.textColor = UIColor.black
        guard let resultData = resultData else { return }
        viewerCountLabel.text = String(format: "%ld", resultData.getTotalViewerCount())
        addSubview(viewerCountLabel)
        viewerCountTipLabel.textAlignment = .center
        viewerCountTipLabel.font = UIFont.boldSystemFont(ofSize: 12)
        viewerCountTipLabel.textColor = UIColor.gray
        viewerCountTipLabel.text = liveRoomLocalize("Demo.TRTC.LiveRoom.viewers")
        addSubview(viewerCountTipLabel)
        praiseLabel.textAlignment = .center
        praiseLabel.font = UIFont.boldSystemFont(ofSize: 12)
        praiseLabel.textColor = UIColor.black
        praiseLabel.text = String(format: "%ld\n", resultData.getLikeCount())
        addSubview(praiseLabel)
        praiseTipLabel.textAlignment = .center
        praiseTipLabel.font = UIFont.boldSystemFont(ofSize: 12)
        praiseTipLabel.textColor = UIColor.gray
        praiseTipLabel.text = liveRoomLocalize("Demo.TRTC.LiveRoom.numberoflikes")
        addSubview(praiseTipLabel)
        line.backgroundColor = UIColor(hex: "EEEEEE")
        addSubview(line)
        backBtn.backgroundColor = UIColor.clear
        backBtn.titleLabel?.font = UIFont.systemFont(ofSize: 16)
        backBtn.setTitle(liveRoomLocalize("Demo.TRTC.LiveRoom.back"), for: .normal)
        backBtn.setTitleColor(UIColor.flatBlue(), for: .normal)
        addSubview(backBtn)
        relayout()
    }
    
    func relayout() {
        let rect = bounds
        titleLabel.size(with: CGSize(width: rect.size.width, height: 24))
        titleLabel.alignParentTop(withMargin: 20)
        
        durationLabel.size(with: CGSize(width: rect.size.width, height: 15))
        durationLabel.layout(below: titleLabel, margin: 20)
        durationTipLabel.size(with: CGSize(width: rect.size.width, height: 14))
        durationTipLabel.layout(below: durationLabel, margin: 7)
        
        viewerCountLabel.frame = CGRect(x: rect.size.width / 4.0 - 10, y: 0, width: rect.size.width / 4.0, height: 15)
        viewerCountLabel.layout(below: durationTipLabel, margin: 20)
        viewerCountTipLabel.frame = CGRect(x: rect.size.width / 5.5, y: 0, width: rect.size.width / 3.5, height: 15)
        viewerCountTipLabel.layout(below: viewerCountLabel, margin: 7)
        
        praiseLabel.frame = CGRect(x: rect.size.width / 2.0 + 10, y: 0, width: rect.size.width / 4.0, height: 15)
        praiseLabel.layout(below: durationTipLabel, margin: 20)
        praiseTipLabel.frame = CGRect(x: rect.size.width / 1.88, y: 0, width: rect.size.width / 3.5, height: 15)
        praiseTipLabel.layout(below: praiseLabel, margin: 7)
        
        backBtn.size(with: CGSize(width: rect.size.width, height: 35))
        backBtn.layoutParentHorizontalCenter()
        backBtn.layout(below: praiseTipLabel, margin: 30)
        
        line.size(with: CGSize(width: rect.size.width, height: 0.5))
        line.layout(below: praiseTipLabel, margin: 30)
        
        backgroundColor = UIColor.white
        layer.cornerRadius = 10
        clipsToBounds = true
    }
}

class TCAnchorToolbarView: UIView, UITextFieldDelegate, UIGestureRecognizerDelegate {
    var btnChat: UIButton  = UIButton(type: .custom)
    var btnCamera: UIButton = UIButton(type: .custom)
    var btnBeauty: UIButton = UIButton(type: .custom)
    var btnPK: UIButton = UIButton(type: .custom)
    var btnMusic: UIButton = UIButton(type: .custom)
    var setting : UIButton = UIButton(type: .custom)
    
    var cover: UIView = UIView()
    var vPKPanel: AnchorPKPanel = AnchorPKPanel()
    var isPreview = false
    weak var delegate: TCAnchorToolbarDelegate?
    weak var liveRoom: TRTCLiveRoom?
    weak var anchorViewController: TCAnchorViewController?
    var showLikeHeartStartRectFreqControl: TCFrequeControl?
    var resultView: TCPushShowResultView?
    var audienceTableView: TCAudienceListTableView?
    var liveInfo: TRTCLiveRoomInfo?
    var clearView: UIView?
    var closeBtn: UIButton = UIButton(type: .custom)
    var pkView: UIImageView?
    var touchBeginLocation = CGPoint.zero
    var bulletBtnIsOn = false
    var closeAlert: UIAlertController?
    var viewsHidden = false
    var heartAnimationPoints: [CGPoint] = [CGPoint]()
    let moreSettingVC = TRTCLiveRoomMoreControllerUI()
    lazy var topView: TCShowLiveTopView = {
        let statusBarHeight = Int(UIApplication.shared.statusBarFrame.size.height)
        var topView = TCShowLiveTopView(
            frame: CGRect(x: 5, y: statusBarHeight + 5, width: 180, height: 48),
            isHost: true,
            roomName: self.liveInfo?.roomName ?? "",
            audienceCount: self.liveInfo?.memberCount ?? 0,
            likeCount: 0,
            hostFaceUrl: self.liveInfo?.streamUrl ?? "")
        return topView
    }()
    lazy private var tap: UITapGestureRecognizer = {
        let tap = UITapGestureRecognizer(target: self, action: #selector(clickScreen(_:)))
        return tap
    }()
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        addGestureRecognizer(tap)
        initUI()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        NotificationCenter.default.removeObserver(self)
    }
    
    func setButtonHidden(_ buttonHidden: Bool) {
        btnChat.isHidden = buttonHidden
        btnCamera.isHidden = buttonHidden
        btnBeauty.isHidden = buttonHidden
        btnPK.isHidden = buttonHidden
        btnMusic.isHidden = buttonHidden
        closeBtn.isHidden = buttonHidden
        setting.isHidden = buttonHidden
    }
    
    func setLive(_ liveInfo: TRTCLiveRoomInfo!) {
        isPreview = false
        self .setButtonHidden(false)
        self.liveInfo = liveInfo
        
        addSubview(topView)
        topView.snp.makeConstraints({ make in
            make.centerY.equalTo(closeBtn)
            make.leading.equalTo(self).offset(20)
            make.width.equalTo(160)
            make.height.equalTo(48)
        })
        topView.clickHead = { [weak self] in
            guard let `self` = self else { return }
            guard let delegate = self.delegate else {
                return
            }
            delegate.clickLog()
        }
        topView.startLive()
        topView.setRoomId(liveInfo.roomId)
        
        let audience_width: CGFloat = width - 25 - topView.right
        let x = topView.right + 10 + (audience_width  - CGFloat(IMAGE_SIZE)) / 2
        let y = topView.center.y - audience_width / 2
        audienceTableView = TCAudienceListTableView(frame: CGRect(x: x, y: y, width: topView.height, height: audience_width), style: UITableView.Style.grouped, live: self.liveInfo!)
        
        audienceTableView!.transform = CGAffineTransform(rotationAngle: -.pi / 2)
        insertSubview(audienceTableView!, at: 0)
        let center = CGPoint(x: audienceTableView?.center.x ?? 0, y: closeBtn.center.y)
        audienceTableView?.center = center
    }
    
    func setLiveRoom(_ liveRoom: TRTCLiveRoom?) {
        self.liveRoom = liveRoom
        vPKPanel.liveRoom = self.liveRoom
    }
    
    func initUI() {
        isPreview = true
        let icon_size = BOTTOM_BTN_ICON_WIDTH
        let margin = (UIScreen.main.bounds.size.width - CGFloat(icon_size * 6)) / 7
        
        btnChat.setBackgroundImage(UIImage(named: "comment", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        btnChat.addTarget(self, action: #selector(clickChat(_:)), for: .touchUpInside)
        addSubview(btnChat)
        btnChat.snp.makeConstraints({ make in
            make.leading.equalTo(self).offset(margin)
            if #available(iOS 11.0, *) {
                make.bottom.equalTo(self).offset(-35)
            } else {
                make.bottom.equalTo(self).offset(-15)
            }
            make.size.equalTo(CGSize(width: icon_size, height: icon_size))
        })
        
        btnCamera.setImage(UIImage(named: "live_camera", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        btnCamera.addTarget(self, action: #selector(clickCamera(_:)), for: .touchUpInside)
        addSubview(btnCamera)
        btnCamera.snp.makeConstraints({ make in
            make.leading.equalTo(btnChat.snp.trailing).offset(margin)
            make.size.centerY.equalTo(btnChat)
        })
        
        btnPK.setImage(UIImage(named: "live_pk_start", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        btnPK.setImage(UIImage(named: "live_pk_end", in: liveRoomBundle(), compatibleWith: nil), for: .selected)
        btnPK.addTarget(self, action: #selector(clickPK(_:)), for: .touchUpInside)
        addSubview(btnPK)
        btnPK.snp.makeConstraints({ make in
            make.leading.equalTo(btnCamera.snp.trailing).offset(margin)
            make.size.centerY.equalTo(btnChat)
        })
        
        btnBeauty.setImage(UIImage(named: "live_beauty", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        btnBeauty.addTarget(self, action: #selector(clickBeauty(_:)), for: .touchUpInside)
        addSubview(btnBeauty)
        btnBeauty.snp.makeConstraints({ make in
            make.leading.equalTo(btnPK.snp.trailing).offset(margin)
            make.size.centerY.equalTo(btnChat)
        })

        btnMusic.setImage(UIImage(named: "music_icon", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        btnMusic.addTarget(self, action: #selector(clickMusic(_:)), for: .touchUpInside)
        addSubview(btnMusic)
        btnMusic.snp.makeConstraints({ make in
            make.leading.equalTo(btnBeauty.snp.trailing).offset(margin)
            make.size.centerY.equalTo(btnChat)
        })
        
        setting.setImage(UIImage(named: "live_more", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        setting.addTarget(self, action: #selector(clickSetting(_:)), for: .touchUpInside)
        addSubview(setting)
        setting.snp.makeConstraints({ make in
            make.leading.equalTo(btnMusic.snp.trailing).offset(margin)
            make.size.centerY.equalTo(btnChat)
        })

        //pk view
        pkView = UIImageView(image: UIImage(named: "PK", in: liveRoomBundle(), compatibleWith: nil))
        pkView!.frame = CGRect(
            x: UIScreen.main.bounds.size.width / 2.0 - 25,
            y: UIScreen.main.bounds.size.height / 2.0 - 25,
            width: 50,
            height: 25)
        pkView!.isHidden = true
        addSubview(pkView!)

        closeBtn.setImage(UIImage(named: "live_exit", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        closeBtn.backgroundColor = UIColor.clear
        closeBtn.addTarget(self, action: #selector(closeVC), for: .touchUpInside)
        addSubview(closeBtn)
        bringSubviewToFront(closeBtn)
        closeBtn.snp.makeConstraints { make in
            make.trailing.equalTo(self).offset(-20)
            make.width.equalTo(52)
            make.height.equalTo(52)
            if #available(iOS 11.0, *) {
                make.top.equalTo(self).offset(60)
            } else {
                make.top.equalTo(self).offset(20)
            }
        }
        
        vPKPanel = AnchorPKPanel()
        vPKPanel.backgroundColor = UIColor.clear
        vPKPanel.isHidden = true
        vPKPanel.pkWithRoom = { [weak self] room in
            guard let `self` = self else { return }
            guard let delegate = self.delegate else {return }
            delegate.pk(withRoom: room)
        }
        vPKPanel.shouldHidden = { [weak self] in
            guard let `self` = self else { return }
            self.addGestureRecognizer(self.tap)
        }
        addSubview(vPKPanel)
        var bottomOffset: CGFloat = 0
        if #available(iOS 11, *) {
            bottomOffset = UIApplication.shared.keyWindow?.safeAreaInsets.bottom ?? 0.0
        }
        vPKPanel.size(with: CGSize(width: width, height: 348 + bottomOffset))
        vPKPanel.alignParentTop(withMargin: height - vPKPanel.height)
        vPKPanel.alignParentLeft(withMargin: 0)
        
        cover.frame = CGRect(x: 10.0, y: CGFloat(55 + 2 * icon_size), width: width - 20, height: height - 75 - CGFloat(3 * icon_size))
        cover.backgroundColor = UIColor.white
        cover.alpha = 0.5
        cover.isHidden = true
        addSubview(cover)
        self.setButtonHidden(true)
    }
    
    func triggeValue() {
        
    }
    
    @objc func clickScreen(_ gestureRecognizer: UITapGestureRecognizer?) {
        _ = gestureRecognizer?.location(in: self)
        vPKPanel.hiddenPanel()
        delegate?.clickScreen(gestureRecognizer)
        
    }
    
    @objc  func clickBullet(_ btn: UIButton?) {
        bulletBtnIsOn = !bulletBtnIsOn
        btn?.isSelected = bulletBtnIsOn
    }
    
    @objc func clickChat(_ button: UIButton?) {
        guard let delegate = delegate else {
            return
        }
        delegate.clickChat(button)
    }
    
    @objc func clickCamera(_ button: UIButton?) {
        guard let delegate = delegate else {
            return
        }
        delegate.clickCamera(button)
    }

    
    @objc func clickPK(_ button: UIButton?) {
        removeGestureRecognizer(tap)
        guard let delegate = delegate else {
            return
        }
        delegate.clickPK(button)
    }
    
    @objc func changeButtonText() {
        closeBtn.isHidden = false
        pkView?.isHidden = true
    }
    
    @objc func clickBeauty(_ button: UIButton?) {
        guard let delegate = delegate else { return }
        delegate.clickBeauty(button)
    }
    
    @objc func clickMusic(_ button: UIButton?) {
        guard let delegate = delegate else { return }
        delegate.clickMusic(button)
    }
    
    @objc func clickSetting(_ button: UIButton?) {
        anchorViewController?.presentBottom(self.moreSettingVC)
    }
    
    
    // MARK: TCAnchorToolbarDelegate
    @objc func closeVC() {
        closeAlert = UIAlertController(title: nil, message: liveRoomLocalize("Demo.TRTC.LiveRoom.interactioninprogress"), preferredStyle: .alert)
        let cancelAction = UIAlertAction(title: liveRoomLocalize("Demo.TRTC.LiveRoom.subCancel"), style: .cancel, handler: { action in
        })
        let otherAction = UIAlertAction(title: liveRoomLocalize("Demo.TRTC.LiveRoom.subConfirm"), style: .default, handler: { [weak self] action in
            guard let self = self else { return }
            guard let delegate = self.delegate else{
                return
            }
            self.topView.pauseLive()
            NotificationCenter.default.removeObserver(self)
            delegate.closeRTMP()
//            self.anchorViewController?.beautyViewModel?.applyDefaultSetting()
            let resultFrame = CGRect(x: UIScreen.main.bounds.size.width / 4.0,
                                     y: UIScreen.main.bounds.size.height / 3.0,
                                     width: UIScreen.main.bounds.size.width / 2.0,
                                     height: 222)
            self.resultView = TCPushShowResultView(frame: resultFrame,
                                                   resultData: self.topView,
                                                   backHomepage: {
                delegate.closeVC()
            })
            self.clearView = UIView(frame: self.bounds)
            self.clearView!.backgroundColor = UIColor.black
            self.clearView!.alpha = 0.3
            self.addSubview(self.clearView!)
            self.addSubview(self.resultView!)
            self.superview?.bringSubviewToFront(self)
        })
        closeAlert!.addAction(cancelAction)
        closeAlert!.addAction(otherAction)
        anchorViewController!.present(closeAlert!, animated: true)
    }
    
    @objc func changeButtonStopPK() {
        closeBtn.isHidden = false
        pkView!.isHidden = false
        closeBtn.removeTarget(nil, action: nil, for: .allEvents)
        closeBtn.addTarget(self, action: #selector(quitRoomPKAction(_:)), for: .touchUpInside)
    }
    
    @objc func quitRoomPKAction(_ sender: UIButton?) {
        guard let liveRoom = liveRoom else { return }
        liveRoom.quitRoomPK(callback: { code, error in })
        closeBtn.addTarget(self, action: #selector(closeVC), for: .touchUpInside)
    }
    
    func getLocation(_ bulletView: TCMsgBarrageView?) -> CGFloat {
        guard let bulletView = bulletView else { return 0.0 }
        let view = bulletView.lastAnimateView
        let rect = view.layer.presentation()?.frame
        return (rect?.origin.x ?? 0.0) + (rect?.size.width ?? 0.0)
    }
    
    func onLogout(notice: Notification?) {
        closeInternal()
    }
    
    func closeInternal() {
        topView.pauseLive()
        NotificationCenter.default.removeObserver(self)
        guard let delegate = delegate else { return  }
        delegate.closeRTMP()
        delegate.closeVC()
    }
    
    func enableMix(disable: Bool) {
        btnMusic.isEnabled = disable
    }
    
    override func point(inside point: CGPoint, with event: UIEvent?) -> Bool {
        return true
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
    
    func isAlready(inAudienceList model: TCMsgModel) -> Bool {
        guard let audienceTableView = audienceTableView else {
            return false
        }
        return audienceTableView.isAlready(inAudienceList: model)
    }
    
    func onEffectViewHidden(_ isHidden: Bool) {
        if isHidden {
            addGestureRecognizer(tap)
        }
    }
    
    func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent) {
        guard let allTouches = event.allTouches else { return }
        let touch = allTouches.first
        guard let touch = touch else { return }
        touchBeginLocation = touch.location(in: self)
        if !vPKPanel.isHidden {
            vPKPanel.isHidden = true
            closeBtn.isHidden = false
            addGestureRecognizer(tap)
        }
    }
}
