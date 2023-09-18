//
//  TCAudienceViewController.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/25.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation
import TUICore

public class TCAudienceViewController: UIViewController, TRTCLiveRoomDelegate,TCAudienceToolbarDelegate {
    public var liveInfo: TRTCLiveRoomInfo?
    public var onPlayError: (() -> Void)?
    public var logicView: TCAudienceToolbarView?
    public var liveRoom: TRTCLiveRoom?
    public var log_switch = false
    public var videoParentView: UIView = UIView()
    public var roomStatus :TRTCLiveRoomLiveStatus
    public var isOwnerEnter = false
    private var playType: TX_Enum_PlayType?
    private var trackingTouchTS: Int64 = 0
    private var startSeek = false
    private var videoPause = false
    private var videoFinished = false
    private var sliderValue: Float = 0.0
    private var isLivePlay = false
    private var isInVC = false
    private var rotate = false
    private var isErrorAlert = false
    //link mic
    private var isBeingLinkMic = false
    private var isWaitingResponse = false
    private var btnCamera: UIButton = UIButton(type: .custom)
    private var btnLinkMic: UIButton = UIButton(type: .custom)
    private var isStop = false
    private var statusInfoViewArray: [TCStatusInfoComponet]
    private var noOwnerTip: UILabel?
    private var errorCode = 0
    private var errorMsg: String?
    private var beginTime: UInt64 = 0
    private var endTime: UInt64 = 0
    
    private var barrageInputView: UIView? = nil
    private var barrageView: UIView? = nil
    private var giftButton: UIButton? = nil
    private var giftView: UIView? = nil
    private var giftPanelView: UIView? = nil
    
    lazy var waitingNotice: UITextView = {
        let waitingNotice = UITextView()
        waitingNotice.isEditable = false
        waitingNotice.isSelectable = false
        waitingNotice.textColor = UIColor.white
        waitingNotice.backgroundColor = UIColor.white
        waitingNotice.backgroundColor = UIColor(hex: "29CC85")
        self.view.addSubview(waitingNotice)
        return waitingNotice
    }()
    
    
    public init(roomInfo: TRTCLiveRoomInfo?) {
        liveInfo = roomInfo
        videoPause = false
        videoFinished = true
        isInVC = false
        log_switch = false
        errorCode = 0
        errorMsg = ""
        isLivePlay = true
        rotate = false
        isErrorAlert = false
        isOwnerEnter = false
        isStop = false
        //link mic
        isBeingLinkMic = false
        isWaitingResponse = false
        roomStatus = TRTCLiveRoomLiveStatus.none
        statusInfoViewArray = [TCStatusInfoComponet]()
        super.init(nibName: nil, bundle: nil)
        liveRoom?.delegate = self
        
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(true, animated: false)
        _ = startRtmp()
        isInVC = true
        if errorCode != 0 {
            onError(errorCode, errMsg: errorMsg, extraInfo: nil)
            errorCode = 0
            errorMsg = ""
        }
    }
    
    public override func viewWillDisappear(_ animated: Bool) {
        super.viewWillDisappear(animated)
        stopRtmp()
        isInVC = false
    }
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        LiveRoomToastManager.sharedManager().setupToast()
        // load bg view
        let backImage = UIImage(named: "avatar0_100", in: liveRoomBundle(), compatibleWith: nil)
        var clipImage: UIImage? = nil
        if let backImage = backImage {
            let backImageNewHeight = view.height
            let backImageNewWidth = backImageNewHeight * backImage.size.width / backImage.size.height
            
            if let gsImage = TCUtil.gsImage(backImage, withGsNumber: 10) {
                if let scaleImage = TCUtil.scale(gsImage, scaleTo: CGSize(width: backImageNewWidth, height: backImageNewHeight)) {
                    clipImage = TCUtil.clipImage(scaleImage, in: CGRect(x: (backImageNewWidth -
                     view.width) / 2, y: (backImageNewHeight - view.height) / 2, width: view.width,
                     height: view.height))
                }
            }
        }
        let backgroundImageView = UIImageView(frame: view.frame)
        backgroundImageView.image = clipImage
        backgroundImageView.contentMode = .scaleToFill
        backgroundImageView.backgroundColor = UIColor.appBackGround
        view.addSubview(backgroundImageView)
        noOwnerTip = UILabel(frame: CGRect(x: 0, y: view.bounds.size.height / 2 - 40, width: view.bounds.size.width, height: 30))
        guard let noOwnerTip = noOwnerTip else { return }
        noOwnerTip.backgroundColor = UIColor.clear
        noOwnerTip.textColor = UIColor.white
        noOwnerTip.textAlignment = .center
        noOwnerTip.text = liveRoomLocalize("Demo.TRTC.LiveRoom.anchornotonline")
        noOwnerTip.isHidden = true
        view.addSubview(noOwnerTip)

        // video view
        videoParentView .frame = view.frame
        videoParentView.tag = FULL_SCREEN_PLAY_VIDEO_VIEW
        view.addSubview(videoParentView)
        videoParentView.isHidden = true
        initLogicView()
        beginTime = UInt64(Date().timeIntervalSince1970)
        TUILogin.add(self)
        
        activeTUIWidget()
        // Enter Room
        enterRoom()
    }
    
    deinit {
        TUILogin.remove(self)
    }
    // MARK:  -TCAudienceToolbarDelegate
    
    func getGiftButton() -> UIButton? {
        if giftButton == nil {
            let giftButtonList = TUICore.getExtensionList(TUICore_TUIGiftExtension_GetEnterBtn,
                                                          param: [:])
            giftButton = giftButtonList.first?.data?[TUICore_TUIGiftExtension_GetEnterBtn] as? UIButton
            giftButton?.addTarget(self, action: #selector(clickGiftButton(_:)), for: .touchUpInside)
        }
        return giftButton
    }
    
    func closeVC(_ popViewController: Bool) {
        stopLocalPreview()
        stopLinkMic()
        closeVC(withRefresh: false, popViewController: popViewController)
        hideWaitingNotice()
        TUILiveRoom.sharedInstance.isEnterRoom = false
    }
    
    func clickLog() {
        log_switch = !log_switch
        liveRoom?.showVideoDebugLog(log_switch)
    }
    
    func clickChat() {
        guard let barrageInputView = self.barrageInputView else {
            return
        }
        view.bringSubviewToFront(barrageInputView)
        barrageInputView.isHidden = false
    }
    
    func clickLike() {
        guard let groupId = liveInfo?.roomId else { return }
        TUICore.callService(TUICore_TUIGiftService, method: TUICore_TUIGiftService_SendLikeMethod, param: ["groupId": groupId])
    }
    
    func clickLinkMic() {
        clickBtnLinkMic(btnLinkMic)
    }
    
    func clickCamera() {
        clickBtnCamera(nil)
    }
    
    func onSeek(_ slider: UISlider?) {
        //    [self.liveRoom seek:_sliderValue];
        trackingTouchTS = Int64(Date().timeIntervalSince1970 * 1_000)
        startSeek = false
    }
    
    func onSeekBegin(_ slider: UISlider?) {
        startSeek = true
    }
    
    func onDrag(_ slider: UISlider?) {
        let progress = slider?.value ?? 0.0
        let intProgress = Int(progress + 0.5)
        logicView?.playLabel.text = String(format: "%02d:%02d:%02d", intProgress / 3600, intProgress / 60, intProgress % 60)
        sliderValue = slider?.value ?? 0.0
    }
    
    func onRecvGroupDeleteMsg() {
        closeVC(false)
        if !isErrorAlert {
            isErrorAlert = true
            showAlert(withTitle: liveRoomLocalize("Demo.TRTC.LiveRoom.endedinteractive"), sureAction: { [weak self] in
                guard let self = self else { return }
                if let isRoot = self.navigationController?.viewControllers.first?.isEqual(self), isRoot {
                    self.dismiss(animated: true)
                } else {
                    self.navigationController?.popViewController(animated: true)
                }
            })
        }
    }
    
    func clickPlayVod() {
        if !videoFinished {
            if (playType == TX_Enum_PlayType.PLAY_TYPE_LIVE_FLV) || (playType ==
             TX_Enum_PlayType.PLAY_TYPE_LIVE_HLS) {
                if videoPause {
                    assert(false, "")
                    //                [self.liveRoom resume];
                    logicView?.playBtn.setImage(UIImage(named: "pause", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
                    UIApplication.shared.isIdleTimerDisabled = true
                } else {
                    assert(false, "")
                    //                [self.liveRoom pause];
                    logicView?.playBtn.setImage(UIImage(named: "start", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
                    UIApplication.shared.isIdleTimerDisabled = false
                }
                videoPause = !videoPause
            } else {
                _ = startRtmp()
                logicView?.playBtn.setImage(UIImage(named: "pause", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
                UIApplication.shared.isIdleTimerDisabled = true
            }
        }
        
    }
    
    func clickScreen(_ position: CGPoint) {
        
    }
    
    func initLogicView() {
        if logicView == nil {
            var bottom: CGFloat = 0
            if #available(iOS 11, *) {
                bottom = UIApplication.shared.keyWindow?.safeAreaInsets.bottom ?? 0.0
            }
            var frame = view.frame
            frame.size.height -= bottom
            logicView = TCAudienceToolbarView(frame: frame, live: liveInfo, withLinkMic: true, delegate: self)
            guard let logicView = logicView else { return }
            logicView.liveRoom = liveRoom
            logicView.setRoomId(liveInfo?.roomId)
            btnLinkMic = logicView.linkMicBtn
            btnCamera = logicView.cameraBtn
            btnCamera.isHidden = true
            initStatusInfoView(0)
            initStatusInfoView(1)
            initStatusInfoView(2)
            view.addSubview(logicView)
            
        }
    }
    
    func initStatusInfoView(_ index: Int) {
        let width = view.size.width        
        let statusInfoView = TCStatusInfoComponet()
        let y = CGFloat(SafeAreaTopHeight) + CGFloat(VIDEO_VIEW_HEIGHT * index) + 68
        
        let videoView = UIView(frame: CGRect(x: Int(CGFloat(Int(CGFloat(Int(width) - VIDEO_VIEW_WIDTH -
         VIDEO_VIEW_MARGIN_RIGHT)))), y: Int(y) , width: VIDEO_VIEW_WIDTH, height: VIDEO_VIEW_HEIGHT))
        statusInfoView.setVideoView(videoView)
        let x = Int(width) - VIDEO_VIEW_WIDTH - VIDEO_VIEW_MARGIN_RIGHT
        statusInfoView.linkFrame = CGRect(x:x , y: Int(y), width: VIDEO_VIEW_WIDTH, height: VIDEO_VIEW_HEIGHT)
        if let videoView = statusInfoView.videoView {
            view.addSubview(videoView)
        }
        statusInfoViewArray.append(statusInfoView)
    }
    
    private func enterRoom() {
        guard let roomIdStr = liveInfo?.roomId, let roomId = UInt32(roomIdStr) else {
            setIsOwnerEnter(false)
            return
        }
        liveRoom?.enterRoom(roomID: roomId) { [weak self] (code, msg) in
            guard let self = self else { return }
            if code == 0 {
                self.setIsOwnerEnter(true)
            } else {
                self.makeToast(msg ?? "enter room error")
                self.setIsOwnerEnter(false)
            }
        }
    }
    
    func startPlay() -> Bool {
        initRoomLogic()
        return true
    }
    
    func startRtmp() -> Bool {
        UIApplication.shared.isIdleTimerDisabled = true
        return startPlay()
    }
    
    func stopRtmp() {
        if !isStop {
            isStop = true
        } else {
            return
        }
        liveRoom?.showVideoDebugLog(false)
        if liveRoom != nil, let liveRoom = liveRoom{
            liveRoom.exitRoom(callback: { code, error in
                print(String(format: "exitRoom: errCode[%ld] errMsg[%@]", code, error ?? ""))
            })
        }
        UIApplication.shared.isIdleTimerDisabled = false
    }
    
    func onError(_ errCode: Int, errMsg: String?, extraInfo: [AnyHashable : Any]?) {
        DispatchQueue.main.async(execute: { [weak self] in
            print("onError:\(errCode), \(errMsg ?? "")")
            guard let `self` = self else { return }
            if errCode != 0 {
                if self.isInVC {
                    self.showAlert(withTitle: liveRoomLocalize("Demo.TRTC.LiveRoom.anchorcloseinteractionroom"), sureAction: {
                        self.closeVC(withRefresh: true, popViewController: true)
                    })
                } else {
                    self.errorCode = errCode
                    self.errorMsg = errMsg
                }
            }
        })
    }
    
    func showAlert(withTitle title: String?, sureAction callback: @escaping () -> Void) {
        let alertController = UIAlertController(title: title, message: nil, preferredStyle: .alert)
        let sureAction = UIAlertAction(title: liveRoomLocalize("Demo.TRTC.LiveRoom.confirm"), style: .default, handler: {  action in
            callback()
        })
        alertController.addAction(sureAction)
        present(alertController, animated: true)
        
    }
    
    func initRoomLogic() {
        liveRoom!.delegate = self
        guard let liveRoom  = liveRoom else {
            return
        }
        guard let liveInfo = liveInfo else {
            return
        }
        var isGetList = false
        DispatchQueue.main.asyncAfter(deadline: DispatchTime.now() + Double(Int64(0.5 * Double(NSEC_PER_SEC))) / Double(NSEC_PER_SEC), execute: { [self] in
            liveRoom.getAudienceList(callback: { [weak self] code, error, users in
                guard let `self` = self else { return }
                isGetList = code == 0
                self.logicView?.initAudienceList(users)
            })
        })
        
        DispatchQueue.main.asyncAfter(deadline: DispatchTime.now() + Double(Int64(1 *
         Double(NSEC_PER_SEC))) / Double(NSEC_PER_SEC), execute: { [self] in
            if isGetList {
                return
            }
            liveRoom.getAudienceList(callback: { [weak self] code, error, users in
                isGetList = code == 0
                guard let `self` = self else { return }
                self.logicView?.initAudienceList(users)
            })
        })
    }
    
    func makeToast(_ message: String?) {
        LiveRoomToastManager.sharedManager().makeToast(view: view, message: message ?? "")
    }
    
    
    func hideWaitingNotice() {
        waitingNotice.removeFromSuperview()
    }
    
    func closeVC(withRefresh refresh: Bool, popViewController: Bool) {
        stopRtmp()
        NotificationCenter.default.removeObserver(self)
        
        if refresh {
            DispatchQueue.main.async(execute: { [weak self] in
                guard let `self` = self else { return }
                if self.onPlayError != nil {
                    self.onPlayError!()
                }
            })
        }
        if popViewController {
            if let isRoot = navigationController?.viewControllers.first?.isEqual(self), isRoot {
                dismiss(animated: true)
            } else {
                navigationController?.popViewController(animated: true)
            }
        }
        TUILiveRoom.sharedInstance.isEnterRoom = false
    }
    
    func stopLinkMic() {
        for statusInfoView in statusInfoViewArray {
            statusInfoView.stopLoading()
            statusInfoView.stopPlay()
            if let userID = statusInfoView.userID {
                liveRoom?.stopPlay(userID: userID, callback: { code, error in
                })
            }
            statusInfoView.emptyPlayInfo()
        }
    }
    
    func stopLocalPreview() {
        if isBeingLinkMic == true {
            liveRoom?.stopPublish(callback: { code, error in
                
            })
            for statusInfoView in statusInfoViewArray {
                if statusInfoView.userID == TUILiveRoomProfileManager.sharedManager().userId {
                    liveRoom?.stopCameraPreview()
                    statusInfoView.stopLoading()
                    statusInfoView.stopPlay()
                    statusInfoView.emptyPlayInfo()
                    break
                }
            }
            btnLinkMic.setImage(UIImage(named: "linkmic_on", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
            btnLinkMic.isEnabled = true
            btnCamera.isHidden = true
            
            isBeingLinkMic = false
            isWaitingResponse = false
        }
    }
    
    @objc func onWaitLinkMicResponseTimeOut() {
        if isWaitingResponse == true {
            isWaitingResponse = false
            btnLinkMic.setImage(UIImage(named: "linkmic_on", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
            btnLinkMic.isEnabled = true
            hideWaitingNotice()
            TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.micconnecttimeoutandanchornoresponse"), parentView: view)
        }
    }
    
    func showWaitingNotice(_ notice: String?) {
        var frameRC = UIScreen.main.bounds
        frameRC.origin.y = frameRC.size.height - (IPHONE_X ? 144 : 110)
        frameRC.size.height -= 110
        frameRC.size.height = CGFloat(TCUtil.height(forString: waitingNotice , andWidth: Float(frameRC.size.width)))
        waitingNotice.frame = frameRC
        waitingNotice.text = notice
        DispatchQueue.main.asyncAfter(deadline: DispatchTime.now() + 500.0, execute: { [weak self] in
            guard let `self` = self else { return }
            self.freshWaitingNotice(notice, withIndex: NSNumber.init(value:  0))
        })
    }
    
    func freshWaitingNotice(_ notice: String?, withIndex numIndex: NSNumber?) {
        var numIndex: NSNumber = numIndex ?? NSNumber(0)
        var index: Int = numIndex.intValue
        index += 1
        index = index % 4
        var text = notice
        for _ in 0..<index {
            text = "\(text ?? "")....."
        }
        waitingNotice.text = text
        numIndex = NSNumber.init(value: index)
        DispatchQueue.main.asyncAfter(deadline: DispatchTime.now() + Double(500 * NSEC_PER_MSEC) / Double(NSEC_PER_SEC), execute: { [weak self] in
            guard let `self` = self else { return }
            self.freshWaitingNotice(notice, withIndex: numIndex)
        })
    }
    
    func onDebugLog(_ msg: String?) {
        print("onDebugMsg:\(msg ?? "")")
    }
    
    func onRoomDestroy(_ roomID: String?) {
        DispatchQueue.main.async(execute: { [weak self] in
            guard let `self` = self else { return }
            print("onRoomDestroy, roomID:\(roomID ?? "")")
            self.showAlert(withTitle: liveRoomLocalize("Demo.TRTC.LiveRoom.anchorcloseinteraction"), sureAction: {
                self.closeVC(withRefresh: true, popViewController: true)
            })
        })
    }
    
    func onKickoutJoinAnchor() {
        TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.sorryforkicked"), parentView: view)
        stopLocalPreview()
    }
    
    func getStatusInfoView(from userID: String?) -> TCStatusInfoComponet? {
        if let userID = userID {
            for statusInfoView in statusInfoViewArray {
                if userID == statusInfoView.userID {
                    return statusInfoView
                }
            }
        }
        return nil
    }
    
    func isNoAnchorINStatusInfoView() -> Bool {
        for statusInfoView in statusInfoViewArray {
            if statusInfoView.userID != nil  {
                return false
            }
        }
        return true
    }
    
    func onLiveEnd() {
        onRecvGroupDeleteMsg()
    }
    
    func onAnchorEnter(_ userID: String?) {
        guard let userID = userID else {
            return
        }
        if userID == TUILiveRoomProfileManager.sharedManager().userId {
            return
        }
        let noAnchor = isNoAnchorINStatusInfoView()
        var bExist = false
        for statusInfoView in statusInfoViewArray {
            if userID == statusInfoView.userID {
                bExist = true
                break
            }
        }
        if bExist == true { return }
        for statusInfoView in statusInfoViewArray {
            if statusInfoView.userID == nil || statusInfoView.userID?.count == 0 {
                statusInfoView.userID = userID
                statusInfoView.startLoading()
                liveRoom!.startPlay(userID: userID, view: statusInfoView.videoView!, callback: {
                    [weak self] code, error in
                    guard let `self` = self else { return }
                    if code == 0 {
                        statusInfoView.stopLoading()
                    } else {
                        self.onAnchorExit(userID)
                    }
                })
                break
            }
        }
        if noAnchor && roomStatus == TRTCLiveRoomLiveStatus.roomPK {
            switchPKMode()
        }
    }
    
    func onAnchorExit(_ userID: String?) {
        if userID == liveInfo?.ownerId {
            liveRoom?.stopPlay(userID: userID!, callback: { code, error in
            })
            isOwnerEnter = false
            self.onRoomDestroy("")
            return
        }
        
        if let statusInfoView = getStatusInfoView(from: userID) {
            if (statusInfoView.userID != TUILiveRoomProfileManager.sharedManager().userId) {
                statusInfoView.stopLoading()
                statusInfoView.stopPlay()
                liveRoom?.stopPlay(userID: (statusInfoView.userID)!, callback: { code, error in })
                statusInfoView.emptyPlayInfo()
                statusInfoView.userID = nil
            } else {
                stopLocalPreview()
                
            }
        } else {
            stopLocalPreview()
            
        }
        if isNoAnchorINStatusInfoView() {
            linkFrameRestore()
        }
    }
    
    func findFullScreenVideoView() -> UIView? {
        for view in view.subviews {
            if view.tag == FULL_SCREEN_PLAY_VIDEO_VIEW {
                return view
            }
        }
        return nil
    }
    
    @objc func clickBtnCamera(_ button: UIButton?) {
        if isBeingLinkMic {
            liveRoom?.switchCamera()
        }
    }
    
    func setIsOwnerEnter(_ isOwnerEnter: Bool) {
        self.isOwnerEnter = isOwnerEnter
        videoParentView.isHidden = !isOwnerEnter
        noOwnerTip?.isHidden = self.isOwnerEnter
    }
    
    func checkPlayUrl(_ playUrl: String?) -> Bool {
        guard let playUrl = playUrl else {
            return false
        }
        if !(playUrl.hasPrefix("http:") || playUrl.hasPrefix("https:") || playUrl.hasPrefix("rtmp:") ) {
            TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.addressillegalandsupportrfhm"), parentView: view)
            return false
        }
        if isLivePlay {
            if playUrl.hasPrefix("rtmp:") {
                playType = TX_Enum_PlayType.PLAY_TYPE_LIVE_RTMP
            } else if (playUrl.hasPrefix("https:") || playUrl.hasPrefix("http:") ) && ((playUrl as NSString?)?.range(of: ".flv").length ?? 0) > 0 {
                playType = TX_Enum_PlayType.PLAY_TYPE_LIVE_FLV
            } else {
                TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.addressillegalandsupportrf"), parentView: view)
                return false
            }
        } else {
            if playUrl.hasPrefix("https:") || playUrl.hasPrefix("http:") {
                if playUrl.range(of: ".flv") != nil {
                    playType = TX_Enum_PlayType.PLAY_TYPE_LIVE_FLV
                    
                    
                } else if playUrl.range(of: ".m3u8") != nil {
                    playType = TX_Enum_PlayType.PLAY_TYPE_LIVE_HLS
                } else {
                    TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.addressillegalandsupportfhm"), parentView: view)
                    return false
                }
                
            } else {
                TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.addressillegalandsupportfhm"), parentView: view)
                return false
            }
        }
        return true
    }
    
    func switchPKMode() {
        for statusInfoView in statusInfoViewArray {
            if statusInfoView.userID != nil {
                guard let videoView = statusInfoView.videoView else {
                    continue
                }
                videoView.frame = CGRect(x: view.frame.size.width / 2, y: 0, width:
                 view.frame.size.width / 2, height: view.frame.size.height / 2)
                break
            }
        }
    }
    
    func linkFrameRestore() {
        for statusInfoView in statusInfoViewArray {
            if statusInfoView.userID != nil {
                guard let videoView = statusInfoView.videoView else {
                    continue
                }
                videoView.frame = statusInfoView.linkFrame
            }
        }
    }
    
    // MARK: TRTCLiveRoomDelegate
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onRecvRoomTextMsg message: String, fromUser user: TRTCLiveUserInfo) {
        var info = IMUserAble()
        info.imUserId = user.userId
        info.imUserName = user.userName
        info.imUserIconUrl = user.avatarURL
        info.cmdType = TCMsgModelType.normal
        logicView?.handleIMMessage(info, msgText: message)
    }
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onRecvRoomCustomMsg command: String, message: String, fromUser user: TRTCLiveUserInfo) {
        var info = IMUserAble()
        info.imUserId = user.userId
        info.imUserName = user.userName
        info.imUserIconUrl = user.avatarURL
        info.cmdType = IntConversionTCMsgModelType(command: command)
        logicView?.handleIMMessage(info, msgText: message)
    }
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onAnchorEnter userID: String) {
        if userID == liveInfo?.ownerId {
            self.setIsOwnerEnter(true)
            liveRoom?.startPlay(userID: userID, view: videoParentView, callback: { code, message in
                
            })
        } else {
            onAnchorEnter(userID)
        }
    }
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onAnchorExit userID: String) {
        onAnchorExit(userID)
    }
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onAudienceEnter user: TRTCLiveUserInfo) {
        var info = IMUserAble()
        info.imUserId = user.userId
        info.imUserName = user.userName
        info.imUserIconUrl = user.avatarURL
        info.cmdType = TCMsgModelType.memberEnterRoom
        logicView?.handleIMMessage(info, msgText: "")
    }
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onAudienceExit user: TRTCLiveUserInfo) {
        var info = IMUserAble()
        info.imUserId = user.userId
        info.imUserName = user.userName
        info.imUserIconUrl = user.avatarURL
        info.cmdType = TCMsgModelType.memberQuitRoom
        logicView?.handleIMMessage(info, msgText: "")
    }
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onRoomInfoChange info: TRTCLiveRoomInfo) {
        let isCdnMode = UserDefaults.standard.bool(forKey: "liveRoomConfig_useCDNFirst")
        if isCdnMode {
            return
        }
        roomStatus = info.roomStatus
        switch info.roomStatus {
        case TRTCLiveRoomLiveStatus.single,TRTCLiveRoomLiveStatus.linkMic :
            UIView.animate(withDuration: 0.1, animations: { [weak self] in
                guard let `self` = self else { return }
                self.videoParentView.frame = self.view.frame
                self.linkFrameRestore()
            })
        case TRTCLiveRoomLiveStatus.roomPK:
            UIView.animate(withDuration: 0.1, animations: { [weak self] in
                guard let `self` = self else { return }
                self.videoParentView.frame = CGRect(x: 0, y: 0, width: self.view.frame.size.width * 0.5, height: self.view.frame.size.height * 0.5)
                self.switchPKMode()
            })
        default:
            break
        }
    }
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onError code: Int, message: String) {
        print("LiveRoomMessage:\(message) Error:\(code)")
        
    }
    
    @objc func clickGiftButton(_ button: UIButton?) {
        giftPanelView?.isHidden = false
    }
    
    @objc func clickBtnLinkMic(_ button: UIButton?) {
        
        button?.isUserInteractionEnabled = false
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) {
            button?.isUserInteractionEnabled = true
        }
        if isBeingLinkMic == false {
            let statusAudio: AVAuthorizationStatus = AVCaptureDevice.authorizationStatus(for: .audio)
            if statusAudio == .denied {
                TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.micauthorityfailed"), parentView: view)
                return
            }
            let statusVideo: AVAuthorizationStatus = AVCaptureDevice.authorizationStatus(for: .video)
            if statusVideo == .denied {
                TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.cameraauthorityfailed"), parentView: view)
                return
            }
            let version = UIDevice.current.systemVersion
            let versionflag = version.compare("8.0.0", options: NSString.CompareOptions.numeric)
            if versionflag == .orderedAscending{
                TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.notsupporthardencodeandstartmicconnectfailed"), parentView: view)
                return
            }
            
            startLinkMic()
        } else {
            stopLocalPreview()
        }
    }
    
    func startLinkMic() {
        if isBeingLinkMic || isWaitingResponse {
            return
        }
        isWaitingResponse = true
        
        NSObject.cancelPreviousPerformRequests(withTarget: self, selector: #selector(onWaitLinkMicResponseTimeOut), object: nil)
        perform(#selector(onWaitLinkMicResponseTimeOut), with: nil, afterDelay: 20)
        btnLinkMic.setImage(UIImage(named: "linkmic_off", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        btnLinkMic.isEnabled = false
        showWaitingNotice(liveRoomLocalize("Demo.TRTC.LiveRoom.waitforanchoraccept"))
        self.liveRoom?.requestJoinAnchor(reason: "", timeout: trtcLiveSendMsgTimeOut, responseCallback: { [weak self] agreed, reason in
            guard let `self` = self else { return }
            if self.isWaitingResponse == false || !self.isInVC {
                return
            }
            self.isWaitingResponse = false
            self.btnLinkMic.isEnabled = true
            self.hideWaitingNotice()
            if agreed {
                self.isBeingLinkMic = true
                self.btnLinkMic.setImage(UIImage(named: "linkmic_off", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
                TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.anchoracceptreqandbegan"), parentView: self.view)
                
                //推流允许前后切换摄像头
                self.btnCamera.isHidden = false
                for statusInfoView in self.statusInfoViewArray {
                    if statusInfoView.userID == nil || statusInfoView.userID?.count == 0 {
#if RTCube_APPSTORE
                        let selector = NSSelectorFromString("showAlertUserLiveTips")
                        if self.responds(to: selector) {
                            self.perform(selector)
                        }
#endif
                        statusInfoView.userID = TUILiveRoomProfileManager.sharedManager().userId
                        self.liveRoom?.startCameraPreview(frontCamera: true, view: statusInfoView.videoView ?? UIView(), callback: { code, error in
                            
                        })
                        let streamID = "\(TUILiveRoomProfileManager.sharedManager().userId)_stream"
                        self.liveRoom?.startPublish(streamID: streamID, callback: { code, error in
                            
                        })
                        break
                        
                        
                    }
                    
                }
            } else {
                self.isBeingLinkMic = false
                self.isWaitingResponse = false
                self.btnLinkMic.setImage(UIImage(named: "linkmic_on", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
                if reason?.count ?? 0 > 0 {
                    TCUtil.toastTip(reason, parentView: self.view)
                } else {
                    TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.refusemicconnectionreq"), parentView: self.view)
                }
            }
        })
    }
    
    public func trtcLiveRoomOnKickoutJoinAnchor(_ liveRoom: TRTCLiveRoom) {
        onKickoutJoinAnchor()
    }
}


// MARK: - Load Widget
extension TCAudienceViewController {
    
    private func activeTUIWidget() {
        if barrageView == nil && loadBarrageWidget(), let barrageView = barrageView, let barrageInputView = barrageInputView {
            view.addSubview(barrageView)
            view.addSubview(barrageInputView)
            barrageInputView.snp.makeConstraints { make in
                make.edges.equalToSuperview()
            }
            barrageView.snp.makeConstraints { make in
                make.leading.equalTo(20)
                make.top.equalTo(SCREEN_HEIGHT - 300 - 120)
                make.height.equalTo(300)
                make.width.equalTo(SCREEN_WIDTH - 20*2)
            }
        }
        
        if giftView == nil && loadGiftWidget(), let giftView = giftView, let giftPanelView = giftPanelView  {
            view.addSubview(giftView)
            view.addSubview(giftPanelView)
            giftView.snp.makeConstraints({ make in
                make.edges.equalToSuperview()
            })
            giftPanelView.snp.makeConstraints({ make in
                make.edges.equalToSuperview()
            })
        }
    }
    
    private func loadBarrageWidget() -> Bool {
        guard let liveInfo = liveInfo else {
            return false
        }
        let barrageInputList = TUICore.getExtensionList(TUICore_TUIBarrageExtension_GetTUIBarrageSendView,
                                                     param: ["frame": NSCoder.string(for: UIScreen.main.bounds),
                                                             "groupId": liveInfo.roomId])
        
        if barrageInputList.count == 0 {
            return false
        }
        guard let barrageInputView = barrageInputList[0].data?[TUICore_TUIBarrageExtension_GetTUIBarrageSendView] as? UIView else {
            return false
        }
        self.barrageInputView = barrageInputView

        let barrageViewFrame = CGRect(x: 20, y: SCREEN_HEIGHT - 300 - 120, width: SCREEN_WIDTH - 20*2, height: 300)
        let barrageViewList = TUICore.getExtensionList(TUICore_TUIBarrageExtension_TUIBarrageDisplayView,
                                                       param: ["frame": NSCoder.string(for: barrageViewFrame),
                                                               "groupId": liveInfo.roomId,
                                                               "maxHeight": "300",])
        
        if barrageViewList.count == 0 {
            return false
        }
        guard let barrageView = barrageViewList[0].data?[TUICore_TUIBarrageExtension_TUIBarrageDisplayView] as? UIView else {
            return false
        }
        self.barrageView = barrageView
        return true
    }
    
    private func loadGiftWidget() -> Bool {
        guard let liveInfo = liveInfo else {
            return false
        }
        let giftPlayList = TUICore.getExtensionList(TUICore_TUIGiftExtension_GetTUIGiftPlayView,
                                                    param: [
                                                        "frame": NSCoder.string(for: UIScreen.main.bounds),
                                                        "groupId": liveInfo.roomId])
        
        if giftPlayList.count == 0 {
            return false
        }
        guard let giftView = giftPlayList[0].data?[TUICore_TUIGiftExtension_GetTUIGiftPlayView] as? UIView else {
            return false
        }
        let giftPanelList = TUICore.getExtensionList(TUICore_TUIGiftExtension_GetTUIGiftListPanel,
                                                           param: [
                                                               "frame": NSCoder.string(for: UIScreen.main.bounds),
                                                               "groupId": liveInfo.roomId])
        
        if giftPanelList.count == 0 {
            return false
        }
        guard let giftPanelView = giftPanelList[0].data?[TUICore_TUIGiftExtension_GetTUIGiftListPanel] as? UIView else {
            return false
        }
        
        self.giftPanelView = giftPanelView
        self.giftView = giftView
        return true
    }

}

// MARK: - TUILoginListener
extension TCAudienceViewController: TUILoginListener {
    public func onConnecting() {
        
    }
    
    public func onConnectSuccess() {
        
    }
    
    public func onConnectFailed(_ code: Int32, err: String?) {
        
    }
    
    public func onKickedOffline() {
        closeVC(withRefresh: true, popViewController: true)
    }
    
    public func onUserSigExpired() {
        
    }
}
