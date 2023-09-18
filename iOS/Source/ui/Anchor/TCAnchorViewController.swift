//
//  TCAnchorViewController.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/29.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation
import CWStatusBarNotification
import UIKit
import TUICore

public class TCAnchorViewController: UIViewController {
    
    var liveInfo: TRTCLiveRoomInfo
    public var liveRoom: TRTCLiveRoom?
    // XMagic License 【Optional】
    public var xMagicLicenseURL: String = ""
    public var xMagicLicenseKey: String = ""
    
    var videoParentView: UIView = UIView()
    var logicView: TCAnchorToolbarView?
    var log_switch = false
    var setLinkMemeber: Set<String> = []
    var curPkRoom :TRTCLiveRoomInfo?
    var roomStatus :TRTCLiveRoomLiveStatus = TRTCLiveRoomLiveStatus.none
    var camera_switch = false
    var curRequest: TRTCLiveUserInfo = TRTCLiveUserInfo()
    var testPath: String = ""
    var appIsInterrupt = false
    var isPKEnter = false
    var notification: CWStatusBarNotification =  CWStatusBarNotification()
    //link mic
    var sessionId: String?
    var userIdRequest: String
    var statusInfoViewArray: [TCStatusInfoComponet]?
    var isSupprotHardware = false
    var beginTime: UInt64 = 0
    var endTime: UInt64 = 0
    var isStop = false
    var publishBtn: UIButton = UIButton(type: .custom)
    var cameraBtn: UIButton = UIButton(type: .custom)
    var beautyBtn: UIButton = UIButton(type: .custom)
    var closeBtn: UIButton = UIButton(type: .custom)
    var musicQualityButton: UIButton = UIButton(type: .custom)
    var standardQualityButton: UIButton = UIButton(type: .custom)
    
    var createTopPanel: UIView = UIView ()
    var createTopPanelBG: UIView = UIView ()

    var userAvatar: UIImageView = UIImageView()
    var userName: UILabel = UILabel()


    lazy var roomName: UITextField = {
        let textField = UITextField()
#if RTCube_APPSTORE
        textField.isUserInteractionEnabled = false
#endif
        return textField
    }()

    var pkalert : UIAlertController?
    var joinAnchor : UIAlertController?
    
    var beautyView: UIView? = nil
    var audioEffectView: UIView? = nil
    var barrageInputView: UIView? = nil
    var barrageView: UIView? = nil
    var giftView: UIView? = nil
    
    private lazy var countdownView: TUILiveCountDownView = {
        let view = TUILiveCountDownView(frame: UIScreen.main.bounds)
        return view
    }()
    
    func audioQualityDefaultColor() -> UIColor? {
        return UIColor(hex: "F4F5F9")
    }
    
    func audioQualitySelectedColor() -> UIColor? {
        return UIColor(hex: "29CC85")
    }
    
    func quitPK() {
        guard  let liveRoom = liveRoom else {
            return
        }
        liveRoom.quitRoomPK(callback: { code, error in
            
        })
        UIView.animate(withDuration: 0.1, animations: { [ weak self] in
            guard let `self` = self else { return }
            self.onAnchorExit(self.curPkRoom?.ownerId)
        })
    }
    
    func onAnchorExit(_ userID: String?) {
        guard let userId = userID else { return }
        if userId == liveInfo.ownerId {
            return
        }
        let statusInfoView = getStatusInfoView(from: userId)
        if let statusInfoView = statusInfoView {
            statusInfoView.stopLoading()
            statusInfoView.stopPlay()
            liveRoom?.stopPlay(userID: statusInfoView.userID ?? "", callback: { code, error in
                
            })
            statusInfoView.emptyPlayInfo()
        }
        // 清理连麦信息
        setLinkMemeber.remove(userId)
    }
    
    func getStatusInfoView(from userID: String?) -> TCStatusInfoComponet? {
        if let userID = userID {
            guard let statusInfoViewArray = statusInfoViewArray else { return nil }
            for statusInfoView in statusInfoViewArray {
                if userID == statusInfoView.userID {
                    return statusInfoView
                }
            }
        }
        return nil
    }
    
    func isNoAnchorINStatusInfoView() -> Bool {
        guard let statusInfoViewArray = statusInfoViewArray else { return true }
        for statusInfoView in statusInfoViewArray {
            if statusInfoView.userID?.count ?? 0 > 0 {
                return false
            }
        }
        return true
    }
    
    /**
     * Notes:
     * 1. `sessionID` is the basis for stream mixing, and streams with the same `sessionID` values will be mixed into one video stream on
        the backend stream mixing server; Therefore, `sessionID` must be globally unique.
     * 2. The live streaming code channel ID is unique. Therefore, it is best practice to use a live streaming code as `sessionID`.
        NSString* strSessionID = [TCLinkMicModel getStreamIDByStreamUrl:self.rtmpUrl];
     * 3. The live streaming code is a string, and the stream mixing server currently supports `sessionID` values containing only 64 digits.
        Generate a `sessionID` as follows:
     */
    func getLinkMicSessionID() -> String {
       
        let timeStamp = UInt64(Date().timeIntervalSince1970 * 1000)
        
        let sessionID = UInt64(3891) << 48 | timeStamp
        
        return String(format: "%llu", sessionID)
        
    }
    
    @objc func onAppWillEnterForeground(_ app: UIApplication) {
        //    [[self.liveRoom getAudioEffectManager] stopPlayMusic:];
    }
    
    @objc func onAppDidEnterBackGround(_ app: UIApplication) {
        UIApplication.shared.beginBackgroundTask(expirationHandler: {
        })
    }
    
    @objc func onAppWillResignActive(_ notification: Notification) {
        if !appIsInterrupt {
            appIsInterrupt = true
        }
    }
    
    @objc func onAppDidBecomeActive(_ notification: Notification?) {
        if appIsInterrupt {
            appIsInterrupt = false
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    public override func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
        guard let logicView = logicView else { return }
        if logicView.isPreview {
            if !roomName.isFirstResponder {
                roomName.becomeFirstResponder()
            }
        }
    }
    
    public override func viewDidDisappear(_ animated: Bool) {
        endTime = UInt64(Date().timeIntervalSince1970)
    }
    
    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(true, animated: false)
    }
    
    @objc func clickBtnKickout(_ btn: UIButton?) {
        guard let statusInfoViewArray = statusInfoViewArray else { return  }
        for statusInfoView in statusInfoViewArray {
            if statusInfoView.btnKickout == btn {
                liveRoom?.stopPlay(userID: statusInfoView.userID ?? "", callback: { code, error in
                })
                liveRoom?.kickoutJoinAnchor(userID: statusInfoView.userID ?? "", callback: { code, error in
                })
                guard let index = setLinkMemeber.firstIndex(of: statusInfoView.userID ?? "") else { return }
                setLinkMemeber.remove(at: index)
                statusInfoView.stopLoading()
                statusInfoView.stopPlay()
                statusInfoView.emptyPlayInfo()
                break
            }
        }
    }
    
    func startPreview() {
#if RTCube_APPSTORE
        let selector = NSSelectorFromString("showAlertUserLiveTips")
        if responds(to: selector) {
            perform(selector)
        }
#endif
        //liveRoom
        guard let liveRoom = liveRoom else { return }
        liveRoom.startCameraPreview(frontCamera: true, view: videoParentView, callback: { code, error in
            if code == 0 {
                UIApplication.shared.isIdleTimerDisabled = true
            }
        })
        liveRoom.delegate = self
    }
    
    func initStatusInfoView(_ index: Int) {
        let width = view.size.width
        let statusInfoView = TCStatusInfoComponet()
        let y = Int(SafeAreaTopHeight + 68) + Int( VIDEO_VIEW_HEIGHT  + VIDEO_VIEW_MARGIN_SPACE) * index
        let videoView = UIView(frame: CGRect(x: Int(CGFloat(Int(CGFloat(Int(width) - VIDEO_VIEW_WIDTH -
         VIDEO_VIEW_MARGIN_RIGHT)))), y:y, width: VIDEO_VIEW_WIDTH, height: VIDEO_VIEW_HEIGHT))
        statusInfoView.setVideoView(videoView)
        let x = Int(width) - VIDEO_VIEW_WIDTH - VIDEO_VIEW_MARGIN_RIGHT
        statusInfoView.linkFrame = CGRect(x:x , y: y, width: VIDEO_VIEW_WIDTH, height: VIDEO_VIEW_HEIGHT)
        if let videoView = statusInfoView.videoView {
            view.addSubview(videoView)
        }
        statusInfoView.pending = false
        statusInfoViewArray?.append(statusInfoView)
        beginTime = UInt64(Date().timeIntervalSince1970)
    }
    
    public init(roomInfo: TRTCLiveRoomInfo) {
        userIdRequest = ""
        liveInfo = roomInfo
        super.init(nibName: nil, bundle: nil)
        liveRoom = nil
        sessionId = getLinkMicSessionID()
        statusInfoViewArray = [TCStatusInfoComponet]()
        setLinkMemeber = Set<String>()
        curPkRoom = nil
        isPKEnter = false
        isSupprotHardware = (Float(UIDevice.current.systemVersion) ?? 0.0 >= 8.0)
       
        camera_switch = false
        log_switch = false
        isStop = false
        notification.notificationLabelBackgroundColor = UIColor.red
        notification.notificationLabelTextColor = UIColor.white
        NotificationCenter.default.addObserver(self, selector: #selector(onAppDidEnterBackGround(_:)),
         name: UIApplication.didEnterBackgroundNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(onAppWillEnterForeground(_:)),
         name: UIApplication.willEnterForegroundNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(onAppWillResignActive(_:)), name:
         UIApplication.willResignActiveNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(onAppDidBecomeActive(_:)), name:
         UIApplication.didBecomeActiveNotification, object: nil)
    }
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        logicView = TCAnchorToolbarView(frame: view.frame)
        LiveRoomToastManager.sharedManager().setupToast()
        //加载背景图
        let colors = [UIColor(red: 19.0 / 255.0, green: 41.0 / 255.0, blue: 75.0 / 255.0, alpha:
         1).cgColor, UIColor(red: 5.0 / 255.0, green: 12.0 / 255.0, blue: 23.0 / 255.0, alpha: 1).cgColor]
        let gradientLayer = CAGradientLayer()
        gradientLayer.colors = colors
        gradientLayer.startPoint = CGPoint(x: 0, y: 0)
        gradientLayer.endPoint = CGPoint(x: 1, y: 1)
        gradientLayer.frame = view.bounds
        view.layer.insertSublayer(gradientLayer, at: 0)
        
        videoParentView = UIView(frame: view.frame)
        view.addSubview(videoParentView)
        //link mic
        initStatusInfoView(0)
        initStatusInfoView(1)
        initStatusInfoView(2)
        guard let logicView = logicView else { return }
        logicView.frame = view.frame
        logicView.delegate = self
        logicView.anchorViewController = self
        logicView.setLiveRoom(liveRoom)
        view.addSubview(logicView)

        let width = view.size.width
        var index = 0
        guard let statusInfoViewArray = statusInfoViewArray else { return  }
        for statusInfoView in statusInfoViewArray {
            let x = width - CGFloat(BOTTOM_BTN_ICON_WIDTH) / 2.0 - CGFloat(VIDEO_VIEW_MARGIN_RIGHT)
            let y = CGFloat(SafeAreaTopHeight) + CGFloat(VIDEO_VIEW_HEIGHT * index) + 68
            statusInfoView.btnKickout = UIButton(frame: CGRect(x:x , y:y, width:
             CGFloat(BOTTOM_BTN_ICON_WIDTH) / 2.0, height: CGFloat(BOTTOM_BTN_ICON_WIDTH) / 2.0))
            guard let btnKickout = statusInfoView.btnKickout else { return }
            btnKickout.addTarget(self, action: #selector(clickBtnKickout(_:)), for: .touchUpInside)
            btnKickout.setImage(UIImage(named: "kickout", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
            btnKickout.isHidden = true
            if let logicView = self.logicView {
                logicView.insertSubview(btnKickout, belowSubview: logicView.btnMusic)
            }
            index += 1
        }
        startPreview()
        logicView.triggeValue()
        initRoomPreview()
        // active widget
        activeTUIWidget()
        TUILogin.add(self)
    }

    func initRoomPreview() {
        publishBtn.backgroundColor = UIColor.appTint
        publishBtn.layer.cornerRadius = 25
        publishBtn.setTitle(liveRoomLocalize("Demo.TRTC.LiveRoom.start"), for: .normal)
        publishBtn.titleLabel?.font = UIFont.systemFont(ofSize: 22)
        view.addSubview(publishBtn)
        publishBtn.width = 160
        publishBtn.height = 50
        publishBtn.center = view.center
        let bottom = IPHONE_X ? 66 : 34
        publishBtn.bottom = view.height - CGFloat(bottom)
        publishBtn.addTarget(self, action: #selector(startPublishVC), for: .touchUpInside)
        
        cameraBtn.setImage(UIImage(named: "live_camera", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        view.addSubview(cameraBtn)
        cameraBtn.snp.makeConstraints({ make in
            make.centerY.equalTo(publishBtn)
            make.centerX.equalTo(view).multipliedBy(0.5).offset(-40)
            make.height.width.equalTo(BOTTOM_BTN_ICON_WIDTH)
        })
        cameraBtn.addTarget(self, action: #selector(clickCamera(_:)), for: .touchUpInside)
        
        beautyBtn.setImage(UIImage(named: "live_beauty", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        view.addSubview(beautyBtn)
        beautyBtn.snp.makeConstraints({ make in
            make.centerY.equalTo(publishBtn)
            make.centerX.equalTo(view).multipliedBy(1.5).offset(40)
            make.height.width.equalTo(BOTTOM_BTN_ICON_WIDTH)
        })
        beautyBtn.addTarget(self, action: #selector(clickBeauty(_:)), for: .touchUpInside)
        
        closeBtn.setImage(UIImage(named: "close", in: liveRoomBundle(), compatibleWith: nil), for: .normal)
        view.addSubview(closeBtn)
        closeBtn.snp.makeConstraints({ make in
            make.top.equalTo(Int(SafeAreaTopHeight) + 20)
            make.trailing.equalTo(-20)
            make.height.width.equalTo(BOTTOM_BTN_ICON_WIDTH)
        })
        closeBtn.addTarget(self, action: #selector(taggleCloseVC), for: .touchUpInside)
        
        createTopPanelBG.backgroundColor = UIColor(hex: "000000")
        createTopPanelBG.alpha = 0.5
        createTopPanelBG.layer.cornerRadius = 6
        view.addSubview(createTopPanelBG)

        createTopPanel.backgroundColor = UIColor.clear
        createTopPanel.layer.cornerRadius = 6
        view.addSubview(createTopPanel)
        
        createTopPanel.left = 20
        createTopPanel.width = view.width - 40.0
        createTopPanel.top = 110
        createTopPanel.height = 163
        createTopPanelBG.frame = createTopPanel.frame
        createTopPanel.addSubview(userAvatar)
        
        userAvatar.layer.masksToBounds = true
        userAvatar.layer.cornerRadius = 10
        userAvatar.height = 52
        userAvatar.width = 52
        userAvatar.layer.cornerRadius = 26
        userAvatar.top = 20
        userAvatar.left = 20
        if let url = URL(string: TUILiveRoomProfileManager.sharedManager().avatar) {
            userAvatar.kf.setImage(with: url, placeholder: nil, options: nil, progressBlock: nil, completionHandler: nil)
        }
        let line = UIView()
        line.backgroundColor = UIColor(hex: "DDDDDD")
        line.alpha = 0.4
        line.top = userAvatar.bottom + 16
        line.height = 1
        line.width = createTopPanel.width - 40
        line.left = 20
        createTopPanel.addSubview(line)
        userName.backgroundColor = UIColor.clear
        userName.textColor = UIColor.white
        userName.font = UIFont.boldSystemFont(ofSize: 18)
        userName.text = TUILiveRoomProfileManager.sharedManager().userId
        createTopPanel.addSubview(userName)
        userName.height = 26
        userName.left = userAvatar.right + 10
        userName.top = userAvatar.top
        userName.sizeToFit()
        
        roomName.backgroundColor = UIColor.clear
        roomName.textColor = UIColor.white
        roomName.returnKeyType = .done
        roomName.font = UIFont.boldSystemFont(ofSize: 14)
        roomName.attributedPlaceholder = NSAttributedString(string: liveRoomLocalize("Demo.TRTC.LiveRoom.titlefuncanattractpopularity"), attributes: [
            NSAttributedString.Key.foregroundColor: UIColor(white: 0.8, alpha: 1)
        ])
        var defaultName = liveInfo.roomName
        if defaultName.isEmpty {
            let uName = TUILogin.getNickName() ?? ""
            defaultName = localizeReplaceXX(liveRoomLocalize("Demo.TRTC.VoiceRoom.xxxsroom"), uName)
        }
        if defaultName.count > 15 {
            defaultName = (defaultName as NSString).substring(to: 15)
        }
        roomName.text = defaultName
        roomName.sizeToFit()
        createTopPanel.addSubview(roomName)
        roomName.height = 32
        roomName.left = userAvatar.right+8
        roomName.top = userName.bottom+4
        roomName.delegate = self
        let audioQualityLabel = UILabel()
        audioQualityLabel.text = liveRoomLocalize("Demo.TRTC.LiveRoom.soundquality")
        audioQualityLabel.font = UIFont.systemFont(ofSize: 16)
        audioQualityLabel.textColor = UIColor.white
        audioQualityLabel.textAlignment = .center
        createTopPanel.addSubview(audioQualityLabel)
        audioQualityLabel.top = line.bottom + 21
        audioQualityLabel.left = userAvatar.left
        audioQualityLabel.sizeToFit()
        standardQualityButton.backgroundColor = audioQualityDefaultColor()
        standardQualityButton.setTitle(liveRoomLocalize("Demo.TRTC.LiveRoom.standard"), for: .normal)
        standardQualityButton.setTitleColor(UIColor.black, for: .normal)
        standardQualityButton.setTitleColor(UIColor.white, for: .selected)
        standardQualityButton.titleLabel?.font = UIFont.systemFont(ofSize: 15)
        standardQualityButton.layer.cornerRadius = 16
        standardQualityButton.addTarget(
            self,
            action: #selector(onAudioQualityButtonClicked(_:)),
            for: .touchUpInside)
        createTopPanel.addSubview(standardQualityButton)
        standardQualityButton.sizeToFit()
        var width = standardQualityButton.width
        standardQualityButton.height = 32
        standardQualityButton.width = 16 + width
        standardQualityButton.center = audioQualityLabel.center
        standardQualityButton.left = audioQualityLabel.right + 90
        musicQualityButton.backgroundColor = audioQualitySelectedColor()
        musicQualityButton.setTitle(liveRoomLocalize("Demo.TRTC.LiveRoom.music"), for: .normal)
        musicQualityButton.titleLabel?.font = UIFont.systemFont(ofSize: 15)
        musicQualityButton.setTitleColor(UIColor.black, for: .normal)
        musicQualityButton.setTitleColor(UIColor.white, for: .selected)
        musicQualityButton.layer.cornerRadius = 16
        musicQualityButton.addTarget(
            self,
            action: #selector(onAudioQualityButtonClicked(_:)),
            for: .touchUpInside)
        musicQualityButton.isSelected = true
        createTopPanel.addSubview(musicQualityButton)
        musicQualityButton.sizeToFit()
        width = musicQualityButton.width
        musicQualityButton.height = 32
        musicQualityButton.width = 16 + width
        musicQualityButton.center = audioQualityLabel.center
        musicQualityButton.left = standardQualityButton.right + 20
        
        if musicQualityButton.right > (createTopPanel.width - 20) {
            musicQualityButton.right = createTopPanel.width - 20
            standardQualityButton.right = musicQualityButton.left - 20
        }
        
        view.addSubview(countdownView)
    }
    
    deinit {
       TUILogin.remove(self)
       debugPrint("TCAnchorViewController deinit")
    }
    
    @objc func onAudioQualityButtonClicked(_ sender: UIButton?) {
        sender?.isSelected = true
        if sender == standardQualityButton {
            musicQualityButton.isSelected = false
            musicQualityButton.backgroundColor = audioQualityDefaultColor()
            standardQualityButton.backgroundColor = audioQualitySelectedColor()
        } else {
            standardQualityButton.isSelected = false
            musicQualityButton.backgroundColor = audioQualitySelectedColor()
            standardQualityButton.backgroundColor = audioQualityDefaultColor()
        }
    }
    
    @objc func taggleCloseVC() {
        if roomName.isFirstResponder {
            roomName.resignFirstResponder()
        }
        closeRTMP()
        closeVC()
    }
    
    @objc func startPublish(roomID: UInt32, roomName: String, callback: @escaping (_ code: Int, _ message: String?) -> Void) {
        if countdownView.isInCountdown {
            return
        }
        countdownView.willDismiss = { [weak self] in
            guard let self = self else { return }
            self.liveRoomCreate(roomName, roomID: roomID) { [weak self] (code, message) in
                guard let self = self else { return }
                TUILiveRoomProfileManager.sharedManager().createRoom(roomID: String(roomID)) { [weak self] in
                    guard let self = self else { return }
                    self.logicView?.topView.setRoomId(String(roomID))
                    callback(code,message)
                } failed: { [weak self] (code, message) in
                    guard let self = self else { return }
                    if code == -1301 {
                        self.logicView?.topView.setRoomId(String(roomID))
                        callback(0, "success")
                    } else {
                        self.previewUIHidden(hide: false)
                        callback(-1, message)
                    }
                }
            }
        }
        previewUIHidden(hide: true)
        countdownView.start()
    }
    
    func liveRoomCreate(_ roomName: String, roomID: UInt32, callback: @escaping (_ code: Int, _ message: String?) -> Void) {
        let roomParam = TRTCCreateRoomParam(roomName: roomName, coverUrl: TUILiveRoomProfileManager.sharedManager().avatar)
        liveRoom?.createRoom(roomID: roomID, roomParam: roomParam, callback: { [weak self] code, message in
            guard let self = self else { return }
            if code == 0 {
                let roomInfo = TRTCLiveRoomInfo(roomId: String(roomID),
                                                roomName: roomName,
                                                coverUrl: self.liveInfo.coverUrl,
                                                ownerId: TUILogin.getUserID() ?? "",
                                                ownerName: (TUILogin.getNickName() ?? ""),
                                                streamUrl: TUILogin.getUserID(),
                                                memberCount: 0,
                                                roomStatus: .single)
                self.setLive(roomInfo)
            }
            callback(Int(code),message)
        })
    }
    
    func setLive(_ liveInfo: TRTCLiveRoomInfo?) {
        self.liveInfo = liveInfo!
        logicView?.setLive(self.liveInfo)
    }
    func previewUIHidden(hide: Bool) {
        publishBtn.isHidden = hide
        cameraBtn.isHidden = hide
        beautyBtn.isHidden = hide
        closeBtn.isHidden = hide
        createTopPanel.isHidden = hide
        createTopPanelBG.isHidden = hide
    }
    
    func stopRtmp() {
        if !isStop {
            isStop = true
        } else {
            return
        }
        guard let liveRoom = liveRoom else {
            return
        }
        liveRoom.delegate = nil
        NotificationCenter.default.removeObserver(self)
        let roomID = liveInfo.roomId
        liveRoom.stopCameraPreview()
        liveRoom.stopPublish(callback: { code, error in
            
        })
        TUILiveRoomProfileManager.sharedManager().destroyRoom(roomID: roomID, success: {
        }, failed: { code, error in
            print("\(code),\(error)")
        })
        liveRoom.destroyRoom(callback: { code, error in
            
        })
        liveRoom.showVideoDebugLog(false)
        UIApplication.shared.isIdleTimerDisabled = false
    }
    
    @objc func startPublishVC() {
        roomName.text = TCUtil.subString(roomName.text ?? "", length: 30)
        if roomName.isFirstResponder {
            roomName.resignFirstResponder()
        }
        if roomName.text?.count == 0 {
            LiveRoomToastManager.sharedManager().makeToast(view: view, message: liveRoomLocalize("Demo.TRTC.LiveRoom.roomnamecantbeempty"))
            return
        }
        guard let roomId = UInt32(liveInfo.roomId) else {
            LiveRoomToastManager.sharedManager().makeToast(view: view, message: "room ID is error")
            return
        }
        startPublish(roomID: roomId, roomName: roomName.text ?? "") { [weak self] code, message in
            guard let `self` = self else { return }
            if code == 0 {
                self.previewUIHidden(hide: true)
                let streamID = "\(TUILiveRoomProfileManager.sharedManager().userId)_stream"
                if self.standardQualityButton.isSelected {
                    self.liveRoom?.setAudioiQuality(2)
                } else {
                    self.liveRoom?.setAudioiQuality(3)
                }
                self.liveRoom!.startPublish(streamID: streamID) { code, error in
                }
            } else {
                let mess = message?.count ?? 0>0 ? message : liveRoomLocalize("Demo.TRTC.LiveRoom.createroomfailed")
                LiveRoomToastManager.sharedManager().makeToast(view: self.view, message: mess ?? "")
                DispatchQueue.main.asyncAfter(deadline: DispatchTime.now() + 1.5, execute: { [weak self] in
                    guard let `self` = self else { return }
                    self.stopRtmp()
                    self.closeVC()
                })
            }
            
        }
    }
    
    func onAnchorEnter(_ userID: String?) {
        if userID == liveInfo.ownerId {
            return
        }
        if userID == nil || (userID?.count ?? 0) == 0 {
            return
        }
        let isPKMode = curPkRoom != nil && (userID == curPkRoom?.ownerId)
        if isPKMode {
            isPKEnter = true
        }
        if let statusInfoViewArray = statusInfoViewArray {
            for statusInfoView in statusInfoViewArray {
                if userID == statusInfoView.userID {
                    if statusInfoView.pending {
                        statusInfoView.pending = false
                        self.liveRoom!.startPlay(userID:userID!, view: statusInfoView.videoView!, callback: {[weak self] code, message in
                            guard let `self` = self else { return }
                            statusInfoView.stopLoading()
                            if code == 0 {
                                statusInfoView.btnKickout?.isHidden = isPKMode
                            } else {
                                if !isPKMode, let userID = userID {
                                    self.liveRoom?.kickoutJoinAnchor(userID: userID, callback: { code, error in
                                        
                                    })
                                    self.onAnchorExit(userID)
                                }
                            }
                        })
                    }
                    break
                }
            }
        }
    }
    
    @objc func handleTimeOutRequest(_ sender: Any?) {
        userIdRequest = ""
        TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.dealmicconnectionreqtimeout"), parentView: view)
    }
    
    @objc func onLinkMicTimeOut(_ userID: String?) {
        if let userID = userID {
            if let statusInfoView = self.getStatusInfoView(from: userID)  {
                if statusInfoView.pending == true {
                    if let liveRoom = self.liveRoom {
                        liveRoom.kickoutJoinAnchor(userID: userID) { code, message in
                        }
                    }
                    setLinkMemeber .remove(at: setLinkMemeber.firstIndex(of: userID)!)
                    statusInfoView.stopPlay()
                    statusInfoView.emptyPlayInfo()
                    TCUtil.toastTip(localizeReplaceXX(liveRoomLocalize("Demo.TRTC.LiveRoom.xxmicconnectiontimeout"), userID), parentView: view)
                }
            }
        }
    }
    
    // MARK:- LinkMic Func
    func onRequestJoinAnchor(_ user: TRTCLiveUserInfo, reason: String) {
        if setLinkMemeber.count >= MAX_LINKMIC_MEMBER_SUPPORT {
            TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.micconnectionrefusedandanchorpeopleexceedsmaxlimit"), parentView: view)
            liveRoom?.responseJoinAnchor(userID: user.userId, agree: false, reason:
             liveRoomLocalize("Demo.TRTC.LiveRoom.anchorpeopleexceedsmaxlimit"))
        } else if userIdRequest.count > 0 {
            if userIdRequest != user.userId {
                TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.micconnectionrefusedandanchordealotherreq"), parentView: view)
                liveRoom?.responseJoinAnchor(userID: user.userId, agree: false, reason:
                 liveRoomLocalize("Demo.TRTC.LiveRoom.waitforhandleotherreq"))
            }
        } else if curPkRoom != nil {
            TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.micconnectionrefusedandinpk"), parentView: view)
            liveRoom?.responseJoinAnchor(userID: user.userId, agree: false, reason:
             liveRoomLocalize("Demo.TRTC.LiveRoom.micconnectionrefusedandinpk"))
        } else {
            let statusInfoView = getStatusInfoView(from: user.userId)
            if let statusInfoView = statusInfoView {
                liveRoom?.kickoutJoinAnchor(userID: user.userId, callback: { code, error in
                    
                })
                setLinkMemeber.remove(at: setLinkMemeber.firstIndex(of: user.userId)! )
                statusInfoView.stopLoading()
                statusInfoView.stopPlay()
                statusInfoView.emptyPlayInfo()
                
            }
            userIdRequest = user.userId
            curRequest = user
            
            let alert = UIAlertController(title: liveRoomLocalize("Demo.TRTC.LiveRoom.prompt"), message:
             localizeReplaceXX(liveRoomLocalize("Demo.TRTC.LiveRoom.xxinitiateamicconnectionreq"),
             user.userName), preferredStyle: .alert)
            let cancel = UIAlertAction(title: liveRoomLocalize("Demo.TRTC.LiveRoom.refuse"), style: .cancel, handler: { [weak self] action in
                guard let `self` = self else { return }
                self.userIdRequest = ""
                self.liveRoom?.responseJoinAnchor(userID: self.curRequest.userId, agree: false, reason:
                 liveRoomLocalize("Demo.TRTC.LiveRoom.refusemicconnectionreq"))
            })
            
            let other = UIAlertAction(title: liveRoomLocalize("Demo.TRTC.LiveRoom.accept"), style: .default, handler: { [weak self] action in
                guard let `self` = self else { return }
                if self.userIdRequest.count <= 0 {
                    return
                }
                self.liveRoom?.responseJoinAnchor(userID: self.curRequest.userId, agree: true, reason: "")
                guard let statusInfoViewArray = self.statusInfoViewArray else {
                    return
                }
                for statusInfoView in statusInfoViewArray {
                    if statusInfoView.userID == nil || statusInfoView.userID?.count == 0 {
                        statusInfoView.pending = true
                        statusInfoView.userID = self.userIdRequest
                        statusInfoView.startLoading()
                        break
                    }
                }
                self.setLinkMemeber.insert(self.userIdRequest)
                self.userIdRequest = ""
            })
            
            alert.addAction(cancel)
            alert.addAction(other)
            joinAnchor = alert
            present(alert, animated: true)
        }
    }
    
    func setCurPkRoom(_ curPkRoom: TRTCLiveRoomInfo?) {
        self.curPkRoom = curPkRoom
        if self.curPkRoom == nil {
            isPKEnter = false
            logicView?.btnPK.isSelected = false
        }
    }
    
    func switchPKMode() {
        let info = getStatusInfoView(from: curPkRoom?.ownerId)
        if info == nil {
            guard let statusInfoViewArray = statusInfoViewArray else {
                return
            }
            for statusInfoView in statusInfoViewArray {
                if statusInfoView.userID == nil || statusInfoView.userID?.count == 0 {
                    statusInfoView.videoView?.frame = CGRect(x: view.frame.size.width / 2, y: 0, width:
                     view.frame.size.width / 2, height: view.frame.size.height / 2)
                    statusInfoView.pending = true
                    statusInfoView.userID = curPkRoom?.ownerId
                    statusInfoView.stopLoading()
                    break
                }
            }
        } else {
            guard let info = info else {
                return
            }
            info.videoView?.frame = CGRect(x: view.frame.size.width / 2, y: 0, width: view.frame.size.width / 2, height: view.frame.size.height / 2)
        }
    }
    
    func onRequestRoomPK(_ user: TRTCLiveUserInfo) {
        curPkRoom = TRTCLiveRoomInfo()
        curPkRoom?.ownerId = user.userId
        curPkRoom?.ownerName = user.userName
        let alert = UIAlertController(title: localizeReplaceXX(liveRoomLocalize("Demo.TRTC.LiveRoom.xxinitiatepk"), user.userName),
             message: nil, preferredStyle: .alert)
        let reject = UIAlertAction(title: liveRoomLocalize("Demo.TRTC.LiveRoom.refuse"), style: UIAlertAction.Style.cancel ) {[weak self] action in
            guard let `self` = self else { return }
            
            self.linkFrameRestore()
            self.liveRoom?.responseRoomPK(userID: user.userId, agree: false, reason: liveRoomLocalize("Demo.TRTC.LiveRoom.anchorrefuse"))
            
        }
        
        let ok = UIAlertAction(title: liveRoomLocalize("Demo.TRTC.LiveRoom.accept"), style: UIAlertAction.Style.default ) { [weak self] action in
            guard let `self` = self else { return }
            
            self.liveRoom?.responseRoomPK(userID: user.userId, agree: true, reason: "")
            self.logicView?.btnPK.isSelected = true
            
        }
        alert.addAction(reject)
        alert.addAction(ok)
        pkalert = alert
        present(alert, animated: true)
    }
    
    @objc func pkAlertCheck(_ alert: UIAlertController?) {
        alert?.dismiss(animated: true)
        TCUtil.toastTip(localizeReplaceXX(liveRoomLocalize("Demo.TRTC.LiveRoom.dealxxpktimeout"), curPkRoom?.ownerName ?? ""), parentView: view)
        linkFrameRestore()
    }
    
    // MARK: PK
    func linkFrameRestore() {
        guard let statusInfoViewArray = statusInfoViewArray else {
            return
        }
        for statusInfoView in statusInfoViewArray {
            if statusInfoView.userID?.count ?? 0 > 0 {
                statusInfoView.videoView?.frame = statusInfoView.linkFrame
            }
        }
        liveRoom?.stopPlay(userID: curPkRoom?.ownerId ?? "", callback: { code, error in
            
        })
        self.setCurPkRoom(nil)
    }
    
}

// MARK: - TUILoginListener
extension TCAnchorViewController: TUILoginListener {
    
    public func onConnecting() {
        
    }
    
    public func onConnectSuccess() {
        
    }
    
    public func onConnectFailed(_ code: Int32, err: String!) {
        
    }
    
    public func onKickedOffline() {
        taggleCloseVC()
    }
    
    public func onUserSigExpired() {
        
    }
    
}

// MARK: - UITextFieldDelegate
extension TCAnchorViewController: UITextFieldDelegate {
    
    public func textFieldShouldReturn(_ textField: UITextField) -> Bool {
        if textField == roomName {
            self.startPublishVC()
        }
        return true
    }
    
}
 
// MARK: - TRTCLiveRoomDelegate
extension TCAnchorViewController: TRTCLiveRoomDelegate {

    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onRoomInfoChange info: TRTCLiveRoomInfo) {
        roomStatus = info.roomStatus
        liveInfo.roomStatus = roomStatus
        switch info.roomStatus {
        case TRTCLiveRoomLiveStatus.single,TRTCLiveRoomLiveStatus.linkMic :
            UIView.animate(withDuration: 0.1, animations: { [weak self] in
                guard let `self` = self else { return }
                self.videoParentView.frame = self.view.frame
                self.linkFrameRestore()
            })
            break
        case TRTCLiveRoomLiveStatus.roomPK:
            UIView.animate(withDuration: 0.1, animations: { [weak self] in
                guard let `self` = self else { return }
                self.videoParentView.frame = CGRect(x: 0, y: 0, width: self.view.frame.size.width * 0.5, height: self.view.frame.size.height * 0.5)
                self.switchPKMode()
            })
            break
        default:
            break
        }
    }
    
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
        onAnchorEnter(userID)
    }
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onAnchorExit userID: String) {
        onAnchorExit(userID)
    }
    
    public  func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onAudienceEnter user: TRTCLiveUserInfo) {
        var info = IMUserAble()
        info.imUserId = user.userId
        info.imUserName = user.userName
        info.imUserIconUrl = user.avatarURL
        info.cmdType = .memberEnterRoom
        logicView?.handleIMMessage(info, msgText: "")
    }
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, anchorRequestJoinAnchorTimeout userID: String) {
        self.pkAlertCheck(pkalert)
    }
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, audienceRequestJoinAnchorTimeout userID: String) {
        self.joinAnchor?.dismiss(animated: true)
        self.onLinkMicTimeOut(userIdRequest)
        self.handleTimeOutRequest(nil)
        
    }
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onRequestJoinAnchor user: TRTCLiveUserInfo, reason: String?) {
        onRequestJoinAnchor(user, reason: reason ?? "")
    }
    
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onRequestRoomPK user: TRTCLiveUserInfo) {
        if userIdRequest.count > 0 {
            self.liveRoom?.responseRoomPK(userID: user.userId, agree: false, reason: liveRoomLocalize("Demo.TRTC.LiveRoom.anchorismicconnecting"))
        } else {
            onRequestRoomPK(user)
        }
    }
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onCancelRoomPK user: TRTCLiveUserInfo) {
        pkalert?.dismiss(animated: true)
        self.setCurPkRoom(nil)
        TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.cancelpk"), parentView: view)
        linkFrameRestore()
    }
    
    public func trtcLiveRoomOnQuitRoomPK(_ liveRoom: TRTCLiveRoom) {
        pkalert?.dismiss(animated: true)
        self.setCurPkRoom(nil)
        TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.opponentanchorendpd"), parentView: view)
        linkFrameRestore()
    }
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onAudienceExit user: TRTCLiveUserInfo) {
        var info = IMUserAble()
        info.imUserId = user.userId
        info.imUserName = user.userName
        info.imUserIconUrl = user.avatarURL
        info.cmdType = TCMsgModelType.memberQuitRoom
        guard let logicView = logicView else { return }
        logicView.handleIMMessage(info, msgText: "")
        if user.userId == self.userIdRequest{
            self.userIdRequest = ""
        }
    }
    
    public func trtcLiveRoom(_ trtcLiveRoom: TRTCLiveRoom, onRoomDestroy roomID: String) {
        taggleCloseVC()
#if RTCube_APPSTORE
        let selector = NSSelectorFromString("showAlertUserLiveTimeOut")
        if UIViewController.responds(to: selector) {
            UIViewController.perform(selector)
        }
#endif
    }
}

// MARK: - TRTCVideoFrameDelegate
extension TCAnchorViewController: TRTCVideoFrameDelegate {
    
    public func onProcessVideoFrame(_ srcFrame: TRTCVideoFrame, dstFrame: TRTCVideoFrame) -> UInt32 {
        if let dstTextureId = TUICore.callService(TUICore_TUIBeautyService,
                                                  method: TUICore_TUIBeautyService_ProcessVideoFrame,
                                                  param: [
                                                      TUICore_TUIBeautyService_ProcessVideoFrame_SRCTextureIdKey: srcFrame.textureId,
                                                      TUICore_TUIBeautyService_ProcessVideoFrame_SRCFrameWidthKey: srcFrame.width,
                                                      TUICore_TUIBeautyService_ProcessVideoFrame_SRCFrameHeightKey: srcFrame.height
                                                         ]) as? GLuint {
            dstFrame.textureId = dstTextureId
        }
        return 0
    }
    
}

// MARK: - Load Widget
extension TCAnchorViewController {
    
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
        if giftView == nil && loadGiftWidget(), let giftView = giftView {
            view.addSubview(giftView)
            giftView.isHidden = false
        }
        if audioEffectView == nil && loadAudioEffectWidget(), let audioEffectView = audioEffectView {
            view.addSubview(audioEffectView)
            audioEffectView.snp.makeConstraints({ make in
                make.edges.equalToSuperview()
            })
        }
        if beautyView == nil && loadBeautyWidget(), let beautyView = beautyView {
            view.addSubview(beautyView)
            beautyView.snp.makeConstraints({ make in
                make.edges.equalToSuperview()
            })
        }
    }
    
    private func loadAudioEffectWidget() -> Bool {
        guard let audioEffectManager = liveRoom?.getAudioEffectManager() else {
            return false
        }
        let audioEffectViewList = TUICore.getExtensionList(TUICore_TUIAudioEffectViewExtension_AudioEffectView,
                                                           param: [
                                                            TUICore_TUIAudioEffectViewExtension_AudioEffectView_AudioEffectManager:
                                                                    audioEffectManager,])
        
        if audioEffectViewList.count == 0 {
            return false
        }
        guard let audioEffectView = audioEffectViewList[0].data?[TUICore_TUIAudioEffectViewExtension_AudioEffectView_View] as? UIView else {
              return false
        }
        self.audioEffectView = audioEffectView
        return true
    }
    
    private func loadBarrageWidget() -> Bool {
        let inputViewList = TUICore.getExtensionList(TUICore_TUIBarrageExtension_GetTUIBarrageSendView,
                                                     param: ["frame": NSCoder.string(for: UIScreen.main.bounds),
                                                             "groupId": liveInfo.roomId])
        
        if inputViewList.count == 0 {
            return false
        }
        guard let inputView = inputViewList[0].data?[TUICore_TUIBarrageExtension_GetTUIBarrageSendView] as? UIView else {
            return false
        }
        self.barrageInputView = inputView
        
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
        let giftPlayList = TUICore.getExtensionList(TUICore_TUIGiftExtension_GetTUIGiftPlayView,
                                                    param: ["frame": NSCoder.string(for: UIScreen.main.bounds),
                                                            "groupId": liveInfo.roomId])
        
        if giftPlayList.count == 0 {
            return false
        }
        guard let giftView = giftPlayList[0].data?[TUICore_TUIGiftExtension_GetTUIGiftPlayView] as? UIView else {
            return false
        }
        self.giftView = giftView
        return true
    }
    
    private func loadBeautyWidget() -> Bool {
        if !xMagicLicenseURL.isEmpty, !xMagicLicenseKey.isEmpty {
            TUICore.callService(TUICore_TUIBeautyService,
                                method: TUICore_TUIBeautyService_SetLicense,
                                param: [
                                    TUICore_TUIBeautyExtension_BeautyView_LicenseUrl: xMagicLicenseURL,
                                    TUICore_TUIBeautyExtension_BeautyView_LicenseKey: xMagicLicenseKey])
        }
        guard let beautyManager = liveRoom?.getBeautyManager() else {
            return false
        }
        let beautyList = TUICore.getExtensionList(TUICore_TUIBeautyExtension_BeautyView,
                                                  param: [TUICore_TUIBeautyExtension_BeautyView_BeautyManager: beautyManager])
        
        if beautyList.count == 0 {
            return false
        }
        guard let view = beautyList[0].data?[TUICore_TUIBeautyExtension_BeautyView_View] as? UIView else {
            return false
        }
        beautyView = view
        TRTCCloud.sharedInstance().setLocalVideoProcessDelegete(self, pixelFormat: ._Texture_2D, bufferType: .texture)
        return true
    }

}

// MARK: - TCAnchorToolbarDelegate
extension TCAnchorViewController: TCAnchorToolbarDelegate {
    
    func closeRTMP() {
        self.previewUIHidden(hide: true)
        guard let statusInfoViewArray = statusInfoViewArray else { return }
        for statusInfoView in statusInfoViewArray {
            if let userID = statusInfoView.userID {
                let a = setLinkMemeber.contains(userID)
                if a {
                    liveRoom?.kickoutJoinAnchor(userID: userID, callback: { code, error in
                        
                    })
                }
            }
            statusInfoView.stopPlay()
        }
        if curPkRoom != nil {
            quitPK()
        }
        stopRtmp()
    }
    
    func closeVC() {
        beautyView?.removeFromSuperview()
        beautyView = nil
        TRTCCloud.sharedInstance().setLocalVideoProcessDelegete(nil, pixelFormat: ._Texture_2D, bufferType: .texture)
        if let isRoot = navigationController?.viewControllers.first?.isEqual(self), isRoot {
            dismiss(animated: true)
        } else {
            navigationController?.popViewController(animated: true)
        }
        TUILiveRoom.sharedInstance.isEnterRoom = false
    }
    
    func clickScreen(_ gestureRecognizer: UITapGestureRecognizer?) {
        guard let logicView = logicView else { return }
        if logicView.isPreview {
            if roomName.isFirstResponder {
                roomName.resignFirstResponder()
            }
            if !isStop {
                self.previewUIHidden(hide: false)
            }
            return
        }
        logicView.setButtonHidden(false)
    }
    
    @objc func clickCamera(_ button: UIButton?) {
        camera_switch = !camera_switch
        guard let liveRoom = liveRoom else { return }
        liveRoom.switchCamera()
    }
    
    @objc func clickBeauty(_ button: UIButton?) {
        guard let beautyView = self.beautyView else { return }
        view.bringSubviewToFront(beautyView)
        beautyView.isHidden = false
    }
    
    func clickMusic(_ button: UIButton?) {
        guard let audioEffectView = self.audioEffectView else { return }
        view.bringSubviewToFront(audioEffectView)
        audioEffectView.isHidden = false
    }
    
    func clickChat(_ button: UIButton?) {
        guard let barrageInputView = self.barrageInputView else { return }
        view.bringSubviewToFront(barrageInputView)
        barrageInputView.isHidden = false
    }
    
    func clickPK(_ button: UIButton?) {
        guard let logicView = self.logicView else {
            return
        }
        if logicView.btnPK.isSelected {
            guard let roomId = UInt32(liveInfo.roomId) else {
                return
            }
            guard let curPkRoom = curPkRoom else { return }
            if roomStatus == .roomPK {
                liveRoom?.quitRoomPK(callback: { (code, msg) in
                    debugPrint("quitRoomPK status:\(code) msg:\(msg ?? "")")
                })
            } else {
                liveRoom?.cancelRoomPK(roomID: roomId, userID: curPkRoom.ownerId, responseCallback: { (code, msg) in
                    debugPrint("cancelRoomPK status:\(code) msg:\(msg ?? "")")
                })
            }
            logicView.btnPK.isSelected = false
        } else {
            view.bringSubviewToFront(logicView)
            logicView.vPKPanel.transform = .identity
            logicView.vPKPanel.isHidden = false
            logicView.vPKPanel.loadRoomsInfo()
        }
    }
    
    func pk(withRoom room: TRTCLiveRoomInfo?) {
        if setLinkMemeber.count > 0 {
            TCUtil.toastTip(liveRoomLocalize("Demo.TRTC.LiveRoom.micconnectingandwaitforpk"), parentView: view)
            return
        }
        guard let room = room else { return }
        logicView?.btnPK.isSelected = true
        liveRoom?.requestRoomPK(roomID: UInt32(room.roomId) ?? 0, userID: room.ownerId, timeout: trtcLiveSendMsgTimeOut) {[weak self] accept, error in
            guard let self = self else { return }
            if accept {
                TCUtil.toastTip(localizeReplaceXX(liveRoomLocalize("Demo.TRTC.LiveRoom.xxacceptpkreq"), room.ownerName), parentView: self.view)
                self.logicView?.btnPK.isSelected = true
            } else {
                if error?.count ?? 0 > 0 {
                    TCUtil.toastTip(error, parentView: self.view)
                } else {
                    TCUtil.toastTip(localizeReplaceXX(liveRoomLocalize("Demo.TRTC.LiveRoom.xxrefusepkreq"), room.ownerName), parentView: self.view)
                }
                let status = self.roomStatus
                self.logicView?.btnPK.isSelected = false
                if TRTCLiveRoomLiveStatus.roomPK != status {
                    self.setCurPkRoom(nil)
                }
            }
        }
        let status = self.roomStatus
        if TRTCLiveRoomLiveStatus.roomPK != status {
            self.setCurPkRoom(room)
        }
    }
    
    func clickLog() {
        log_switch = !log_switch
        liveRoom?.showVideoDebugLog(log_switch)
    }
    
}
