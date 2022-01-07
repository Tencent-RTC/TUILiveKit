//
//  TCAnchorToolbarView.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/28.
//

import Foundation

typealias ShowResultComplete = () -> Void

protocol TCAnchorToolbarDelegate: NSObjectProtocol {
    func closeRTMP()
    func closeVC()
    func clickScreen(_ gestureRecognizer: UITapGestureRecognizer?)
    func clickCamera(_ button: UIButton?)
    func clickBeauty(_ button: UIButton?)
    func clickMusic(_ button: UIButton?)
    func clickPK(_ button: UIButton?)
    func pk(withRoom room: TRTCLiveRoomInfo?)
    func clickLog()
    func clickMusicSelect(_ button: UIButton?)
    func clickMusicClose(_ button: UIButton?)
    func clickVolumeSwitch(_ button: UIButton?)
    func sliderValueChange(_ slider: UISlider?)
    func sliderValueChangeEx(_ slider: UISlider?)
    func selectEffect(_ index: Int)
    func selectEffect2(_ index: Int)
    func motionTmplSelected(_ mid: String?)
    func greenSelected(_ mid: URL?)
    func filterSelected(_ index: Int)
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
        titleLabel.text = LiveRoomLocalize("Demo.TRTC.LiveRoom.interactionitsover")
        addSubview(titleLabel)
        durationLabel.textAlignment = .center
        durationLabel.font = UIFont.boldSystemFont(ofSize: 15)
        durationLabel.textColor = UIColor.black
        durationLabel.text = String(format: "%02d:%02d:%02d", hour, min, sec)
        addSubview(durationLabel)
        durationTipLabel.textAlignment = .center
        durationTipLabel.font = UIFont.boldSystemFont(ofSize: 12)
        durationTipLabel.textColor = UIColor.gray
        durationTipLabel.text = LiveRoomLocalize("Demo.TRTC.LiveRoom.interactionduration")
        addSubview(durationTipLabel)
        viewerCountLabel.textAlignment = .center
        viewerCountLabel.font = UIFont.boldSystemFont(ofSize: 12)
        viewerCountLabel.textColor = UIColor.black
        viewerCountLabel.text = String(format: "%ld", resultData!.getTotalViewerCount())
        addSubview(viewerCountLabel)
        viewerCountTipLabel.textAlignment = .center
        viewerCountTipLabel.font = UIFont.boldSystemFont(ofSize: 12)
        viewerCountTipLabel.textColor = UIColor.gray
        viewerCountTipLabel.text = LiveRoomLocalize("Demo.TRTC.LiveRoom.viewers")
        addSubview(viewerCountTipLabel)
        praiseLabel.textAlignment = .center
        praiseLabel.font = UIFont.boldSystemFont(ofSize: 12)
        praiseLabel.textColor = UIColor.black
        praiseLabel.text = String(format: "%ld\n", resultData!.getLikeCount())
        addSubview(praiseLabel)
        praiseTipLabel.textAlignment = .center
        praiseTipLabel.font = UIFont.boldSystemFont(ofSize: 12)
        praiseTipLabel.textColor = UIColor.gray
        praiseTipLabel.text = LiveRoomLocalize("Demo.TRTC.LiveRoom.numberoflikes")
        addSubview(praiseTipLabel)
        line.backgroundColor = UIColor(hex: "EEEEEE")
        addSubview(line)
        backBtn.backgroundColor = UIColor.clear
        backBtn.titleLabel?.font = UIFont.systemFont(ofSize: 16)
        backBtn.setTitle(LiveRoomLocalize("Demo.TRTC.LiveRoom.back"), for: .normal)
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
    var msgTableView: TCMsgListTableView?
    var bulletViewOne: TCMsgBarrageView?
    var bulletViewTwo: TCMsgBarrageView?
    var liveInfo: TRTCLiveRoomInfo?
    var clearView: UIView?
    var msgInputView: UIView = UIView()
    var msgInputFeild: UITextField = UITextField()
    var closeBtn: UIButton = UIButton(type: .custom)
    var pkView: UIImageView?
    var touchBeginLocation = CGPoint.zero
    var bulletBtnIsOn = false
    var closeAlert: UIAlertController?
    var labVolumeForVoice: UILabel = UILabel()
    var sldVolumeForVoice: UISlider = UISlider()
    var labVolumeForBGM: UILabel = UILabel()
    var sldVolumeForBGM: UISlider = UISlider()
    var labPositionForBGM: UILabel = UILabel()
    var sldPositionForBGM: UISlider = UISlider()
    var btnSelectBGM: UIButton = UIButton(type: .custom)
    var btnStopBGM: UIButton = UIButton(type: .custom)
    var vBGMPanel: UIView  = UIView()
    var vAudioEffectPanel: UIView = UIView()
    var audioEffectSelectedType = 0
    var audioEffectSelectedType2 = 0
    var isTouchMusicPanel = false
    var viewsHidden = false
    var heartAnimationPoints: [CGPoint] = [CGPoint]()
    let moreSettingVC = TRTCLiveRoomMoreControllerUI()
    lazy var topView: TCShowLiveTopView = {
        let statusBarHeight = Int(UIApplication.shared.statusBarFrame.size.height)
        var topView = TCShowLiveTopView(
            frame: CGRect(x: 5, y: statusBarHeight + 5, width: 180, height: 48),
            isHost: true,
            hostNickName: self.liveInfo?.ownerName ?? "",
            audienceCount: self.liveInfo?.memberCount ?? 0,
            likeCount: 0,
            hostFaceUrl: self.liveInfo?.streamUrl ?? "")
        return topView
    }()
    var audioEffectArry: [String] = {
        var audioEffectArry = [LiveRoomLocalize("Demo.TRTC.LiveRoom.originalsound"), LiveRoomLocalize("Demo.TRTC.LiveRoom.ktv"), LiveRoomLocalize("Demo.TRTC.LiveRoom.room"), LiveRoomLocalize("Demo.TRTC.LiveRoom.hall"), LiveRoomLocalize("Demo.TRTC.LiveRoom.muffled"), LiveRoomLocalize("Demo.TRTC.LiveRoom.sonorous"), LiveRoomLocalize("Demo.TRTC.LiveRoom.metal"), LiveRoomLocalize("Demo.TRTC.LiveRoom.magnetic")]
        return audioEffectArry
    }()
    lazy private var audioEffectViewArry : [UIButton]  = {
        var audioEffectViewArry = [UIButton]()
        return audioEffectViewArry
    }()
    lazy  private var audioEffectArry2: [String] = {
        var audioEffectArry2 = [
            LiveRoomLocalize("Demo.TRTC.LiveRoom.originalsound"),
            LiveRoomLocalize("Demo.TRTC.LiveRoom.child"),
            LiveRoomLocalize("Demo.TRTC.LiveRoom.loli"),
            LiveRoomLocalize("Demo.TRTC.LiveRoom.uncle"),
            LiveRoomLocalize("Demo.TRTC.LiveRoom.heavymetal"),
            LiveRoomLocalize("Demo.TRTC.LiveRoom.catarrh"),
            LiveRoomLocalize("Demo.TRTC.LiveRoom.foreigner"),
            LiveRoomLocalize("Demo.TRTC.LiveRoom.sleepybeast"),
            LiveRoomLocalize("Demo.TRTC.LiveRoom.Otaku"),
            LiveRoomLocalize("Demo.TRTC.LiveRoom.strongcurrent"),
            LiveRoomLocalize("Demo.TRTC.LiveRoom.heavymachinery"),
            LiveRoomLocalize("Demo.TRTC.LiveRoom.ethereal"),
        ]
        return audioEffectArry2
    }()// 变声
    lazy var audioEffectViewArry2: [UIButton]  = {
        var audioEffectViewArry = [UIButton]()
        return audioEffectViewArry
    }()
    lazy private var tap: UITapGestureRecognizer = {
        let tap = UITapGestureRecognizer(target: self, action: #selector(clickScreen(_:)))
        return tap
    }()
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardFrameDidChange(_:)), name: UIResponder.keyboardWillChangeFrameNotification, object: nil)
        addGestureRecognizer(tap)
        audioEffectSelectedType = 0
        audioEffectSelectedType2 = 0
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
        //topview,展示主播头像，在线人数及点赞
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
        //观众列表
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
        //聊天
        btnChat.setBackgroundImage(UIImage(named: "comment", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
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
        
        //前置后置摄像头切换
        btnCamera.setImage(UIImage(named: "live_camera", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        btnCamera.addTarget(self, action: #selector(clickCamera(_:)), for: .touchUpInside)
        addSubview(btnCamera)
        btnCamera.snp.makeConstraints({ make in
            make.leading.equalTo(btnChat.snp.trailing).offset(margin)
            make.size.centerY.equalTo(btnChat)
        })
        
        btnPK.setImage(UIImage(named: "live_pk_start", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        btnPK.addTarget(self, action: #selector(clickPK(_:)), for: .touchUpInside)
        addSubview(btnPK)
        btnPK.snp.makeConstraints({ make in
            make.leading.equalTo(btnCamera.snp.trailing).offset(margin)
            make.size.centerY.equalTo(btnChat)
        })
        
        //美颜开关按钮
        btnBeauty.setImage(UIImage(named: "live_beauty", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        btnBeauty.addTarget(self, action: #selector(clickBeauty(_:)), for: .touchUpInside)
        addSubview(btnBeauty)
        btnBeauty.snp.makeConstraints({ make in
            make.leading.equalTo(btnPK.snp.trailing).offset(margin)
            make.size.centerY.equalTo(btnChat)
        })
        //音乐按钮
        btnMusic.setImage(UIImage(named: "music_icon", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        btnMusic.addTarget(self, action: #selector(clickMusic(_:)), for: .touchUpInside)
        addSubview(btnMusic)
        btnMusic.snp.makeConstraints({ make in
            make.leading.equalTo(btnBeauty.snp.trailing).offset(margin)
            make.size.centerY.equalTo(btnChat)
        })
        
        setting.setImage(UIImage(named: "live_more", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        setting.addTarget(self, action: #selector(clickSetting(_:)), for: .touchUpInside)
        addSubview(setting)
        setting.snp.makeConstraints({ make in
            make.leading.equalTo(btnMusic.snp.trailing).offset(margin)
            make.size.centerY.equalTo(btnChat)
        })

        //pk view
        pkView = UIImageView(image: UIImage(named: "PK", in: LiveRoomBundle(), compatibleWith: nil))
        pkView!.frame = CGRect(
            x: UIScreen.main.bounds.size.width / 2.0 - 25,
            y: UIScreen.main.bounds.size.height / 2.0 - 25,
            width: 50,
            height: 25)
        pkView!.isHidden = true
        addSubview(pkView!)
        //退出VC 停止直播
        closeBtn.setImage(UIImage(named: "live_exit", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        closeBtn.backgroundColor = UIColor.clear
        closeBtn.addTarget(self, action: #selector(closeVC), for: .touchUpInside)
        addSubview(closeBtn)
        bringSubviewToFront(closeBtn)
        closeBtn.snp.makeConstraints { make in
            make.trailing.equalTo(self).offset(-20);
            make.width.equalTo(52);
            make.height.equalTo(52);
            if #available(iOS 11.0, *) {
                make.top.equalTo(self).offset(60)
            } else {
                make.top.equalTo(self).offset(20)
            }
        }
        NotificationCenter.default.addObserver(self, selector: #selector(changeButtonStopPK), name: NSNotification.Name("PKNotificationKey"), object: nil)
        
        NotificationCenter.default.addObserver(self, selector: #selector(changeButtonText), name: NSNotification.Name("ChangePKToStopNotificationKey"), object: nil)
        msgTableView = TCMsgListTableView(frame: CGRect(x: 15, y: Int(btnChat.top) - MSG_TABLEVIEW_HEIGHT - MSG_TABLEVIEW_BOTTOM_SPACE, width: MSG_TABLEVIEW_WIDTH, height: MSG_TABLEVIEW_HEIGHT), style: .grouped)
        addSubview(msgTableView!)
        //弹幕
        msgTableView?.snp.makeConstraints({ make in
            make.width.equalTo(MSG_TABLEVIEW_WIDTH)
            make.height.equalTo(MSG_TABLEVIEW_HEIGHT)
            make.leading.equalTo(self).offset(15)
            make.bottom.equalTo(btnChat.top).offset(-MSG_TABLEVIEW_BOTTOM_SPACE-35)
            
        })
        
        bulletViewOne = TCMsgBarrageView(frame: CGRect(x: 0, y: Int(msgTableView!.top) - MSG_UI_SPACE - MSG_BULLETVIEW_HEIGHT, width: Int(SCREEN_WIDTH), height: MSG_BULLETVIEW_HEIGHT))
        guard let bulletViewOne = bulletViewOne else { return }
        insertSubview(bulletViewOne, belowSubview: btnChat)
        bulletViewOne.snp.makeConstraints({ make in
            make.leading.equalTo(self)
            make.bottom.equalTo(msgTableView!.top).offset(-MSG_UI_SPACE)
            make.width.equalTo(SCREEN_WIDTH)
            make.height.equalTo(MSG_BULLETVIEW_HEIGHT)
        })
        bulletViewTwo = TCMsgBarrageView(frame: CGRect(x: 0, y: CGFloat(Int(msgTableView!.top) - MSG_UI_SPACE), width: SCREEN_WIDTH, height: CGFloat(MSG_BULLETVIEW_HEIGHT)))
        guard let bulletViewTwo = bulletViewTwo else { return }
        insertSubview(bulletViewTwo, belowSubview: btnChat)
        bulletViewTwo.snp.makeConstraints({ make in
            make.top.equalTo(msgTableView!.top)
            make.width.equalTo(SCREEN_WIDTH)
            make.height.equalTo(MSG_BULLETVIEW_HEIGHT)
        })
        
        //输入框
        msgInputView.frame = CGRect(x: 0, y: height, width: width, height: CGFloat(MSG_TEXT_SEND_VIEW_HEIGHT))
        msgInputView.backgroundColor = UIColor.clear
        
        let imageView = UIImageView(frame: CGRect(x: 0, y: 0, width: msgInputView.width, height: msgInputView.height))
        imageView.image = UIImage(named: "input_comment", in: LiveRoomBundle(), compatibleWith: nil)
        let bulletBtn = UIButton(type: .custom)
        bulletBtn.frame = CGRect(x: 10, y: (msgInputView.height - CGFloat(MSG_TEXT_SEND_FEILD_HEIGHT)) / 2, width: CGFloat(MSG_TEXT_SEND_BULLET_BTN_WIDTH), height: CGFloat(MSG_TEXT_SEND_FEILD_HEIGHT))
        bulletBtn.setImage(UIImage(named: "Switch_OFF", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        bulletBtn.setImage(UIImage(named: "Switch_ON", in: LiveRoomBundle(), compatibleWith: nil), for: .selected)
        bulletBtn.addTarget(self, action: #selector(clickBullet(_:)), for: .touchUpInside)
        
        let sendBtn = UIButton(type: .custom)
        let x = width - CGFloat(15 + MSG_TEXT_SEND_BTN_WIDTH)
        sendBtn.frame = CGRect(x: x, y: (msgInputView.height - CGFloat (MSG_TEXT_SEND_FEILD_HEIGHT)) / 2, width: CGFloat(MSG_TEXT_SEND_BTN_WIDTH), height: CGFloat(MSG_TEXT_SEND_FEILD_HEIGHT))
        sendBtn.setTitle(LiveRoomLocalize("Demo.TRTC.LiveRoom.send"), for: .normal)
        sendBtn.titleLabel?.font = UIFont.systemFont(ofSize: 16)
        sendBtn.setTitleColor(UIColor(hex: "0x0ACCAC"), for: .normal)
        sendBtn.backgroundColor = UIColor.clear
        sendBtn.addTarget(self, action: #selector(clickSend), for: .touchUpInside)
        
        let msgInputFeildLine1 = UIImageView(image: UIImage(named: "vertical_line", in: LiveRoomBundle(), compatibleWith: nil))
        msgInputFeildLine1.frame = CGRect(x: bulletBtn.right + 10, y: sendBtn.y, width: 1, height: CGFloat(MSG_TEXT_SEND_FEILD_HEIGHT))
        let msgInputFeildLine2 = UIImageView(image: UIImage(named: "vertical_line", in: LiveRoomBundle(), compatibleWith: nil))
        msgInputFeildLine2.frame = CGRect(x: sendBtn.left - 10, y: sendBtn.y, width: 1, height: CGFloat(MSG_TEXT_SEND_FEILD_HEIGHT))
        msgInputFeild = UITextField(frame: CGRect(x: msgInputFeildLine1.right + 10, y: sendBtn.y, width: msgInputFeildLine2.left - msgInputFeildLine1.right - 20, height: CGFloat(MSG_TEXT_SEND_FEILD_HEIGHT)))
        msgInputFeild.backgroundColor = UIColor.clear
        msgInputFeild.returnKeyType = .send
        msgInputFeild.placeholder = LiveRoomLocalize("Demo.TRTC.LiveRoom.saysomething")
        msgInputFeild.delegate = self
        msgInputFeild.textColor = UIColor.black
        msgInputFeild.font = UIFont.systemFont(ofSize: 14)
        msgInputView.addSubview(imageView)
        msgInputView.addSubview(msgInputFeild)
        msgInputView.addSubview(bulletBtn)
        msgInputView.addSubview(sendBtn)
        msgInputView.addSubview(msgInputFeildLine1)
        msgInputView.addSubview(msgInputFeildLine2)
        addSubview(msgInputView)
        
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
        //BGM
        vBGMPanel.backgroundColor = UIColor(white: 0.2, alpha: 0.5)
        
        btnSelectBGM = UIButton()
        btnSelectBGM.titleLabel?.font = UIFont.systemFont(ofSize: 12.0)
        btnSelectBGM.layer.borderColor = UIColor(hex: "0x0ACCAC")?.cgColor
        btnSelectBGM.layer.masksToBounds = true
        btnSelectBGM.layer.cornerRadius = 6
        btnSelectBGM.layer.borderWidth = 1.0
        btnSelectBGM.setTitle(LiveRoomLocalize("Demo.TRTC.LiveRoom.accompaniment"), for: .normal)
        btnSelectBGM.setTitleColor(UIColor(hex: "0x0ACCAC"), for: .normal)
        btnSelectBGM.addTarget(self, action: #selector(clickMusicSelect(_:)), for: .touchUpInside)
        
        btnStopBGM = UIButton()
        btnStopBGM.titleLabel?.font = UIFont.systemFont(ofSize: 12.0)
        btnStopBGM.layer.borderColor = UIColor(hex: "0x0ACCAC")?.cgColor
        btnStopBGM.setTitle(LiveRoomLocalize("Demo.TRTC.LiveRoom.end"), for: .normal)
        btnStopBGM.layer.masksToBounds = true
        btnStopBGM.layer.cornerRadius = 6
        btnStopBGM.layer.borderWidth = 1.0
        btnStopBGM.setTitleColor(UIColor(hex: "0x0ACCAC"), for: .normal)
        btnStopBGM.addTarget(self, action: #selector(clickMusicClose(_:)), for: .touchUpInside)
        //音效
        vAudioEffectPanel.backgroundColor = UIColor(white: 0.2, alpha: 0.5)
        labVolumeForBGM.text = LiveRoomLocalize("Demo.TRTC.LiveRoom.accompanimentvolume")
        labVolumeForBGM.font = UIFont.systemFont(ofSize: 12.0)
        labVolumeForBGM.textColor = UIColor(hex: "0x0ACCAC")
        sldVolumeForBGM.minimumValue = 0
        sldVolumeForBGM.maximumValue = 200
        sldVolumeForBGM.value = 200
        sldVolumeForBGM.setThumbImage(UIImage(named: "slider", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        sldVolumeForBGM.setMinimumTrackImage(UIImage(named: "green", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        sldVolumeForBGM.setMaximumTrackImage(UIImage(named: "gray", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        sldVolumeForBGM.tag = 4
        sldVolumeForBGM.addTarget(self, action: #selector(sliderValueChange(_:)), for: .valueChanged)
        
        labVolumeForVoice.text = LiveRoomLocalize("Demo.TRTC.LiveRoom.vocalvolume")
        labVolumeForVoice.font = UIFont.systemFont(ofSize: 12.0)
        labVolumeForVoice.textColor = UIColor(hex: "0x0ACCAC")
        //    [_labVolumeForVoice sizeToFit];
        
        sldVolumeForVoice = UISlider()
        sldVolumeForVoice.minimumValue = 0
        sldVolumeForVoice.maximumValue = 200
        sldVolumeForVoice.value = 200
        sldVolumeForVoice.setThumbImage(UIImage(named: "slider", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        sldVolumeForVoice.setMinimumTrackImage(UIImage(named: "green", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        sldVolumeForVoice.setMaximumTrackImage(UIImage(named: "gray", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        sldVolumeForVoice.tag = 5
        sldVolumeForVoice.addTarget(self, action: #selector(sliderValueChange(_:)), for: .valueChanged)
        labPositionForBGM.text = LiveRoomLocalize("Demo.TRTC.LiveRoom.accompanimentfastforward")
        labPositionForBGM.font = UIFont.systemFont(ofSize: 12.0)
        labPositionForBGM.textColor = UIColor(hex: "0x0ACCAC")
        sldPositionForBGM.minimumValue = 0.0
        sldPositionForBGM.maximumValue = 1.0
        sldPositionForBGM.value = 0.0
        sldPositionForBGM.isContinuous = false
        sldPositionForBGM.setThumbImage(UIImage(named: "slider", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        sldPositionForBGM.setMinimumTrackImage(UIImage(named: "green", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        sldPositionForBGM.setMaximumTrackImage(UIImage(named: "gray", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        sldPositionForBGM.tag = 6
        sldPositionForBGM.addTarget(self, action: #selector(sliderValueChange(_:)), for: .valueChanged)
        for i in 0..<audioEffectArry.count {
            let btn = UIButton()
            btn.titleLabel?.font = UIFont.systemFont(ofSize: 12.0)
            btn.setTitle(audioEffectArry[i], for: .normal)
            btn.setTitleColor(UIColor.black, for: .normal)
            btn.setTitleColor(UIColor.white, for: .selected)
            btn.setBackgroundImage(UIImage(named: "unselected", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
            btn.layer.masksToBounds = true
            btn.layer.cornerRadius = btn.height / 2
            btn.addTarget(self, action: #selector(selectEffect(_:)), for: .touchUpInside)
            audioEffectViewArry.append(btn)
            btn.tag = i
        }
        
        for i in 0..<audioEffectArry2.count {
            let btn = UIButton()
            btn.titleLabel?.font = UIFont.systemFont(ofSize: 12.0)
            btn.setTitle(audioEffectArry2[i] , for: .normal)
            btn.setTitleColor(UIColor.black, for: .normal)
            btn.setTitleColor(UIColor.white, for: .selected)
            btn.setBackgroundImage(UIImage(named: "round-unselected", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
            btn.layer.masksToBounds = true
            btn.layer.cornerRadius = btn.height / 2
            btn.addTarget(self, action: #selector(selectEffect2(_:)), for: .touchUpInside)
            audioEffectViewArry2.append(btn)
            btn.tag = i
        }
        //add view
        for i in 0..<audioEffectViewArry.count {
            vAudioEffectPanel.addSubview(audioEffectViewArry[i])
            
        }
        
        for i in 0..<audioEffectViewArry2.count {
            vAudioEffectPanel.addSubview(audioEffectViewArry2[i])
        }
        
        vAudioEffectPanel.addSubview(labVolumeForVoice)
        vAudioEffectPanel.addSubview(sldVolumeForVoice)
        vAudioEffectPanel.addSubview(labVolumeForBGM)
        vAudioEffectPanel.addSubview(sldVolumeForBGM)
        vAudioEffectPanel.addSubview(labPositionForBGM)
        vAudioEffectPanel.addSubview(sldPositionForBGM)
        
        vBGMPanel.addSubview(btnSelectBGM)
        vBGMPanel.addSubview(btnStopBGM)
        btnSelectBGM.size(with: CGSize(width: 50, height: 20))
        btnSelectBGM.alignParentTop(withMargin: 10)
        btnSelectBGM.alignParentLeft(withMargin: 15)
        
        btnStopBGM.size(with: CGSize(width: 50, height: 20))
        btnStopBGM.alignParentTop(withMargin: 10)
        btnStopBGM.layout(toRightOf: btnSelectBGM, margin: 15)
        
        labPositionForBGM.size(with: CGSize(width: 60, height: 20))
        labPositionForBGM.alignParentTop(withMargin: 20)
        labPositionForBGM.alignParentLeft(withMargin: 15)
        
        sldPositionForBGM.size(with: CGSize(width: 270, height: 20))
        sldPositionForBGM.alignParentTop(withMargin: 20)
        sldPositionForBGM.alignParentLeft(withMargin: 90)
        
        labVolumeForBGM.size(with: CGSize(width: 60, height: 20))
        labVolumeForBGM.layout(below: labPositionForBGM, margin: 15)
        labVolumeForBGM.alignParentLeft(withMargin: 15)
        
        
        sldVolumeForBGM.size(with: CGSize(width: 270, height: 20))
        sldVolumeForBGM.layout(below: sldPositionForBGM, margin: 15)
        sldVolumeForBGM.alignParentLeft(withMargin: 90)
        
        labVolumeForVoice.size(with: CGSize(width: 60, height: 20))
        labVolumeForVoice.layout(below: labVolumeForBGM, margin: 15)
        labVolumeForVoice.alignParentLeft(withMargin: 15)
        
        sldVolumeForVoice.size(with: CGSize(width: 270, height: 20))
        sldVolumeForVoice.layout(below: sldVolumeForBGM, margin: 15)
        sldVolumeForVoice.alignParentLeft(withMargin: 90)
        
        // 混响
        for i in 0..<audioEffectViewArry.count {
            let btn = audioEffectViewArry[i]
            btn.size(with: CGSize(width: 40, height: 40))
            btn.layout(below: labVolumeForVoice,margin: 20)
            let a = (self.width-(CGFloat(audioEffectViewArry.count)*btn.width + 30.0))
            let b = (audioEffectViewArry.count-1)
            let c = btn.width+a / CGFloat(b)
            let margin = CGFloat(15.0 + CGFloat((c * CGFloat(i))))
            btn.alignParentLeft(withMargin: margin)
            
        }
        
        // 变声
        for i in 0..<audioEffectViewArry2.count {
            // 声音效果，每行放置6个
            let rowNum = i / 6
            let btn = audioEffectViewArry2[i]
            btn.size(with: CGSize(width: 40, height: 40))
            btn.layout(below: labVolumeForVoice, margin: CGFloat(65 + 45 * rowNum))
            let a = btn.width + (width - (6 * btn.width + 30)) / (6 - 1)
            let margin = CGFloat(15 + Int(a) * (i % 6))
            btn.alignParentLeft(withMargin: margin)
        }
        
        cover.frame = CGRect(x: 10.0, y: CGFloat(55 + 2 * icon_size), width: width - 20, height: height - 75 - CGFloat(3 * icon_size))
        cover.backgroundColor = UIColor.white
        cover.alpha = 0.5
        cover.isHidden = true
        addSubview(cover)
        self.setButtonHidden(true)
    }
    
    @available(iOS 13.0, *)
    func showLikeHeart() {
        let x = (btnMusic.frame.origin.x + btnBeauty.frame.origin.x) / 2
        let rect = CGRect(x: CGFloat(x), y: btnMusic.frame.origin.y, width: btnBeauty.frame.size.width, height: btnBeauty.frame.size.height)
        showLikeHeartStart(rect)
    }
    
    //监听键盘高度变化
    @objc func keyboardFrameDidChange(_ notice: Notification!) {
        if !msgInputFeild.isFirstResponder {
            return
        }
        let userInfo = notice.userInfo
        let endFrameValue = userInfo?[UIResponder.keyboardFrameEndUserInfoKey] as? NSValue
        let endFrame = endFrameValue!.cgRectValue
        UIView.animate(withDuration: 0.25, animations: { [self] in
            if endFrame.origin.y == height {
                msgInputView.y = endFrame.origin.y
            } else {
                msgInputView.y = endFrame.origin.y - msgInputView.height
            }
        })
    }
    
    func triggeValue() {
        
    }
    
    @objc func clickScreen(_ gestureRecognizer: UITapGestureRecognizer?) {
        _ = gestureRecognizer?.location(in: self)
        msgInputFeild.resignFirstResponder()
        vPKPanel.hiddenPanel()
        delegate?.clickScreen(gestureRecognizer)
        
    }
    
    @objc  func clickBullet(_ btn: UIButton?) {
        bulletBtnIsOn = !bulletBtnIsOn
        btn?.isSelected = bulletBtnIsOn
    }
    
    @objc func clickChat(_ button: UIButton?) {
        msgInputFeild.becomeFirstResponder()
    }
    
    @objc func clickSend() {
        _ = textFieldShouldReturn(msgInputFeild)
    }
    
    @objc func clickCamera(_ button: UIButton?) {
        guard let delegate = delegate else {
            return
        }
        delegate.clickCamera(button)
    }
    @objc func clickMusicSelect(_ button: UIButton?) {
        guard let delegate = delegate else { return }
        delegate.clickMusicSelect(button)
    }
    
    @objc func clickMusicClose(_ button: UIButton?) {
        guard let delegate = delegate else { return }
        delegate.clickMusicClose(button)
    }
    
    @objc func clickPK(_ button: UIButton?) {
        removeGestureRecognizer(tap)
        closeBtn.isHidden = true
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
        guard let delegate = delegate else { return  }
        delegate.clickBeauty(button)
    }
    
    @objc func clickMusic(_ button: UIButton?) {
        selectEffect(audioEffectViewArry[audioEffectSelectedType])
        selectEffect2(audioEffectViewArry2[audioEffectSelectedType2])
        guard let delegate = delegate else { return  }
        delegate.clickMusic(button)
    }
    
    @objc func clickSetting(_ button: UIButton?) {
        anchorViewController?.presentBottom(self.moreSettingVC)
    }
    
    
    @objc func sliderValueChange(_ slider: UISlider?) {
        guard let delegate = delegate else { return }
        delegate.sliderValueChange(slider)
    }
    
    @objc func sliderValueChangeEx(_ slider: UISlider?) {
        guard let delegate = delegate else { return }
        delegate.sliderValueChangeEx(slider)
    }
    
    // MARK: TCAnchorToolbarDelegate
    @objc func closeVC() {
        closeAlert = UIAlertController(title: nil, message: LiveRoomLocalize("Demo.TRTC.LiveRoom.interactioninprogress"), preferredStyle: .alert)
        let cancelAction = UIAlertAction(title: LiveRoomLocalize("Demo.TRTC.LiveRoom.subCancel"), style: .cancel, handler: { action in
        })
        let otherAction = UIAlertAction(title: LiveRoomLocalize("Demo.TRTC.LiveRoom.subConfirm"), style: .default, handler: { [weak self] action in
            guard let `self` = self else { return }
            guard let delegate = self.delegate else{
                return
            }
            self.topView.pauseLive()
            self.bulletViewOne?.stopAnimation()
            self.bulletViewTwo?.stopAnimation()
            NotificationCenter.default.removeObserver(self)
            delegate.closeRTMP()
            self.anchorViewController?.beautyViewModel?.applyDefaultSetting()
            // 直播过程中退出时展示统计信息
            self.resultView = TCPushShowResultView(
                frame: CGRect(
                    x: UIScreen.main.bounds.size.width / 4.0,
                    y: UIScreen.main.bounds.size.height / 3.0,
                    width: UIScreen.main.bounds.size.width / 2.0,
                    height: 222),
                resultData: self.topView,
                backHomepage: {
                    delegate.closeVC()
                })
            self.clearView = UIView(frame: self.bounds)
            self.clearView!.backgroundColor = UIColor.black
            self.clearView!.alpha = 0.3
            self.addSubview(self.clearView!)
            self.addSubview(self.resultView!)
            
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
    
    func textFieldShouldReturn(_ textField: UITextField) -> Bool {
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
            guard let liveRoom = liveRoom else {
                return true
            }
            liveRoom.sendRoomCustomMsg(cmd: cmd, message: msgModel.userMsg ?? "", callback: { code, error in
                
            })
        } else {
            msgModel.msgType = .normal
            liveRoom?.sendRoomTextMsg(message: textMsg ?? "", callback: { code, error in })
        }
        bulletMsg(msgModel)
        msgInputFeild.resignFirstResponder()
        return true
    }
    
    func bulletMsg(_ msgModel: TCMsgModel?) {
        msgTableView?.bulletNewMsg(msgModel)
        if msgModel?.msgType == .danmaMsg {
            if getLocation(bulletViewOne) >= getLocation(bulletViewTwo) {
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
    
    func getLocation(_ bulletView: TCMsgBarrageView?) -> CGFloat {
        guard let bulletView = bulletView else { return 0.0 }
        let view = bulletView.lastAnimateView
        let rect = view.layer.presentation()?.frame
        return (rect?.origin.x ?? 0.0) + (rect?.size.width ?? 0.0)
    }
    
    // 监听登出消息
    func onLogout(notice: Notification?) {
        closeInternal()
    }
    
    func closeInternal() {
        topView.pauseLive()
        bulletViewOne?.stopAnimation()
        bulletViewTwo?.stopAnimation()
        NotificationCenter.default.removeObserver(self)
        guard let delegate = delegate else { return  }
        delegate.closeRTMP()
        delegate.closeVC()
    }
    
    @objc func selectEffect(_ button: UIButton) {
        for i in 0..<audioEffectViewArry.count {
            let btn = audioEffectViewArry[i]
            btn.isSelected = false
            btn.setBackgroundImage(UIImage(named: "round-unselected", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        }
        button.isSelected = true
        button.setBackgroundImage(UIImage(named: "round-selected", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        audioEffectSelectedType = button.tag
        guard let delegate = delegate else { return  }
        delegate.selectEffect(audioEffectSelectedType)
    }
    
    @objc func selectEffect2(_ button: UIButton) {
        for i in 0..<audioEffectViewArry2.count {
            let btn = audioEffectViewArry2[i]
            btn.isSelected = false
            btn.setBackgroundImage(UIImage(named: "round-unselected", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        }
        button.isSelected = true
        button.setBackgroundImage(UIImage(named: "round-selected", in: LiveRoomBundle(), compatibleWith: nil), for: .normal)
        audioEffectSelectedType2 = button.tag
        guard let delegate = delegate else { return  }
        delegate.selectEffect2(audioEffectSelectedType2)
    }
    
    @available(iOS 13.0, *)
    func showLikeHeartStart(_ frame: CGRect) {
        if showLikeHeartStartRectFreqControl == nil {
            showLikeHeartStartRectFreqControl = TCFrequeControl(counts: 10, andSeconds: 1)
        }
        if !showLikeHeartStartRectFreqControl!.canTrigger() {
            return
        }
        if viewsHidden { return }
        let imageView = UIImageView(frame: frame)
        imageView.image = UIImage(named: "img_like", in: LiveRoomBundle(), compatibleWith: nil)!.withTintColor(UIColor.randomFlatDark())
        superview!.addSubview(imageView)
        imageView.alpha = 0
        imageView.layer.add(hearAnimation(from: frame), forKey: nil)
        DispatchQueue.main.asyncAfter(deadline: DispatchTime.now() + Double(Int64(3 * Double(NSEC_PER_SEC))) / Double(NSEC_PER_SEC), execute: {
            imageView.removeFromSuperview()
        })
    }
    
    func enableMix(disable: Bool) {
        btnMusic.isEnabled = disable
    }
    
    func hearAnimation(from frame: CGRect) -> CAAnimation {
        //位置
        let animation = CAKeyframeAnimation(keyPath: "position")
        animation.beginTime = 0.5
        animation.duration = 2.5
        animation.isRemovedOnCompletion = true
        animation.fillMode = .forwards
        animation.repeatCount = 0
        animation.calculationMode = .cubicPaced
        let curvedPath = CGMutablePath()
        let point0 = CGPoint(x: frame.origin.x + frame.size.width / 2, y: frame.origin.y + frame.size.height / 2)
        curvedPath.move(to: CGPoint(x: point0.x, y: point0.y), transform: .identity)
        if heartAnimationPoints.count < 40 {
            let x11 = CGFloat(point0.x - CGFloat(arc4random() % 30) + 30.0)
            let y11 = CGFloat(frame.origin.y - CGFloat(arc4random() % 60))
            let x1 = CGFloat(point0.x - CGFloat(arc4random() % 15) + 15)
            let y1 = CGFloat(frame.origin.y - CGFloat(arc4random() % 60) - 30)
            let point1 = CGPoint(x: x11, y: y11)
            let point2 = CGPoint(x: x1, y: y1)
            let conffset2 = CGFloat(superview!.bounds.size.width * 0.2)
            let conffset21 = CGFloat(superview!.bounds.size.width * 0.1)
            let x2 = CGFloat(point0.x - CGFloat(arc4random() % UInt32(conffset2)) + conffset2)
            let y2 = CGFloat(arc4random() % 30 + 240)
            let x21 = CGFloat(point0.x - CGFloat(arc4random() % UInt32(conffset21)) + conffset21)
            let y21 = (y2 + y1) / 2 + CGFloat(arc4random() % 30) - 30
            let point3 = CGPoint(x: x21, y: y21)
            let point4 = CGPoint(x: x2, y: y2)
            heartAnimationPoints.append(point1)
            heartAnimationPoints.append(point2)
            heartAnimationPoints.append(point3)
            heartAnimationPoints.append(point4)
        }
        // 从_heartAnimationPoints中随机选取一组point
        let idx = Int(Int(arc4random()) % (heartAnimationPoints.count / 4))
        let p1 = heartAnimationPoints[4 * idx]
        let p2 = heartAnimationPoints[4 * idx + 1]
        let p3 = heartAnimationPoints[4 * idx + 2]
        let p4 = heartAnimationPoints[4 * idx + 3]
        curvedPath.addQuadCurve(to: CGPoint(x: p2.x, y: p2.y), control: CGPoint(x: p1.x, y: p1.y), transform: .identity)
        curvedPath.addQuadCurve(to: CGPoint(x: p4.x, y: p4.y), control: CGPoint(x: p3.x, y: p3.y), transform: .identity)
        animation.path = curvedPath
        let opacityAnim = CABasicAnimation(keyPath: "opacity")
        opacityAnim.fromValue = NSNumber(value: 1.0)
        opacityAnim.toValue = NSNumber(value: 0)
        opacityAnim.isRemovedOnCompletion = false
        opacityAnim.beginTime = 0
        opacityAnim.duration = 3
        //比例
        let scaleAnim = CABasicAnimation(keyPath: "transform.scale")
        scaleAnim.fromValue = NSNumber(value: 0.0)
        scaleAnim.toValue = NSNumber(value: 1)
        scaleAnim.isRemovedOnCompletion = false
        scaleAnim.fillMode = .forwards
        scaleAnim.duration = 0.5
        let animGroup = CAAnimationGroup()
        animGroup.animations = [scaleAnim, opacityAnim, animation]
        animGroup.duration = 3
        return animGroup
    }
    
    override func point(inside point: CGPoint, with event: UIEvent?) -> Bool {
        return true
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
            if #available(iOS 13.0, *) {
                showLikeHeart()
            } else {
                // Fallback on earlier versions
            }
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
    
    func textFieldShouldBeginEditing(_ textField: UITextField) -> Bool {
        msgInputFeild.text = ""
        return true
    }
    
    func textFieldDidEndEditing(_ textField: UITextField) {
        msgInputFeild.text = textField.text
    }
    // MARK: - 滑动隐藏界面UI
    func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent) {
        let touch = event.allTouches!.first
        touchBeginLocation = touch!.location(in: self)
        if !vPKPanel.isHidden {
            vPKPanel.isHidden = true
            closeBtn.isHidden = false
            addGestureRecognizer(tap)
        }
    }
}
