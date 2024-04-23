//
//  AnchorLivingView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/19.
//

import Foundation
import TUICore

class AnchorLivingView: UIView {
    private var isPortrait: Bool = {
        return WindowUtils.isPortrait
    }()
    private let giftCacheService = GiftCacheService()

    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
    }
    
    private var liveRoomInfo:LiveRoomInfo {
        engineService.liveRoomInfo
    }
    private var engineService: RoomEngineService
    init(engineService: RoomEngineService) {
        self.engineService = engineService
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private lazy var infoButton: UIControl = {
        let view = UIControl()
        view.mm_h = 32.scale375()
        view.backgroundColor = UIColor.g2.withAlphaComponent(0.4)
        view.layer.cornerRadius = view.mm_h * 0.5
        view.addTarget(self, action: #selector(infoButtonClick), for: .touchUpInside)

        let imageView = UIImageView()
        view.addSubview(imageView)
        imageView.snp.makeConstraints { make in
            make.size.equalTo(CGSize(width: 24.scale375(), height: 24.scale375()))
            make.leading.equalToSuperview().inset(4.scale375())
            make.centerY.equalToSuperview()
        }
        imageView.layer.masksToBounds = true
        imageView.layer.cornerRadius = 24.scale375() * 0.5
        imageView.kf.setImage(with: URL(string: liveRoomInfo.anchorInfo.value.avatarUrl.value), placeholder: UIImage.placeholderImage)
        
        let title = UILabel()
        view.addSubview(title)
        title.font = .customFont(ofSize: 14)
        title.textColor = .g7
        title.textAlignment = .left
        title.text = liveRoomInfo.name.value
        
        title.snp.makeConstraints { make in
            make.leading.equalTo(imageView.snp.trailing).offset(8.scale375())
            make.trailing.equalToSuperview().inset(8.scale375())
            make.centerY.equalToSuperview()
            make.height.equalToSuperview()
        }
        weak var weakTitle = title
        weak var weakImageView = imageView
        liveRoomInfo.anchorInfo.addObserver(self) { [weak self] _, _ in
            guard let self = self else{ return}
            guard let weakTitle = weakTitle else{ return}
            guard let weakImageView = weakImageView else{ return}
            weakImageView.kf.setImage(with: URL(string: self.liveRoomInfo.anchorInfo.value.avatarUrl.value), placeholder: UIImage.placeholderImage)
            weakTitle.text = self.liveRoomInfo.name.value
        }
        return view
    }()

    private lazy var closeButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(.liveBundleImage("live_anchor_close_icon"), for: .normal)
        view.addTarget(self, action: #selector(closeButtonClick), for: .touchUpInside)
        return view
    }()

    private lazy var audienceListView: AudienceListView = {
        var view = AudienceListView(engineService: self.engineService)
        return view
    }()

    private lazy var featureClickPanel: FeatureClickPanel = {
        var model = FeatureClickPanelModel()
        model.itemSize = CGSize(width: 36.scale375(), height: 36.scale375())
        model.itemDiff = CGFloat(57.75).scale375Width()
        model.items.append(FeatureItem(image: .liveBundleImage("live_link_icon"), action: LivingViewActionEvent.linkClick))
        model.items.append(FeatureItem(image: .liveBundleImage("live_set_icon"), action: LivingViewActionEvent.setClick))
        model.items.append(FeatureItem(image: .liveBundleImage("live_music_icon"), action: LivingViewActionEvent.musicClick))
        var featureClickPanel = FeatureClickPanel(model: model)
        featureClickPanel.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            guard let action = action as? LivingViewActionEvent else{ return}
            switch action {
            case .linkClick:
                self?.showAnchorLinkControlPanel()
            case .setClick:
                self?.showAnchorSettingPanel()
            case .musicClick:
                self?.showAnchorMusicPlayPanel()
            default:
                break
            }
        }
        return featureClickPanel
    }()
    
    private lazy var floatView: LinkMicAnchorFloatView = {
        let view = LinkMicAnchorFloatView(engineService: self.engineService)
        view.isHidden = true
        return view
    }()
    private lazy var barrageDisplayView: TUIBarrageDisplayView = {
        let view = TUIBarrageDisplayView(roomId: self.liveRoomInfo.roomId.value)
        view.delegate = self
        
        return view
    }()
    
    private lazy var giftDisplayView: TUIGiftPlayView = {
        let view = TUIGiftPlayView(groupId: self.liveRoomInfo.roomId.value)
        view.delegate = self
        return view
    }()
    
    deinit {
        giftCacheService.clearCacheDirectory()
    }
}

// MARK: Layout

extension AnchorLivingView {
    func constructViewHierarchy() {
        backgroundColor = .clear
        addSubview(barrageDisplayView)
        addSubview(giftDisplayView)
        addSubview(closeButton)
        addSubview(audienceListView)
        addSubview(infoButton)
        addSubview(featureClickPanel)
        addSubview(floatView)
    }

    func updateRootViewOrientation(isPortrait: Bool) {
        self.isPortrait = isPortrait
        activateConstraints()
    }

    func activateConstraints() {
        giftDisplayView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }

        barrageDisplayView.snp.remakeConstraints { make in
            make.left.equalToSuperview().offset(16)
            make.width.equalTo(268.scale375())
            make.height.equalTo(212.scale375Height())
            if self.isPortrait {
                make.bottom.equalTo(featureClickPanel.snp.top).offset(-4.scale375Height())
            } else {
                make.bottom.equalTo(featureClickPanel.snp.bottom)
            }
        }
        
        closeButton.snp.remakeConstraints { make in
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
            make.trailing.equalToSuperview().inset((self.isPortrait ? 16 : 45).scale375())
            make.top.equalToSuperview().inset((self.isPortrait ? 58 : 24).scale375Height())
        }

        audienceListView.snp.remakeConstraints { make in
            make.centerY.equalTo(closeButton)
            make.height.equalTo(24.scale375())
            make.width.equalTo(LocalizedLanguage.isChinese ? 74.scale375() : 102.scale375())
            make.trailing.equalTo(closeButton.snp.leading).offset(-4.scale375())
        }

        infoButton.snp.remakeConstraints { make in
            make.centerY.equalTo(closeButton)
            make.height.equalTo(infoButton.mm_h)
            make.width.greaterThanOrEqualTo(100.scale375())
            make.width.lessThanOrEqualTo(375.scale375()*0.5)
            make.leading.equalToSuperview().inset((self.isPortrait ? 16 : 45).scale375())
        }

        featureClickPanel.snp.remakeConstraints { make in
            make.height.equalTo(featureClickPanel.mm_h)
            make.width.equalTo(featureClickPanel.mm_w)
            if self.isPortrait {
                make.centerX.equalToSuperview()
            } else {
                make.trailing.equalToSuperview().inset(CGFloat(75.75).scale375Width())
            }
            make.bottom.equalToSuperview().inset(WindowUtils.bottomSafeHeight + 12)
        }
        
        floatView.snp.makeConstraints { make in
            make.top.equalTo(audienceListView.snp.bottom).offset(34.scale375Width())
            make.height.equalTo(86.scale375())
            make.width.equalTo(114.scale375())
            make.trailing.equalToSuperview().offset(-8.scale375())
        }
    }
}

// MARK: Action

extension AnchorLivingView {
    @objc func closeButtonClick() {
        let model = ActionModel()
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .redPinkColor)
        designConfig.backgroundColor = .g2
        designConfig.lineColor = .g3.withAlphaComponent(0.1)
        let item = ActionItem(text: .confirmCloseText, designConfig: designConfig, action: LivingViewActionEvent.closeClick)
        model.items.append(item)
        
        let actionPanel = ActionPanel(model: model)
        PopupPanelController.alertView(actionPanel)
        actionPanel.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            if let actionType = action as? LivingViewActionEvent , actionType == .closeClick {
                self?.liveRoomInfo.userLiveStatus.value = .none
            }
        }
    }

    @objc func infoButtonClick() {
        PopupPanelController.alertView(RoomInfoViewPanel(engineService: engineService))
    }
    
    private func showAnchorLinkControlPanel() {
        PopupPanelController.alertView(AnchorLinkControlPanel(engineService: engineService))
    }

    private func showAnchorSettingPanel() {
        let view = AnchorSettingPanel(engineService: engineService)
        view.rootController = WindowUtils.getCurrentWindowViewController()
        PopupPanelController.alertView(view)
    }

    private func showAnchorMusicPlayPanel() {
        let view = AnchorMusicPlayPanel(engineService: engineService)
        PopupPanelController.alertView(view)
    }
}

extension AnchorLivingView {
    func showLinkMicFloatView(isPresent: Bool) {
        floatView.isHidden = !isPresent
    }
    
    func getBarrageCount() -> Int {
        barrageDisplayView.getBarrageCount()
    }
    
    func getLikeCount() -> Int {
        giftDisplayView.getLikeCount()
    }
    
}

extension AnchorLivingView: TUIBarrageDisplayViewDelegate {
    func barrageDisplayView(_ barrageDisplayView: TUIBarrageDisplayView, createCustomCell barrage: TUIBarrage) -> UIView? {
        guard let type = barrage.extInfo["TYPE"], type.value as? String == "GIFTMESSAGE" else {
            return nil
        }
        return CustomBarrageCell.getCustomCell(barrage: barrage)
    }
}


extension AnchorLivingView: TUIGiftPlayViewDelegate {
    func giftPlayView(_ giftPlayView: TUIGiftPlayView, onReceiveGift gift: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser) {
        liveRoomInfo.giftIncome += gift.price * giftCount
        liveRoomInfo.giftPeopleMap[sender.userId] = ""
        let barrage = TUIBarrage()
        barrage.content = "gift"
        barrage.user.userId = sender.userId
        barrage.user.userName = sender.userName
        barrage.user.avatarUrl = sender.avatarUrl
        barrage.user.level = sender.level
        barrage.extInfo["TYPE"] = AnyCodable("GIFTMESSAGE")
        barrage.extInfo["gift_name"] = AnyCodable(gift.giftName)
        barrage.extInfo["gift_count"] = AnyCodable(giftCount)
        barrage.extInfo["gift_icon_url"] = AnyCodable(gift.imageUrl)
        if receiver.userId == TUILogin.getUserID() {
            receiver.userName = .meText
        }
        barrage.extInfo["gift_receiver_username"] = AnyCodable(receiver.userName)
        barrageDisplayView.insertBarrages([barrage])
    }
    
    func giftPlayView(_ giftPlayView: TUIGiftPlayView, onPlayGiftAnimation gift: TUIGift) {
        giftCacheService.request(urlString: gift.animationUrl) { error, data in
            guard let data = data else { return }
            if error == 0 {
                DispatchQueue.main.async {
                    giftPlayView.playGiftAnimation(animationData: data)
                }
            }
        }
    }
}

private extension String {
    static var confirmCloseText = {
        localized("live.anchor.confirm.close")
    }()
    
    static var sendText = {
       localized("live.giftView.sendOut")
    }()
    
    static var meText = {
        localized("live.barrage.me")
    }()
}
