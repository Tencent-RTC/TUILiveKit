//
//  AudienceLivingView.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/12/15.
//

import TUICore
import UIKit

class AudienceLivingView: UIView {
    private var isViewReady: Bool = false
    private let giftCacheService = GiftCacheService()
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }

    private var popupViewController: UIViewController?
    private let giftCloudServer: IGiftCloudServer = GiftCloudServer()
    private var engineService: RoomEngineService
    private var liveRoomInfo: LiveRoomInfo {
        engineService.liveRoomInfo
    }

    init(engineService: RoomEngineService) {
        self.engineService = engineService
        super.init(frame: .zero)
        registerObserver()
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
    

    private lazy var audienceListView: AudienceListView = {
        var view = AudienceListView(engineService: self.engineService)
        return view
    }()

    private lazy var leaveImageView: UIImageView = {
        var imageView = UIImageView()
        imageView.image = .liveBundleImage("live_leave_icon")
        let leaveTap = UITapGestureRecognizer(target: self, action: #selector(leaveImageViewClick))
        imageView.isUserInteractionEnabled = true
        imageView.addGestureRecognizer(leaveTap)
        return imageView
    }()

    private lazy var barrageSendView: TUIBarrageButton = {
        var view = TUIBarrageButton(roomId: liveRoomInfo.roomId.value)
        view.layer.borderColor = UIColor.g3.withAlphaComponent(0.3).cgColor
        view.layer.borderWidth = 0.5
        view.layer.cornerRadius = 18.scale375Height()
        view.layer.masksToBounds = true
        return view
    }()

    private var featureItems: [FeatureItem] = {
        var items = [FeatureItem]()
        items.append(FeatureItem(image: .liveBundleImage("live_gift_icon"), action: AudienceViewActionEvent.giftClick))
        items.append(FeatureItem(image: .liveBundleImage("live_link_icon"), action: AudienceViewActionEvent.linkClick))
        items.append(FeatureItem(image: .liveBundleImage("live_like_icon"), action: AudienceViewActionEvent.likeClick))
        return items
    }()

    private lazy var featureClickPanel: FeatureClickPanel = {
        var model = FeatureClickPanelModel()
        model.itemSize = CGSize(width: 36.scale375Width(), height: 36.scale375Width())
        model.itemDiff = 6.scale375()
        model.items = self.featureItems
        var featureClickPanel = FeatureClickPanel(model: model)
        featureClickPanel.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            guard let action = action as? AudienceViewActionEvent else { return }
            switch action {
            case .linkClick:
                self?.showLinkMicTypePanel()
            case .willCancelRequestLinkClick:
                self?.showCancelLinkMicPanel()
            case .willCloseLinkClick:
                self?.showCloseLinkMicPanel()
            case .giftClick:
                self?.onClickGift()
            case .likeClick:
                self?.onClickLike()
            default:
                break
            }
        }

        return featureClickPanel
    }()

    private lazy var floatView: LinkMicAudienceFloatView = {
        let view = LinkMicAudienceFloatView(engineService: self.engineService)
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

    private lazy var giftPanelView: TUIGiftListView = {
        let view = TUIGiftListView(groupId: liveRoomInfo.roomId.value)
        view.delegate = self
        giftCloudServer.queryGiftInfoList { [weak self] error, giftList in
            guard let self = self else { return }
            DispatchQueue.main.async {
                if error == .noError {
                    view.setGiftList(giftList)
                } else {
                    self.makeToast("query gift list error, code = \(error)")
                }
            }
        }
        giftCloudServer.queryBalance { [weak self] error, balance in
            guard let self = self else { return }
            DispatchQueue.main.async {
                if error == .noError {
                    view.setBalance(balance)
                } else {
                    self.makeToast("query balance error, code = \(error)")
                }
            }
        }
        return view
    }()

    func constructViewHierarchy() {
        backgroundColor = .clear
        addSubview(infoButton)
        addSubview(audienceListView)
        addSubview(leaveImageView)
        addSubview(barrageDisplayView)
        addSubview(featureClickPanel)
        addSubview(floatView)
        addSubview(giftDisplayView)
        addSubview(barrageSendView)
    }

    func activateConstraints() {
        giftDisplayView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }

        barrageDisplayView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(16)
            make.bottom.equalTo(barrageSendView.snp.top).offset(-8.scale375Height())
            make.width.equalTo(247.scale375Width())
            make.height.equalTo(212.scale375Height())
        }

        infoButton.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(16.scale375Width())
            make.top.equalToSuperview().offset(54.scale375Height())
            make.width.equalTo(155.scale375Width())
            make.height.equalTo(32.scale375Height())
        }
        
        infoButton.snp.remakeConstraints { make in
            make.top.equalToSuperview().offset(54.scale375Height())
            make.height.equalTo(infoButton.frame.height)
            make.width.greaterThanOrEqualTo(100.scale375())
            make.width.lessThanOrEqualTo(375.scale375()*0.5)
            make.leading.equalToSuperview().inset(16.scale375())
        }

        leaveImageView.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-20.scale375Width())
            make.top.equalToSuperview().offset(58.scale375Height())
            make.width.equalTo(24.scale375Width())
            make.height.equalTo(24.scale375Width())
        }

        audienceListView.snp.makeConstraints { make in
            make.trailing.equalTo(leaveImageView.snp.leading).offset(-4.scale375Width())
            make.centerY.equalTo(leaveImageView)
            make.height.equalTo(24.scale375Height())
            make.width.equalTo(LocalizedLanguage.isChinese ? 74.scale375() : 102.scale375())
        }
        barrageSendView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(16.scale375Width())
            make.trailing.equalTo(featureClickPanel.snp.leading).offset(-12.scale375Width())
            make.height.equalTo(36.scale375Height())
            make.bottom.equalToSuperview().offset(-34.scale375Height())
        }

        featureClickPanel.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-16.scale375Width())
            make.height.equalTo(featureClickPanel.mm_h)
            make.width.equalTo(featureClickPanel.mm_w)
            make.bottom.equalToSuperview().offset(-34.scale375Height())
        }

        floatView.snp.makeConstraints { make in
            make.top.equalTo(audienceListView.snp.bottom).offset(34.scale375Width())
            make.height.width.equalTo(86.scale375())
            make.trailing.equalToSuperview().offset(-8.scale375())
        }
    }
    
    deinit {
        giftCacheService.clearCacheDirectory()
    }
}

// MARK: Action

extension AudienceLivingView {
    @objc func infoButtonClick() {
        PopupPanelController.alertView(RoomInfoViewPanel(engineService: engineService))
    }

    @objc func leaveImageViewClick() {
        exitRoom()
        WindowUtils.getCurrentWindowViewController()?.backToPreviousPage()
    }

    private func enterRoom() {
        engineService.enterRoom(roomId: liveRoomInfo.roomId.value) { [weak self] in
            self?.initStreamInfo()
            self?.engineService.changeSelfRole(role: .audience)
            self?.engineService.changeSelfStatus(status: .none)
            self?.engineService.changeUserLiveStatus(userLiveStatus: .pushing)
        } onError: { [weak self] code, message in
            let statusString = String(code.rawValue) + "," + message
            self?.makeToast(.localizedReplace(.enterRoomFailedmessageText, replace: statusString))
        }
    }

    private func exitRoom() {
        engineService.changeSelfRole(role: .audience)
        engineService.changeSelfStatus(status: .none)
        engineService.exitRoom(onSuccess: nil, onError: nil)
    }

    private func initStreamInfo() {
        engineService.getUserList(onSuccess: nil, onError: nil)
        engineService.fetchRoomInfo(onSuccess: nil, onError: nil)
    }

    private func initLivingConfig() {
        engineService.initLivingConfig()
    }

    private func showRecentWatchMemberPanel() {
        PopupPanelController.alertView(RecentWatchMemberPanel(engineService: engineService))
    }

    private func showLinkMicTypePanel() {
        let actionPanel = LinkMicTypePanel()
        PopupPanelController.alertView(actionPanel)
        actionPanel.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            if let actionType = action as? AudienceLinkActionEvent {
                switch actionType {
                case .settingClick:
                    self?.showVideoLinkSettingPanel()
                case .videoLinkClick:
                    self?.videoLinkClick()
                case .audioLinkClick:
                    self?.audioLinkClick()
                }
            }
        }
    }

    private func showVideoLinkSettingPanel() {
        let actionPanel = VideoLinkSettingPanel(engineService: engineService)
        PopupPanelController.alertView(actionPanel)
        actionPanel.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            if let actionType = action as? AudienceLinkActionEvent, actionType == .videoLinkClick {
                self?.videoLinkClick()
            }
        }
    }

    private func videoLinkClick() {
        engineService.changeSelfRole(role: .audience)
        engineService.changeSelfStatus(status: .applying)
        makeToast(.waitToLinkText)
        let request = engineService.takeSeat { [weak self] _, _ in
            guard let self = self else { return }
            self.engineService.openLocalCamera {
            } onError: { [weak self] code, message in
                let statusString = String(code.rawValue) + "," + message
                self?.makeToast(.localizedReplace(.operateFailedText, replace: statusString))
            }
            self.engineService.openLocalMicrophone {
            } onError: { [weak self] code, message in
                let statusString = String(code.rawValue) + "," + message
                self?.makeToast(.localizedReplace(.operateFailedText, replace: statusString))
            }
            self.engineService.changeSelfRole(role: .anchor)
            self.engineService.changeSelfStatus(status: .linking)
            self.initLivingConfig()
            self.makeToast(.linkSuccessTipText)
        } onRejected: { [weak self] _, _, _ in
            guard let self = self else { return }
            self.engineService.changeSelfStatus(status: .none)
            self.makeToast(.rejectedMessageText)
        } onCancelled: { [weak self] _, _ in
            guard let self = self else { return }
            self.engineService.changeSelfStatus(status: .none)
        } onTimeout: { [weak self] _, _ in
            guard let self = self else { return }
            self.engineService.changeSelfStatus(status: .none)
            self.makeToast(.timeoutMessageText)
        } onError: { [weak self] _, _, _, _ in
            guard let self = self else { return }
            self.engineService.changeSelfStatus(status: .none)
        }
        engineService.liveKitStore.selfInfo.requestId = request.requestId
    }

    private func audioLinkClick() {
        engineService.changeSelfRole(role: .audience)
        engineService.changeSelfStatus(status: .applying)
        onRequestLinkMic()
        let request = engineService.takeSeat { [weak self] _, _ in
            guard let self = self else { return }
            self.engineService.openLocalMicrophone {
            } onError: { [weak self] code, message in
                let statusString = String(code.rawValue) + "," + message
                self?.makeToast(.localizedReplace(.operateFailedText, replace: statusString))
            }
            self.engineService.changeSelfRole(role: .anchor)
            self.engineService.changeSelfStatus(status: .linking)
            self.initLivingConfig()
            self.makeToast(.linkSuccessTipText)
        } onRejected: { [weak self] _, _, _ in
            guard let self = self else { return }
            self.engineService.changeSelfStatus(status: .none)
            self.makeToast(.rejectedMessageText)
        } onCancelled: { [weak self] _, _ in
            guard let self = self else { return }
            self.engineService.changeSelfStatus(status: .none)
            self.onCancelRequestLinkMic()
        } onTimeout: { [weak self] _, _ in
            guard let self = self else { return }
            self.engineService.changeSelfStatus(status: .none)
            self.makeToast(.timeoutMessageText)
        } onError: { [weak self] _, _, _, _ in
            guard let self = self else { return }
            self.engineService.changeSelfStatus(status: .none)
        }
        engineService.liveKitStore.selfInfo.requestId = request.requestId
    }

    private func showCancelLinkMicPanel() {
        let model = ActionModel()
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .redPinkColor)
        designConfig.backgroundColor = .g2
        designConfig.lineColor = .g3.withAlphaComponent(0.1)
        let item = ActionItem(text: .cancelLinkMicRequestText, designConfig: designConfig, action: AudienceViewActionEvent.didCancelRequestLinkClick)
        model.items.append(item)

        let actionPanel = ActionPanel(model: model)
        PopupPanelController.alertView(actionPanel)
        actionPanel.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            if let actionType = action as? AudienceViewActionEvent, actionType == .didCancelRequestLinkClick {
                self?.cancelLinkMicClick()
            }
        }
    }

    private func showCloseLinkMicPanel() {
        let model = ActionModel()
        let designConfig = ActionItemDesignConfig(lineWidth: 7, titleColor: .redPinkColor)
        designConfig.backgroundColor = .g2
        designConfig.lineColor = .g3.withAlphaComponent(0.1)
        let item = ActionItem(text: .closeLinkMicText, designConfig: designConfig, action: AudienceViewActionEvent.didCloseLinkClick)
        model.items.append(item)

        let actionPanel = ActionPanel(model: model)
        PopupPanelController.alertView(actionPanel)
        actionPanel.clickEventCallBack.addObserver(self) { [weak self] action, _ in
            if let actionType = action as? AudienceViewActionEvent, actionType == .didCloseLinkClick {
                self?.closeLinkMicClick()
            }
        }
    }

    private func cancelLinkMicClick() {
        engineService.cancelRequest(engineService.liveKitStore.selfInfo.requestId)
    }

    private func closeLinkMicClick() {
        engineService.leaveSeat { [weak self] in
            guard let self = self else { return }
            self.engineService.changeSelfRole(role: .audience)
            self.engineService.changeSelfStatus(status: .none)
        }
    }

    private func registerObserver() {
        engineService.liveKitStore.selfInfo.status.addObserver(self) { [weak self] _, _ in
            guard let self = self else{ return}
            if self.engineService.liveKitStore.selfInfo.status.value == UserInteractionStatus.none {
                self.onCloseLinkMic()
                self.onCancelRequestLinkMic()
            } else if self.engineService.liveKitStore.selfInfo.status.value == UserInteractionStatus.applying {
                self.onRequestLinkMic()
            } else if self.engineService.liveKitStore.selfInfo.status.value == UserInteractionStatus.linking {
                self.onLinkingMic()
            }
        }
    }
}

extension AudienceLivingView {
    private func updateBottomAreaLayout() {
        featureClickPanel.snp.updateConstraints { make in
            make.width.equalTo(featureClickPanel.mm_w)
        }
    }

    func onEnableLinkMic() {
        var items = featureClickPanel.model.items
        if !items.contains(where: { $0.normalImage == .liveBundleImage("live_link_icon") }) {
            items.insert(FeatureItem(image: .liveBundleImage("live_link_icon"), action: AudienceViewActionEvent.linkClick), at: 1)
            featureClickPanel.updateFeatureItems(newItems: items)
            updateBottomAreaLayout()
        } else {
            return
        }
    }

    func onRequestLinkMic() {
        var items = featureClickPanel.model.items
        if items.contains(where: { $0.normalImage == .liveBundleImage("live_link_icon") }) {
            items = featureClickPanel.model.items.filter { $0.normalImage != .liveBundleImage("live_link_icon") }
            items.insert(FeatureItem(image: .liveBundleImage("live_linking_icon"),
                                     action: AudienceViewActionEvent.willCancelRequestLinkClick), at: 1)
            featureClickPanel.updateFeatureItems(newItems: items)
            updateBottomAreaLayout()

            floatView.isHidden = false
        }
    }

    func onLinkingMic() {
        var items = featureClickPanel.model.items
        if items.contains(where: { $0.normalImage == .liveBundleImage("live_link_icon") ||
                $0.normalImage == .liveBundleImage("live_linking_icon")
        }) {
            items = featureClickPanel.model.items.filter { ![.liveBundleImage("live_link_icon"),
                                                             .liveBundleImage("live_linking_icon"),].contains($0.normalImage) }
            items.insert(FeatureItem(image: .liveBundleImage("live_linked_icon"),
                                     action: AudienceViewActionEvent.willCloseLinkClick), at: 1)
            featureClickPanel.updateFeatureItems(newItems: items)
            updateBottomAreaLayout()

            floatView.isHidden = true
        }
    }

    func onCancelRequestLinkMic() {
        var items = featureClickPanel.model.items
        if items.contains(where: { $0.normalImage == .liveBundleImage("live_linking_icon") }) {
            items = featureClickPanel.model.items.filter { $0.normalImage != .liveBundleImage("live_linking_icon") }
            items.insert(FeatureItem(image: .liveBundleImage("live_link_icon"),
                                     action: AudienceViewActionEvent.linkClick), at: 1)
            featureClickPanel.updateFeatureItems(newItems: items)
            updateBottomAreaLayout()

            floatView.isHidden = true
        }
    }

    func onCloseLinkMic() {
        var items = featureClickPanel.model.items
        if items.contains(where: { $0.normalImage == .liveBundleImage("live_linked_icon") }) {
            items = featureClickPanel.model.items.filter { $0.normalImage != .liveBundleImage("live_linked_icon") }
            items.insert(FeatureItem(image: .liveBundleImage("live_link_icon"),
                                     action: AudienceViewActionEvent.linkClick), at: 1)
            featureClickPanel.updateFeatureItems(newItems: items)
            updateBottomAreaLayout()
        }
    }

    func onDisableLinkMic() {
        let items = featureClickPanel.model.items.filter { ![.liveBundleImage("live_link_icon"),
                                                             .liveBundleImage("live_linking_icon"),
                                                             .liveBundleImage("live_lined_icon"),].contains($0.normalImage) }

        featureClickPanel.updateFeatureItems(newItems: items)
        updateBottomAreaLayout()
    }

    func onClickGift() {
        PopupPanelController.alertView(giftPanelView)
    }

    func onClickLike() {
        giftPanelView.sendLike()
    }
}

extension AudienceLivingView: TUIBarrageDisplayViewDelegate {
    func barrageDisplayView(_ barrageDisplayView: TUIBarrageDisplayView, createCustomCell barrage: TUIBarrage) -> UIView? {
        guard let type = barrage.extInfo["TYPE"], type.value as? String == "GIFTMESSAGE" else {
            return nil
        }
        return CustomBarrageCell.getCustomCell(barrage: barrage)
    }
}

extension AudienceLivingView: TUIGiftPlayViewDelegate {
    func giftPlayView(_ giftPlayView: TUIGiftPlayView, onReceiveGift gift: TUIGift, giftCount: Int, sender: TUIGiftUser, receiver: TUIGiftUser) {
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

extension AudienceLivingView: TUIGiftListViewDelegate {
    func onRecharge(giftListView view: TUIGiftListView) {
        giftCloudServer.rechargeBalance { [weak self] error, balance in
            guard let self = self else { return }
            if error == .noError {
                view.setBalance(balance)
            } else {
                self.makeToast(.balanceInsufficientText)
            }
        }
    }
    
    func onSendGift(giftListView view: TUIGiftListView, giftModel: TUIGift, giftCount: Int) {
        let receiver = TUIGiftUser()
        receiver.userId = liveRoomInfo.anchorInfo.value.userId
        receiver.userName = liveRoomInfo.anchorInfo.value.name.value
        receiver.avatarUrl = liveRoomInfo.anchorInfo.value.avatarUrl.value
        receiver.level = "0"
        giftCloudServer.sendGift(sender: TUILogin.getUserID() ?? "",
                                 receiver: receiver.userId,
                                 giftModel: giftModel,
                                 giftCount: giftCount) { [weak self] error, balance in
            guard let self = self else { return }
            if error == .noError {
                view.sendGift(giftModel: giftModel, giftCount: giftCount, receiver: receiver)
                view.setBalance(balance)
            } else {
                self.makeToast(.balanceInsufficientText)
            }
        }
    }
}

private extension String {
    static var chatPlaceHolderText: String {
        localized("live.audience.barrage.placeholder")
    }

    static var cancelLinkMicRequestText = {
        localized("live.audience.link.confirm.cancelLinkMicRequest")
    }()

    static var closeLinkMicText = {
        localized("live.audience.link.confirm.closeLinkMic")
    }()

    static var waitToLinkText = {
        localized("live.audience.wait.link.tips")
    }()

    static var linkSuccessTipText = {
        localized("live.link.success")
    }()

    static var enterRoomFailedTitleText = {
        localized("live.alert.enterRoom.failed.title")
    }()

    static var enterRoomFailedmessageText = {
        localized("live.alert.enterRoom.failed.message.xxx")
    }()

    static var confirmText = {
        localized("live.alert.confirm")
    }()

    static var rejectedTitleText = {
        localized("live.alert.linkMic.rejected.title")
    }()

    static var rejectedMessageText = {
        localized("live.alert.linkMic.rejected.message")
    }()

    static var knownText = {
        localized("live.alert.known")
    }()

    static var timeoutTitleText = {
        localized("live.alert.linkMic.timeout.title")
    }()

    static var timeoutMessageText = {
        localized("live.alert.linkMic.timeout.message")
    }()

    static var operateFailedText: String {
        localized("live.operation.fail.xxx")
    }

    static var meText = {
        localized("live.barrage.me")
    }()
    
    static var balanceInsufficientText = {
        localized("live.balanceInsufficient")
    }()
}
