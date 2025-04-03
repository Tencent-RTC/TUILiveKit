//
//  VoiceRoomRootView.swift
//  VoiceRoom
//
//  Created by aby on 2024/3/4.
//

import UIKit
import RTCCommon
import SnapKit
import Combine
import LiveStreamCore
import RTCRoomEngine

class VRBottomMenuView: UIView {
    var cancellableSet = Set<AnyCancellable>()
    
    private let manager: VoiceRoomManager
    private let routerManager: VRRouterManager
    private let coreView: SeatGridView
    private let isOwner: Bool
    
    private let buttonSliceIndex: Int = 1
    
    private let maxMenuButtonNumber = 5
    private let buttonWidth: CGFloat = 36.0
    private let buttonSpacing: CGFloat = 6.0
    private var isPending: Bool = false
    
    var menus = [VRButtonMenuInfo]()
    
    let stackView: UIStackView = {
        let view = UIStackView()
        view.axis = .horizontal
        view.alignment = .center
        view.distribution = .fillProportionally
        return view
    }()
    private let designConfig: ActionItemDesignConfig = {
        let designConfig = ActionItemDesignConfig(lineWidth: 1, titleColor: .g2)
        designConfig.backgroundColor = .white
        designConfig.lineColor = .g8
        return designConfig
    }()
    
    private var buttons: [UIButton] = []
    
    init(manager: VoiceRoomManager, routerManager: VRRouterManager, coreView: SeatGridView, isOwner: Bool) {
        self.manager = manager
        self.routerManager = routerManager
        self.coreView = coreView
        self.isOwner = isOwner
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        setupViewStyle()
        setupMenuButtons()
        isViewReady = true
    }
    
    private func constructViewHierarchy() {
        addSubview(stackView)
    }
    
    private func activateConstraints() {
        stackView.snp.makeConstraints { make in
            let maxWidth = buttonWidth * CGFloat(maxMenuButtonNumber) + buttonSpacing * CGFloat(maxMenuButtonNumber - 1)
            make.centerY.equalToSuperview()
            make.trailing.equalTo(-16)
            make.width.lessThanOrEqualTo(maxWidth)
            make.leading.equalToSuperview()
        }
    }
    
    private func setupViewStyle() {
        stackView.spacing = buttonSpacing
    }
    
    private func setupMenuButtons() {
        menus = generateBottomMenuData()
        
        stackView.subviews.forEach { view in
            stackView.removeArrangedSubview(view)
            view.removeFromSuperview()
        }
        buttons = menus
            .enumerated().map { value -> MenuButton in
                let index = value.offset
                let item = value.element
                let button = self.createButtonFromMenuItem(index: index, item: item)
                stackView.addArrangedSubview(button)
                button.snp.makeConstraints { make in
                    make.height.width.equalTo(32.scale375Height())
                }
                button.addTarget(self, action: #selector(menuTapAction(sender:)), for: .touchUpInside)
                return button
            }
    }
    
    private func createButtonFromMenuItem(index: Int, item: VRButtonMenuInfo) -> MenuButton {
        let button = MenuButton(frame: .zero)
        button.setImage(.liveBundleImage(item.normalIcon), for: .normal)
        button.setImage(.liveBundleImage(item.selectIcon), for: .selected)
        button.setTitle(item.normalTitle, for: .normal)
        button.setTitle(item.selectTitle, for: .selected)
        button.tag = index + 1_000
        item.bindStateClosure?(button, &cancellableSet)
        return button
    }
}

class MenuButton: UIButton {
    
    let rotateAnimation: CABasicAnimation = {
        let animation = CABasicAnimation(keyPath: "transform.rotation.z")
        animation.fromValue = 0
        animation.toValue = Double.pi * 2
        animation.duration = 2
        animation.autoreverses = false
        animation.fillMode = .forwards
        animation.repeatCount = MAXFLOAT
        animation.isRemovedOnCompletion = false
        return animation
    }()
    
    let redDotContentView: UIView = {
        let view = UIView()
        view.backgroundColor = .redDotColor
        view.layer.cornerRadius = 10.scale375Height()
        view.layer.masksToBounds = true
        view.isHidden = true
        return view
    }()
    
    let redDotLabel: UILabel = {
        let redDot = UILabel()
        redDot.textColor = .white
        redDot.textAlignment = .center
        redDot.font = UIFont(name: "PingFangSC-Semibold", size: 12)
        return redDot
    }()
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        addSubview(redDotContentView)
        redDotContentView.addSubview(redDotLabel)
        
        redDotContentView.snp.makeConstraints { make in
            make.centerY.equalTo(snp.top).offset(5.scale375Height())
            make.centerX.equalTo(snp.right).offset(-5.scale375Height())
            make.height.equalTo(20.scale375Height())
            make.width.greaterThanOrEqualTo(20.scale375Height())
        }
        
        redDotLabel.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(4.scale375())
            make.trailing.equalToSuperview().offset(-4.scale375())
            make.centerY.equalToSuperview()
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    func updateDotCount(count: Int) {
        if count == 0 {
            redDotContentView.isHidden = true
        } else {
            redDotContentView.isHidden = false
            redDotLabel.text = "\(count)"
        }
    }
    
    func startRotate() {
        layer.add(rotateAnimation, forKey: nil)
    }
    
    func endRotate() {
        layer.removeAllAnimations()
    }
}

extension VRBottomMenuView {
    @objc func menuTapAction(sender: MenuButton) {
        let index = sender.tag - 1_000
        let bottomMenu = menus[index]
        bottomMenu.tapAction?(sender)
    }
}

// MARK: MenuDataCreator
extension VRBottomMenuView {
    func generateBottomMenuData() -> [VRButtonMenuInfo] {
        if isOwner {
            return ownerBottomMenu()
        } else {
            return memberBottomMenu()
        }
    }
    
    private func ownerBottomMenu() -> [VRButtonMenuInfo] {
        var menus: [VRButtonMenuInfo] = []
        
        var setting = VRButtonMenuInfo(normalIcon: "live_anchor_setting_icon")
        setting.tapAction = { [weak self] sender in
            guard let self = self else { return }
            let settingItems = self.generateOwnerSettingModel()
            self.routerManager.router(action: .present(.featureSetting(settingItems)))
        }
        menus.append(setting)
        
        var linkMic = VRButtonMenuInfo(normalIcon: "live_link_voice_room", normalTitle: "")
        linkMic.tapAction = { [weak self] sender in
            guard let self = self else { return }
            self.routerManager.router(action: .present(.voiceLinkControl(self.coreView)))
        }
        
        linkMic.bindStateClosure = { [weak manager] button, cancellableSet in
            guard let manager = manager else { return }
            manager.subscribeSeatState(StateSelector(keyPath: \.seatApplicationList))
                .receive(on: RunLoop.main)
                .sink(receiveValue: { list in
                    button.updateDotCount(count: list.count)
                })
                .store(in: &cancellableSet)
        }
        menus.append(linkMic)
        return menus
    }
    
    private func generateOwnerSettingModel() -> VRFeatureClickPanelModel {
        let model = VRFeatureClickPanelModel()
        model.itemSize = CGSize(width: 56.scale375(), height: 76.scale375())
        model.itemDiff = 44.scale375()
        var designConfig = VRFeatureItemDesignConfig()
        designConfig.backgroundColor = .g3
        designConfig.cornerRadius = 10
        designConfig.titleFont = .customFont(ofSize: 12)
        designConfig.type = .imageAboveTitleBottom
        model.items.append(VRFeatureItem(normalTitle: .backgroundText,
                                       normalImage: .liveBundleImage("live_setting_background_icon"),
                                       designConfig: designConfig,
                                         actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.routerManager.router(action: .present(.systemImageSelection(.background)))
        }))
        model.items.append(VRFeatureItem(normalTitle: .audioEffectsText,
                                       normalImage: .liveBundleImage("live_setting_audio_effects"),
                                       designConfig: designConfig,
                                         actionClosure: { [weak self] _ in
            guard let self = self else { return }
            self.routerManager.router(action: .present(.audioEffect))
        }))
        return model
    }
    
    private func memberBottomMenu() -> [VRButtonMenuInfo] {
        var menus: [VRButtonMenuInfo] = []
        var gift = VRButtonMenuInfo(normalIcon: "live_gift_icon", normalTitle: "")
        gift.tapAction = { [weak self] sender in
            guard let self = self else { return }
            self.routerManager.router(action: .present(.giftView))
        }
        menus.append(gift)
        
        var like = VRButtonMenuInfo(normalIcon: "live_like_icon")
        like.tapAction = { [weak self] sender in
            guard let self = self else { return }
            manager.likeSubject.send()
        }
        menus.append(like)
        
        var linkMic = VRButtonMenuInfo(normalIcon: "live_voice_room_link_icon", selectIcon: "live_voice_room_linking_icon")
        linkMic.tapAction = { [weak self] sender in
            guard let self = self, !isPending else { return }
            let selfUserId = manager.userState.selfInfo.userId
            let isApplying = manager.seatState.isApplyingToTakeSeat
            if isApplying {
                isPending = true
                coreView.cancelRequest(userId: selfUserId) { [weak self] in
                    guard let self = self else { return }
                    isPending = false
                    self.handleApplicationState(isApplying: false)
                } onError: { [weak self] code, message in
                    guard let self = self else { return }
                    isPending = false
                    let error = InternalError(code: code, message: message)
                    self.manager.toastSubject.send(error.localizedMessage)
                }
            } else {
                let isOnSeat = manager.seatState.seatList.contains(where: { $0.userId == selfUserId })
                if isOnSeat {
                    coreView.leaveSeat {
                    } onError: { [weak self] code, message in
                        guard let self = self else { return }
                        let error = InternalError(code: code, message: message)
                        self.manager.toastSubject.send(error.localizedMessage)
                    }

                } else {
                    // request
                    if manager.seatState.isApplyingToTakeSeat {
                        manager.toastSubject.send(.repeatRequest)
                        return
                    }
                    let kTimeoutValue = 60
                    coreView.takeSeat(index: -1, timeout: kTimeoutValue) { [weak self] userInfo in
                        guard let self = self else { return }
                        self.handleApplicationState(isApplying: false)
                    } onRejected: { [weak self] userInfo in
                        guard let self = self else { return }
                        self.handleApplicationState(isApplying: false)
                        self.manager.toastSubject.send(.takeSeatApplicationRejected)
                    } onCancelled: { [weak self] userInfo in
                        guard let self = self else { return }
                        self.handleApplicationState(isApplying: false)
                    } onTimeout: { [weak self] userInfo in
                        guard let self = self else { return }
                        self.handleApplicationState(isApplying: false)
                        self.manager.toastSubject.send(.takeSeatApplicationTimeout)
                    } onError: { [weak self] userInfo, code, message in
                        guard let self = self else { return }
                        if code != LiveError.requestIdRepeat.rawValue && code != LiveError.alreadyOnTheSeatQueue.rawValue {
                            self.handleApplicationState(isApplying: false)
                        }
                        guard let err = TUIError(rawValue: code) else { return }
                        let error = InternalError(code: code, message: message)
                        self.manager.toastSubject.send(error.localizedMessage)
                    }
                    handleApplicationState(isApplying: true)
                }
            }
        }
        linkMic.bindStateClosure = { [weak self] button, cancellableSet in
            guard let self = self else { return }
            
            manager.subscribeSeatState(StateSelector(keyPath: \.isApplyingToTakeSeat))
                .sink { isApplying in
                    DispatchQueue.main.async {
                        button.isSelected = isApplying
                        button.isSelected ? button.startRotate() : button.endRotate()
                    }
                }
                .store(in: &cancellableSet)
            
            
            manager.subscribeSeatState(StateSelector(keyPath: \.seatList))
                .receive(on: RunLoop.main)
                .sink { [weak self] seatInfoList in
                    guard let self = self else { return }
                    let isOnSeat = seatInfoList.contains(where: { $0.userId == self.manager.userState.selfInfo.userId })
                    let imageName = isOnSeat ? "live_linked_icon" : "live_voice_room_link_icon"
                    button.setImage(.liveBundleImage(imageName), for: .normal)
                }
                .store(in: &cancellableSet)
        }
        menus.append(linkMic)
        return menus
    }
    
    private func handleApplicationState(isApplying: Bool) {
        manager.update(applicationStateIsApplying: isApplying)
    }
    
}

private extension String {
    static let backgroundText = localized("Background")
    static let audioEffectsText = localized("Audio")
    static let repeatRequest = localized("Signal request repetition")
    static let takeSeatApplicationRejected = localized("Take seat application has been rejected")
    static let takeSeatApplicationTimeout = localized("Take seat application timeout")
}
