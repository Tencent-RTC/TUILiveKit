//
//  TUILivePreviewViewController.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2024/4/3.
//  Copyright © 2023 Tencent. All rights reserved.
//

import Foundation
import TUICore
import UIKit
import RTCRoomEngine

public class TUILivePreviewViewController: UIViewController {
    
    private let liveRoomId : String
    private let voiceRoomId : String
    
    public init(liveRoomId:String, voiceRoomId:String) {
        self.liveRoomId = liveRoomId
        self.voiceRoomId = voiceRoomId
        super.init(nibName: nil, bundle: nil)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private var selectIndex: Int = -1 {
        didSet {
            resetState()
        }
    }
    
    lazy var listController: [UIViewController] = {
        // TODO: - clear controller after start live.
        let videoLiveController = TUILiveRoomAnchorViewController(roomId: liveRoomId)
        videoLiveController.title = .videoLiveTitle
        videoLiveController.startLiveBlock = { [weak self] in
            guard let self = self else { return }
            self.clearTopButton()
        }
        let voiceLiveController = TUIVoiceRoomViewController(roomId: voiceRoomId, behavior: .prepareCreate, voiceRoomParams: VoiceRoomParams())
        voiceLiveController.title = .voiceLiveTitle
        voiceLiveController.startLiveClosure = { [weak self] in
            guard let self = self else { return }
            DispatchQueue.global(qos: .userInitiated).async {
                TUIRoomEngine.sharedInstance().closeLocalCamera()
            }
            self.clearTopButton()
        }
        return [videoLiveController, voiceLiveController]
    }()

    private var topButtonWidth = 0.0
    private var listTopButton: [UIButton] = []
    private lazy var lineLayer: CALayer? = {
        let lineLayer = CALayer()
        lineLayer.backgroundColor = UIColor.white.cgColor
        return lineLayer
    }()

    override public func viewDidLoad() {
        super.viewDidLoad()
        setupViewControllers()
        constructViewHierarchy()
        activateConstraints()
    }

    override public func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()
        guard let lineLayer = lineLayer else { return }
        if selectIndex < 0 {
            selectIndex = 0
            view.layer.addSublayer(lineLayer)
        }
    }

    override public var shouldAutorotate: Bool {
        return false
    }

    override public var supportedInterfaceOrientations: UIInterfaceOrientationMask {
        return .portrait
    }
}

extension TUILivePreviewViewController {
    private func setupViewControllers() {
        for i in 0 ... listController.count - 1 {
            let controller = listController[i]
            addChild(controller)
            view.addSubview(controller.view)
            controller.didMove(toParent: self)
        }
    }

    private func constructViewHierarchy() {
        for i in 0 ... listController.count - 1 {
            let controller = listController[i]
            let button = createTopButton(title: controller.title ?? "")
            view.addSubview(button)
            listTopButton.append(button)
            topButtonWidth = topButtonWidth + button.frame.width
        }
    }

    private func activateConstraints() {
        let diff = 16.0
        var left = (view.frame.width - topButtonWidth - CGFloat(listController.count - 1) * diff) * 0.5
        for i in 0 ... listTopButton.count - 1 {
            let button = listTopButton[i]
            button.frame = CGRect(x: left, y: 55.scale375Height(), width: button.frame.width, height: button.frame.height)
            left = button.frame.maxX + diff
        }
    }
}

extension TUILivePreviewViewController {
    private func createTopButton(title: String) -> UIButton {
        let button = UIButton()
        button.setTitle(title, for: .normal)
        button.setTitleColor(.flowKitWhite.withAlphaComponent(0.7), for: .normal)
        button.setTitleColor(.flowKitWhite, for: .selected)
        button.titleLabel?.font = UIFont(name: "PingFangSC-Medium", size: 18)
        button.sizeToFit()
        button.isSelected = true
        button.addTarget(self, action: #selector(buttonTapped(_:)), for: .touchUpInside)
        return button
    }

    @objc private func buttonTapped(_ sender: UIButton) {
        view.endEditing(true)
        guard let index = listTopButton.firstIndex(where: { $0 == sender }) else { return }
        selectIndex = index
    }

    private func resetState() {
        for i in 0 ... listController.count - 1 {
            let controller = listController[i]
            controller.view.isHidden = i != selectIndex
        }

        for i in 0 ... listTopButton.count - 1 {
            let button = listTopButton[i]
            button.titleLabel?.font = UIFont(name: "PingFangSC-Medium", size: (i == selectIndex) ? 18 : 16)
            button.isSelected = i == selectIndex
            if button.isSelected {
                lineLayer?.frame = CGRect(x: button.frame.minX, y: button.frame.maxY - 2, width: button.frame.width, height: 2)
            }
        }
    }

    private func clearTopButton() {
        lineLayer?.isHidden = true
        for button in listTopButton {
            button.isHidden = true
        }
    }
}

private extension String {
    static var videoLiveTitle: String {
        localized("live.preview.video.live")
    }

    static var voiceLiveTitle: String {
        localized("live.preview.voice.live")
    }
}
