//
//  TUILiveRoomAnchorViewController.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/11.
//  Copyright © 2023 Tencent. All rights reserved.
//

import Foundation
import TUICore
import RTCRoomEngine

public class TUILiveRoomAnchorViewController: UIViewController {
    private let roomId : String
    private lazy var anchorView : AnchorView = {
        return  AnchorView(roomId: self.roomId)
    }()
    public var startLiveBlock:(()->Void)?
    public init(roomId:String) {
        self.roomId = roomId
        super.init(nibName: nil, bundle: nil)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    public override func viewDidLoad() {
        super.viewDidLoad()
        initView()
    }

    public override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        super.touchesBegan(touches, with: event)
        view.endEditing(true)
    }

    public override func viewWillTransition(to size: CGSize, with coordinator: UIViewControllerTransitionCoordinator) {
        super.viewWillTransition(to: size, with: coordinator)
        let isPortrait = size.width < size.height
        anchorView.updateRootViewOrientation(isPortrait: isPortrait)
    }

    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        UIApplication.shared.isIdleTimerDisabled = true
    }

    public override func viewWillDisappear(_ animated: Bool) {
        super.viewWillDisappear(animated)
        UIApplication.shared.isIdleTimerDisabled = false
    }
    
    public override var shouldAutorotate: Bool {
        return false
    }
    public override var supportedInterfaceOrientations: UIInterfaceOrientationMask {
        return .portrait
    }
    
    deinit {
        // Reset audio effect View data.
        AudioEffectView.session.reset()
        print("deinit \(type(of: self))")
    }
}

extension TUILiveRoomAnchorViewController {
    private func initView() {
        navigationController?.setNavigationBarHidden(true, animated: true)
        view.addSubview(anchorView)
        anchorView.snp.makeConstraints({ make in
            make.edges.equalToSuperview()
        })
        anchorView.startLiveBlock = startLiveBlock
        view.backgroundColor = .black
    }
}
