//
//  LiveListViewController.swift
//  TUILiveKitApp
//
//  Created by jack on 2024/10/9.
//

import UIKit
import TUICore
import TUILiveKit

class VideoLiveViewController: UIViewController {
    
    private let navBackgroundImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = UIImage(named: "top_background")
        return imageView
    }()
    
    private lazy var goLiveButton: UIButton = {
        let button = UIButton()
        button.setImage(UIImage(named: "goLive"), for: .normal)
        button.addTarget(self, action: #selector(goLiveClick), for: .touchUpInside)
        return button
    }()
    
    private lazy var liveListViewController = {
        return TUILiveListViewController()
    }()

    override func viewDidLoad() {
        super.viewDidLoad()
        setupNavigation()
        constructViewHierarchy()
        activateConstraints()
    }
    
    
    private func constructViewHierarchy() {
        view.addSubview(navBackgroundImageView)
        view.addSubview(goLiveButton)
        
        addChild(liveListViewController)
        view.addSubview(liveListViewController.view)
        view.bringSubviewToFront(goLiveButton)
    }
    
    private func activateConstraints() {
        navBackgroundImageView.snp.makeConstraints { make in
            make.top.leading.trailing.equalToSuperview()
            make.height.equalTo(200.scale375Height())
        }
        liveListViewController.view.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        goLiveButton.snp.makeConstraints { make in
            make.bottom.equalTo(-(15.scale375Height() + kDeviceSafeBottomHeight))
            make.centerX.equalToSuperview()
            make.height.width.equalTo(40.scale375())
        }
    }
    
}

// MARK: - Private
extension VideoLiveViewController {
    
    private func setupNavigation() {
        let helpButton = UIButton()
        helpButton.setImage(UIImage(named: "help_small"), for: .normal)
        helpButton.addTarget(self, action: #selector(helpClick), for: .touchUpInside)
        helpButton.sizeToFit()
        let helpItem = UIBarButtonItem(customView: helpButton)
        helpItem.tintColor = .black
        navigationItem.rightBarButtonItem = helpItem
        
        navigationItem.title = .videoLiveTitle
    }

}

// MARK: - Actions
extension VideoLiveViewController {
    
    @objc private func helpClick() {
        if let url = URL(string: "https://cloud.tencent.com/document/product/647/105441") {
            UIApplication.shared.open(url, options: [:], completionHandler: nil)
        }
    }
    
    @objc private func goLiveClick() {
        let liveRoomId = LiveIdentityGenerator.shared.generateId(TUILogin.getUserID() ?? "", type: .live)
        VideoLiveKit.createInstance().startLive(roomId: liveRoomId)
    }
}


fileprivate extension String {
    static let videoLiveTitle = TUILiveKitAppLocalize("Video Live")
}
