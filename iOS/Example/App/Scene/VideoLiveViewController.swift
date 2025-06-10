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
    private var currentStyle = LiveListViewStyle.doubleColumn
    
    private lazy var goLiveButton: UIButton = {
        let button = UIButton()
        button.layer.cornerRadius = 26.scale375()
        button.setImage(UIImage(named: "create_live"), for: .normal)
        button.setTitle(.goLiveText, for: .normal)
        button.addTarget(self, action: #selector(goLiveClick), for: .touchUpInside)
        button.titleLabel?.font = UIFont(name: "PingFangSC-Semibold", size: 20)
        button.backgroundColor = UIColor("1C66E5")
        
        let spacing: CGFloat = 8
        button.imageEdgeInsets = UIEdgeInsets(top: 0, left: -spacing / 2, bottom: 0, right: spacing / 2)
        button.titleEdgeInsets = UIEdgeInsets(top: 0, left: spacing / 2, bottom: 0, right: -spacing / 2)
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
        view.backgroundColor = .bgTopBarColor
    }
    
    
    private func constructViewHierarchy() {
        view.addSubview(goLiveButton)
        
        addChild(liveListViewController)
        view.addSubview(liveListViewController.view)
        view.bringSubviewToFront(goLiveButton)
    }
    
    private func activateConstraints() {
        liveListViewController.view.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        goLiveButton.snp.makeConstraints { make in
            make.bottom.equalTo(-(15.scale375Height() + kDeviceSafeBottomHeight))
            make.centerX.equalToSuperview()
            make.height.equalTo(48.scale375())
            make.width.equalTo(154.scale375())
        }
    }
    
}

// MARK: - Private
extension VideoLiveViewController {
    
    private func setupNavigation() {
        let appearance = UINavigationBarAppearance()
        appearance.configureWithTransparentBackground()
        appearance.backgroundColor = .clear
        appearance.titleTextAttributes = [.foregroundColor: UIColor.white]
        navigationItem.standardAppearance = appearance
        navigationItem.scrollEdgeAppearance = appearance
        
        let backBtn = UIButton(type: .custom)
        backBtn.setImage(UIImage(named: "live_back")?.withTintColor(.white), for: .normal)
        backBtn.addTarget(self, action: #selector(backBtnClick), for: .touchUpInside)
        backBtn.sizeToFit()
        let backItem = UIBarButtonItem(customView: backBtn)
        navigationItem.leftBarButtonItem = backItem
        
        let switchColumnBtn = UIButton(type: .custom)
        switchColumnBtn.setImage(UIImage(named: "live_single_column_icon"), for: .normal)
        switchColumnBtn.setImage(UIImage(named: "live_double_column_icon"), for: .selected)
        switchColumnBtn.addTarget(self, action: #selector(swithColumnBtnClick), for: .touchUpInside)
        switchColumnBtn.sizeToFit()
        let switchItem = UIBarButtonItem(customView: switchColumnBtn)
        switchItem.tintColor = .white
        navigationItem.rightBarButtonItem = switchItem
    
        let titleView = UILabel()
        titleView.text = .videoLiveTitle
        titleView.textColor = .white
        titleView.textAlignment = .center
        titleView.font = UIFont.boldSystemFont(ofSize: 17)
        titleView.adjustsFontSizeToFitWidth = true
        let width = titleView.sizeThatFits(CGSize(width: CGFloat.greatestFiniteMagnitude,
                                                  height: CGFloat.greatestFiniteMagnitude)).width
        titleView.frame = CGRect(origin: CGPoint.zero, size: CGSize(width: width, height: 44))
        self.navigationItem.titleView = titleView
    }

}

// MARK: - Actions
extension VideoLiveViewController {
    @objc private func backBtnClick(sender: UIButton) {
        if let nav = navigationController {
            nav.popViewController(animated: true)
        } else {
            dismiss(animated: true)
        }
    }
    
    @objc private func swithColumnBtnClick(sender: UIButton) {
        let newStyle: LiveListViewStyle = currentStyle == .doubleColumn ? .singleColumn : .doubleColumn
        currentStyle = newStyle
        liveListViewController.setColumnStyle(style: newStyle)
        sender.isSelected = currentStyle == .singleColumn
        goLiveButton.isHidden = currentStyle == .singleColumn
    }
    
    @objc private func goLiveClick() {
        let liveRoomId = LiveIdentityGenerator.shared.generateId(TUILogin.getUserID() ?? "", type: .live)
        VideoLiveKit.createInstance().startLive(roomId: liveRoomId)
    }
}


fileprivate extension String {
    static let videoLiveTitle = TUILiveKitAppLocalize("Video Live")
    static let goLiveText = TUILiveKitAppLocalize("Go Live")
}
