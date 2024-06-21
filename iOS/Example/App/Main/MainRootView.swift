//
//  MainRootView.swift
//  TUILiveKitApp
//
//  Created by adams on 2021/6/4.
//

import SnapKit
import TUILiveKit
import UIKit

class MainRootView: UIView {
    private let topBackgroundImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = UIImage(named: "top_background")
        return imageView
    }()

    var rootVC: MainViewController?

    let contentView: UIView = {
        let view = UIView()
        view.backgroundColor = .clear
        return view
    }()

    private let bottomView: UIView = {
        let view = UIView()
        view.backgroundColor = .white
        return view
    }()

    private let lineView: UIView = {
        let view = UIView()
        view.backgroundColor = .black.withAlphaComponent(0.4)
        return view
    }()

    lazy var liveListButton: AlignmentButton = {
        let button = AlignmentButton()
        button.imageAlignment = .top
        button.spaceBetweenTitleAndImage = 2.scale375Height()
        button.setImage(UIImage(named: "liveList_normal"), for: .normal)
        button.setImage(UIImage(named: "liveList_selected"), for: .selected)
        button.setTitle(.liveText, for: .normal)
        button.setTitleColor(.black, for: .normal)
        button.setTitleColor(UIColor(hex: "0099FF"), for: .selected)
        button.titleLabel?.textAlignment = .center
        button.titleLabel?.font = UIFont(name: "PingFangSC-Regular", size: 12)
        button.imageView?.contentMode = .scaleAspectFit
        button.addTarget(self, action: #selector(liveListButtonClick), for: .touchUpInside)
        return button
    }()

    private lazy var goLiveButton: UIButton = {
        let button = UIButton()
        button.setImage(UIImage(named: "goLive"), for: .normal)
        button.addTarget(self, action: #selector(goLiveButtonClick), for: .touchUpInside)
        return button
    }()

    lazy var meButton: AlignmentButton = {
        let button = AlignmentButton()
        button.imageAlignment = .top
        button.spaceBetweenTitleAndImage = 2.scale375Height()
        button.setImage(UIImage(named: "me_normal"), for: .normal)
        button.setImage(UIImage(named: "me_selected"), for: .selected)
        button.setTitle(.meText, for: .normal)
        button.setTitleColor(.black, for: .normal)
        button.setTitleColor(UIColor(hex: "0099FF"), for: .selected)
        button.titleLabel?.textAlignment = .center
        button.titleLabel?.font = UIFont(name: "PingFangSC-Regular", size: 12)
        button.imageView?.contentMode = .scaleAspectFit
        button.addTarget(self, action: #selector(meButtonClick), for: .touchUpInside)
        
        return button
    }()

    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
}

// MARK: Layout

extension MainRootView {
    private func constructViewHierarchy() {
        addSubview(topBackgroundImageView)
        addSubview(contentView)
        addSubview(bottomView)
        bottomView.addSubview(lineView)
        bottomView.addSubview(liveListButton)
        bottomView.addSubview(goLiveButton)
        bottomView.addSubview(meButton)
    }

    private func activateConstraints() {
        topBackgroundImageView.snp.makeConstraints { make in
            make.top.leading.trailing.equalToSuperview()
            make.height.equalTo(200.scale375Height())
        }
        contentView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(99.scale375Height())
            make.leading.trailing.equalToSuperview()
            make.bottom.equalTo(bottomView.snp.top)
        }
        bottomView.snp.makeConstraints { make in
            make.bottom.leading.trailing.equalToSuperview()
            make.height.equalTo(80.scale375Height())
        }
        lineView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(7.scale375Height())
            make.leading.trailing.equalToSuperview()
            make.height.equalTo(0.3)
        }
        liveListButton.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(13.scale375Height())
            make.leading.equalToSuperview().offset(41.scale375())
            make.width.equalTo(60.scale375())
            make.height.equalTo(60.scale375())
        }
        goLiveButton.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(15.scale375Height())
            make.centerX.equalToSuperview()
            make.height.width.equalTo(40.scale375())
        }
        meButton.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(13.scale375Height())
            make.trailing.equalToSuperview().offset(-41.scale375())
            make.width.equalTo(60.scale375())
            make.height.equalTo(60.scale375())
        }
    }
}

// MARK: Action

extension MainRootView {
    @objc private func liveListButtonClick() {
        rootVC?.liveListButtonClick()
    }

    @objc private func goLiveButtonClick() {
        rootVC?.startButtonClick()
    }

    @objc private func meButtonClick() {
        rootVC?.meButtonClick()
    }
}

private extension String {
    static let liveText = TUILiveKitAppLocalize("TUILiveKitApp.Main.Live")
    static let meText = TUILiveKitAppLocalize("TUILiveKitApp.Main.Me")
}
