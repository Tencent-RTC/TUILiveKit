//
//  WaitLinkMicAnimationView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/31.
//

import Foundation
import TUICore

class WaitLinkMicDesc: UILabel {
    private var dotsCount: Int = 0
    private let dotsMaxCount: Int = 3
    private var dotsTimer: Timer?
    private var dotsDesc: String = ""

    func initDotsDesc(dotsDesc: String) {
        self.dotsDesc = dotsDesc
        text = .localizedReplace(dotsDesc, replace: "...")
        if TUIGlobalization.getRTLOption() {
            textAlignment = .right
        } else {
            textAlignment = .left
        }
        sizeToFit()
    }

    func showAnim() {
        dotsTimer?.invalidate()
        var dots = ""
        dotsTimer = Timer(timeInterval: 0.6, repeats: true) { [weak self] _ in
            guard let self = self else { return }
            if dots.count == self.dotsMaxCount {
                dots.removeAll()
            } else {
                dots.append(".")
            }
            self.text = .localizedReplace(self.dotsDesc, replace: dots)
        }
        guard let dotsTimer = dotsTimer else { return }
        RunLoop.current.add(dotsTimer, forMode: .default)
    }

    func stopAnim() {
        dotsTimer?.invalidate()
    }

    deinit {
        stopAnim()
    }
}

class WaitLinkMicAnimationView: UIView {
    var spotCount: Int = 0
    var spotMaxCount: Int = 3
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        waitLinkMicDesc.showAnim()
    }

    private let waitLinkMicImageView: UIImageView = {
        var view = UIImageView()
        view.image = .liveBundleImage("live_wait_link_mic_icon")
        return view
    }()

    private let waitLinkMicDesc: WaitLinkMicDesc = {
        let label = WaitLinkMicDesc(frame: .zero)
        label.font = .customFont(ofSize: 12)
        label.textColor = .greyColor
        label.initDotsDesc(dotsDesc: .waitLinkMicDesc)
        return label
    }()
}

// MARK: Layout
extension WaitLinkMicAnimationView {
    func constructViewHierarchy() {
        addSubview(waitLinkMicImageView)
        addSubview(waitLinkMicDesc)
    }

    func activateConstraints() {
        waitLinkMicImageView.snp.makeConstraints { make in
            make.width.height.equalTo(45.scale375())
            make.center.equalToSuperview()
        }

        waitLinkMicDesc.snp.makeConstraints { make in
            make.centerX.equalToSuperview()
            make.width.equalTo(waitLinkMicDesc.mm_w)
            make.height.equalTo(waitLinkMicDesc.mm_h)
            make.top.equalTo(waitLinkMicImageView.snp.bottom).offset(8.scale375Height())
        }
    }
}

private extension String {
    static var waitLinkMicDesc: String {
        localized("Waiting for link xxx")
    }
}
