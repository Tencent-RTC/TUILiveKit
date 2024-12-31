//
//  TCEffectAnimationView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/7/19.
//

import UIKit
import TUICore

let KEY_SERVICE_NAME = "TUIEffectPlayerService"
let KEY_GET_VIEW = "TCEffectAnimView"
let KEY_METHOD_START_PLAY = "startPlay"
let KEY_METHOD_STOP_PLAY = "stopPlay"
let KEY_PARAM_PLAY_URL = "playUrl"
let KEY_PARAM_VIEW = "view"

class TCEffectAnimationView: UIView {
    var usable: Bool = false
    private var effectAnimationView: UIView?
    private var finishClosure: ((Int) -> Void)?

    init() {
        super.init(frame: .zero)
        effectAnimationView = createAnimationView()
        if effectAnimationView != nil {
            usable = true
        }
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    private func createAnimationView() -> UIView? {
        let playerList = TUICore.getExtensionList(KEY_GET_VIEW,
                                                  param: [:])
        if playerList.count == 0 {
            LiveKitLog.warn("\(#file)", "\(#line)","createAnimationView create view failed!")
            return nil
        }
        guard let playerAnimationView = playerList.first?.data?[KEY_GET_VIEW] as? UIView else { return nil }
        return playerAnimationView
    }
    
    private func constructViewHierarchy() {
        if let animationView = effectAnimationView {
            addSubview(animationView)
        }
    }
    
    private func activateConstraints() {
        if let animationView = effectAnimationView {
            animationView.snp.makeConstraints { make in
                make.edges.equalToSuperview()
            }
        }
    }
}

extension TCEffectAnimationView: AnimationView {
    func playAnimation(playUrl: String, onFinished: @escaping ((Int)->Void)) {
        finishClosure = onFinished
        reportGiftData()
        if !isMp4File(url: playUrl) {
            makeToast(.isNotMp4FileText)
            onFinished(-1)
            return
        }
        if let animationView = effectAnimationView {
            TUICore.callService(KEY_SERVICE_NAME,
                                method: KEY_METHOD_START_PLAY,
                                param: [KEY_PARAM_PLAY_URL:playUrl,
                                        KEY_PARAM_VIEW:animationView]) { code, message,_ in
                onFinished(code)
            }
        }
    }
    
    private func isMp4File(url: String) -> Bool {
        let mp4Extension = "mp4"
        let fileExtension = URL(fileURLWithPath: url).pathExtension
        return fileExtension.lowercased() == mp4Extension
    }
}

// MARK: DataReport

extension TCEffectAnimationView {
    private func reportGiftData() {
        let key = DataReporter.componentType == .liveRoom ? Constants.DataReport.kDataReportLiveGiftEffectPlayCount :
        Constants.DataReport.kDataReportLiveGiftEffectPlayCount
        DataReporter.reportEventData(eventKey: key)
    }
}

private extension String {
    static let isNotMp4FileText = localized("live.gift.animation.isNotMP4File")
}
