//
//  SVGAAnimationView.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/7/19.
//

import UIKit
import SVGAPlayer

class SVGAAnimationView: UIView, AnimationView {
    var finishClosure: ((Int) -> Void)?
    func playAnimation(playUrl: String, onFinished: @escaping ((Int)->Void)) {
        finishClosure = onFinished
        reportGiftData()
        if !isSVGAFile(url: playUrl) {
            makeToast(.isNotSVGAFileText)
            finishClosure?(-1)
            return
        }
        
        let playerView = SVGAPlayer(frame: bounds)
        playerView.contentMode = .scaleAspectFill
        playerView.delegate = self
        playerView.loops = 1
        addSubview(playerView)
        playerView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        
        DispatchQueue.global().async { [weak self] in
            guard let url = URL(string: playUrl), let animationData = FileManager.default.contents(atPath: playUrl) else {
                guard let self = self else { return }
                DispatchQueue.main.async { [weak self] in
                    guard let self = self else { return }
                    playerView.removeFromSuperview()
                    finishClosure?(-1)
                }
                return
            }
            let parser = SVGAParser()
            parser.parse(with: animationData, cacheKey: url.lastPathComponent) { [weak self] videoItem in
                DispatchQueue.main.async { [weak self] in
                    guard let self = self else { return }
                    playerView.videoItem = videoItem
                    playerView.startAnimation()
                }
            } failureBlock: { [weak self] erro in
                DispatchQueue.main.async { [weak self] in
                    guard let self = self else { return }
                    self.svgaPlayerDidFinishedAnimation(playerView)
                }
            }
        }
    }
    
    private func isSVGAFile(url: String) -> Bool {
        let svgaExtension = "svga"
        let fileExtension = URL(fileURLWithPath: url).pathExtension
        return fileExtension.lowercased() == svgaExtension
    }
}

// MARK: SVGAPlayerDelegate

extension SVGAAnimationView: SVGAPlayerDelegate {
    func svgaPlayerDidFinishedAnimation(_ player: SVGAPlayer) {
        UIView.animate(withDuration: 0.2) {
            player.alpha = 0
        } completion: { _ in
            player.removeFromSuperview()
        }
        finishClosure?(0)
    }
}

// MARK: DataReport

extension SVGAAnimationView {
    private func reportGiftData() {
        let key = DataReporter.componentType == .liveRoom ? Constants.DataReport.kDataReportLiveGiftSVGAPlayCount :
        Constants.DataReport.kDataReportVoiceGiftSVGAPlayCount
        DataReporter.reportEventData(eventKey: key)
    }
}

private extension String {
    static let isNotSVGAFileText = localized("live.gift.animation.isNotSVGAFile")
}
