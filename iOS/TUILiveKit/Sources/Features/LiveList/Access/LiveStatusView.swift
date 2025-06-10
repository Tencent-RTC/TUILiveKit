//
//  LiveStatusView.swift
//  TUILiveKit
//
//  Created by jeremiawang on 2025/4/28.
//

import UIKit

class LiveStatusView: UIView {
    private let barCount = 3
    private let barWidth: CGFloat = 2
    private let barHeight: CGFloat = 7
    private let barSpacing: CGFloat = 3
    private let animDuration: TimeInterval = 1.0
    private let color: UIColor = .white

    private var barHeights: [CGFloat] = []
    private var displayLink: CADisplayLink?
    private var startTime: CFTimeInterval = 0

    override init(frame: CGRect) {
        super.init(frame: frame)
        setup()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func setup() {
        barHeights = Array(repeating: barHeight, count: barCount)
        backgroundColor = .clear
    }

    override func draw(_ rect: CGRect) {
        guard let _ = UIGraphicsGetCurrentContext() else { return }
        let totalWidth = barWidth * CGFloat(barCount) + barSpacing * CGFloat(barCount - 1)
        let startX = (bounds.width - totalWidth) / 2
        let centerY = bounds.height / 2

        for i in 0..<barCount {
            let left = startX + CGFloat(i) * (barWidth + barSpacing)
            let top = centerY - barHeights[i] / 2
            let barRect = CGRect(x: left, y: top, width: barWidth, height: barHeights[i])
            let path = UIBezierPath(roundedRect: barRect, cornerRadius: 2)
            color.setFill()
            path.fill()
        }
    }

    private func startAnimation() {
        stopAnimation()
        startTime = CACurrentMediaTime()
        displayLink = CADisplayLink(target: self, selector: #selector(updateAnimation))
        displayLink?.add(to: .main, forMode: .common)
    }

    private func stopAnimation() {
        displayLink?.invalidate()
        displayLink = nil
    }

    @objc private func updateAnimation() {
        let t = (CACurrentMediaTime() - startTime).truncatingRemainder(dividingBy: animDuration)
        let progress = t / animDuration
        for i in 0..<barCount {
            let phase = Double(i) * 0.6
            let adjustedProgress = (progress + phase).truncatingRemainder(dividingBy: 1.0)
            barHeights[i] = barHeight + CGFloat(sin(adjustedProgress * .pi) * 15)
        }
        setNeedsDisplay()
    }

    override func didMoveToWindow() {
        super.didMoveToWindow()
        if window != nil {
            startAnimation()
        } else {
            stopAnimation()
        }
    }

    deinit {
        stopAnimation()
    }
}
