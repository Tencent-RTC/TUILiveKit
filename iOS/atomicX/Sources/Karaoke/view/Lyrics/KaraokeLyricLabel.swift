//
//  KaraokeLyricLabel.swift
//  Pods
//
//  Created by ssc on 2025/8/25.
//

class KaraokeLyricLabel: UILabel {
    var progress: CGFloat = 0
    private var displayLink: CADisplayLink?
    private var startProgress: CGFloat = 0
    private var targetProgress: CGFloat = 0
    private var animationStart: CFTimeInterval = 0
    private var animationDuration: TimeInterval = 0

    func setProgress(_ newValue: CGFloat, animated: Bool = false, duration: TimeInterval = 0.2) {
        let clampedValue = min(max(newValue, 0), 1)
        if animated {
            startProgress = progress
            targetProgress = clampedValue
            animationDuration = duration
            animationStart = CACurrentMediaTime()
            displayLink?.invalidate()
            displayLink = CADisplayLink(target: self, selector: #selector(updateProgressAnimation))
            displayLink?.add(to: .main, forMode: .default)
        } else {
            progress = clampedValue
            setNeedsDisplay()
        }
    }

    override func draw(_ rect: CGRect) {
        let textColor = self.textColor ?? UIColor.gray
        if let text = self.text, let font = self.font {
            let style = NSMutableParagraphStyle()
            style.alignment = self.textAlignment
            let attributes: [NSAttributedString.Key: Any] = [
                .font: font,
                .foregroundColor: textColor,
                .paragraphStyle: style
            ]
            (text as NSString).draw(in: rect, withAttributes: attributes)
        }

        if progress > 0 {
            let fillRect = CGRect(x: rect.origin.x,
                                  y: rect.origin.y,
                                  width: rect.width * progress,
                                  height: rect.height)
            if let ctx = UIGraphicsGetCurrentContext() {
                ctx.saveGState()
                ctx.clip(to: fillRect)
                if let text = self.text, let font = self.font {
                    let style = NSMutableParagraphStyle()
                    style.alignment = self.textAlignment
                    let attributes: [NSAttributedString.Key: Any] = [
                        .font: font,
                        .foregroundColor:  UIColor("00ABD6"),
                        .paragraphStyle: style
                    ]
                    (text as NSString).draw(in: rect, withAttributes: attributes)
                }
                ctx.restoreGState()
            }
        }
    }

    @objc private func updateProgressAnimation() {
        let dt = min((CACurrentMediaTime() - animationStart) / animationDuration, 1)
        progress = startProgress + (targetProgress - startProgress) * CGFloat(dt)
        setNeedsDisplay()
        if dt >= 1.0 {
            displayLink?.invalidate()
            displayLink = nil
            progress = targetProgress
            setNeedsDisplay()
        }
    }
}
