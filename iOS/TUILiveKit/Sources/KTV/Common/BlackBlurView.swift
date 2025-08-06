//
//  BlackBlurView.swift
//  TUILiveKit
//
//  Created by CY zhao on 2025/7/23.
//

import SwiftUI

struct BlackBlurView: UIViewRepresentable {
    var style: UIBlurEffect.Style = .dark
    var overlayOpacity: CGFloat = 0.3

    func makeUIView(context: Context) -> UIView {
        let blurEffect = UIBlurEffect(style: style)
        let blurView = UIVisualEffectView(effect: blurEffect)


        let overlay = UIView()
        overlay.backgroundColor = UIColor.black
        overlay.alpha = overlayOpacity
        overlay.translatesAutoresizingMaskIntoConstraints = false


        let container = UIView()
        blurView.frame = container.bounds
        blurView.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        container.addSubview(blurView)

        container.addSubview(overlay)
        NSLayoutConstraint.activate([
            overlay.topAnchor.constraint(equalTo: container.topAnchor),
            overlay.bottomAnchor.constraint(equalTo: container.bottomAnchor),
            overlay.leadingAnchor.constraint(equalTo: container.leadingAnchor),
            overlay.trailingAnchor.constraint(equalTo: container.trailingAnchor),
        ])

        return container
    }

    func updateUIView(_ uiView: UIView, context: Context) {

    }
}
