//
//  AnimationViewProtocol.swift
//  TUILiveKit
//
//  Created by krabyu on 2024/7/19.
//

import UIKit

protocol AnimationView: UIView {
    func playAnimation(playUrl: String, onFinished: @escaping ((Int)->Void))
}

