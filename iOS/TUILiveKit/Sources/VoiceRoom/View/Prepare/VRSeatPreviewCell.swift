//
//  VRSeatPreviewCell.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/14.
//

import UIKit
import SnapKit
import RTCRoomEngine
import Combine


class VRSeatPreviewCell: UICollectionViewCell {
    static let identifier = "VRSeatPreviewCell"
    private var isViewReady: Bool = false
    
    let seatView: VRSeatView = {
        let view = VRSeatView()
        return view
    }()
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    func constructViewHierarchy() {
        contentView.addSubview(seatView)
    }
    
    func activateConstraints() {
        seatView.snp.makeConstraints { (make) in
            make.top.left.bottom.right.equalToSuperview()
        }
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }

}

class VRSeatView: UIView {
    private var isViewReady: Bool = false
    let seatContentView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .seatContentColor
        return view
    }()
    
    let seatImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.image = .liveBundleImage("live_seat_empty_icon")
        return imageView
    }()
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        setupViewConfig()
    }
    
    required init?(coder: NSCoder) {
        fatalError("can't init this viiew from coder")
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }

    override func draw(_ rect: CGRect) {
        super.draw(rect)
        seatContentView.layer.cornerRadius = seatContentView.frame.height * 0.5
        seatContentView.layer.borderWidth = 0.5
        seatContentView.layer.borderColor = UIColor.seatContentBorderColor.cgColor
    }
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }
    
    func setupViewConfig() {
        backgroundColor = .clear
    }
    
    func constructViewHierarchy() {
        addSubview(seatContentView)
        seatContentView.addSubview(seatImageView)
    }
    
    func activateConstraints() {
        seatContentView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(10.scale375())
            make.size.equalTo(CGSizeMake(50, 50))
            make.centerX.equalToSuperview()
        }
        seatImageView.snp.makeConstraints { make in
            make.center.equalToSuperview()
        }
    }
}
