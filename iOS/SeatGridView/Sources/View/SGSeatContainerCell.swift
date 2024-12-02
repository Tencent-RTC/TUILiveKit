//
//  SGSeatContainerCell.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/28.
//

import UIKit
import Combine
import RTCRoomEngine

typealias updateVolumeClosure = (_ volume: Int, _ customView: UIView) -> Void
typealias updateCustomViewClosure = (_ seatInfo: TUISeatInfo, _ customView: UIView) -> Void

class SGSeatContainerCell: UICollectionViewCell {
    static let identifier = "SeatContainerCell"
    
    @Published var isSpeaking: Bool = false
    @Published var volume: Int = 0
    @Published var isAudioMuted: Bool = true
    @Published var seatInfo: TUISeatInfo? = nil
    var cancellableSet = Set<AnyCancellable>()

    var volumeClosure: updateVolumeClosure?
    var seatInfoClosure: updateCustomViewClosure?
    
    let contentContainerView: UIView = {
        let view = UIView()
        view.backgroundColor = .clear
        return view
    }()
  
    private var isDefaultSeatView: Bool = true
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        setupDisplayView()
        isViewReady = true
    }
    
    override func prepareForReuse() {
        super.prepareForReuse()
        contentContainerView.subviews.forEach { $0.removeFromSuperview() }
    }
    
    private func setupDisplayView() {
        contentContainerView.frame = contentView.frame
        contentContainerView.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        contentView.addSubview(contentContainerView)
    }
    
    func configure(with model: SeatContainerCellModel) {
        self.seatInfo = model.seatInfo
        if let customView = model.customView {
            customView.frame = contentContainerView.bounds
            customView.autoresizingMask = [.flexibleWidth, .flexibleHeight]
            contentContainerView.addSubview(customView)
            isDefaultSeatView = false
        } else {
            let defaultView = SGSeatView(seatInfo: seatInfo ?? TUISeatInfo())
            defaultView.frame = contentContainerView.bounds
            defaultView.autoresizingMask = [.flexibleWidth, .flexibleHeight]
            contentContainerView.addSubview(defaultView)
            isDefaultSeatView = true
        }
        
        $seatInfo
            .receive(on: RunLoop.main)
            .sink { [weak self] seatInfo in
                guard let self = self, let seatInfo = seatInfo else { return }
                if isDefaultSeatView, let seatView = self.contentContainerView.subviews.first as? SGSeatView {
                    seatView.seatInfo = seatInfo
                } else if !isDefaultSeatView, let customView = self.contentContainerView.subviews.first {
                    seatInfoClosure?(seatInfo, customView)
                }
            }
            .store(in: &cancellableSet)
        
        $isSpeaking
            .combineLatest($volume)
            .receive(on: RunLoop.main)
            .sink { [weak self] isSpeaking, volume in
                guard let self = self else { return }
                if isDefaultSeatView, let seatView = self.contentContainerView.subviews.first as? SGSeatView {
                    seatView.isSpeaking = isSpeaking
                } else if !isDefaultSeatView, let customView = self.contentContainerView.subviews.first {
                    volumeClosure?(volume, customView)
                }
            }
            .store(in: &cancellableSet)
        
        $isAudioMuted
            .receive(on: RunLoop.main)
            .sink { [weak self] isAudioMuted in
                guard let self = self else { return }
                if isDefaultSeatView, let seatView = self.contentContainerView.subviews.first as? SGSeatView {
                    seatView.isAudioMuted = isAudioMuted
                }
            }
            .store(in: &cancellableSet)
    }
    
    deinit {
        debugPrint("deinit:\(self)")
    }
}

class SeatContainerCellModel {
    let customView: UIView?
    let seatInfo: TUISeatInfo
    
    init(customView: UIView?, seatInfo: TUISeatInfo) {
        self.customView = customView
        self.seatInfo = seatInfo
    }
}
