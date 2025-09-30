//
//  TUIGiftCell.swift
//  TUILiveKit
//
//  Created by krab on 2024/1/2.
//

import UIKit
import AtomicXCore

class TUIGiftCell: UICollectionViewCell {
    var sendBlock: TUIActionSendBlock?
    var giftInfo: Gift? {
        didSet {
            guard let giftInfo = giftInfo else { return }
            setGiftInfo(giftInfo: giftInfo)
        }
    }
    override var isSelected: Bool {
        didSet {
            setSelectedState(isSelected)
        }
    }

    private lazy var giftBaseView: TUIGiftView = {
        let view = TUIGiftView(frame: .zero)
        view.mm_centerX = self.mm_w * 0.5
        view.mm_centerY = self.mm_h * 0.5
        view.sendBlock = { [weak self] giftModel in
            guard let self = self else { return }
            self.sendBlock?(giftModel)
        }
        return view
    }()

    override init(frame: CGRect) {
        super.init(frame: frame)
        setupUI()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func setGiftInfo(giftInfo: Gift) {
        giftBaseView.giftInfo = giftInfo
    }
    
    private func setSelectedState(_ isSelected: Bool) {
        giftBaseView.isSelected = isSelected
    }
}

// MARK: Layout

extension TUIGiftCell {
    func setupUI() {
        clipsToBounds = true
        addSubview(giftBaseView)
    }
}
