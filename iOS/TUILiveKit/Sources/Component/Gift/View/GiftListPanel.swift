//
//  GiftListPanel.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/11/7.
//

import Foundation
import SnapKit
import Combine
import RTCRoomEngine

class GiftListPanel: UIView {
    private let roomId: String
    private var cancellableSet: Set<AnyCancellable> = []
    private lazy var giftListView = GiftListView(roomId: roomId)

    init(roomId: String) {
        self.roomId = roomId
        super.init(frame: .zero)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        isViewReady = true
    }

    private let titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.contentMode = .center
        label.font = .customFont(ofSize: 16, weight: .medium)
        label.textColor = .textPrimaryColor
        label.text = .giftTitle
        label.sizeToFit()
        return label
    }()
    
    deinit {
        debugPrint("\(type(of: self)) deinit")
    }
}

// MARK: Layout

extension GiftListPanel {
    private func constructViewHierarchy() {
        backgroundColor = .bgOperateColor
        layer.cornerRadius = 16
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        addSubview(titleLabel)
        addSubview(giftListView)
    }

    private func activateConstraints() {
        titleLabel.snp.remakeConstraints { make in
            make.top.equalToSuperview().offset(20.scale375Height())
            make.centerX.equalToSuperview()
            make.height.equalTo(24.scale375Width())
            make.width.equalTo(titleLabel.mm_w)
        }

        giftListView.snp.remakeConstraints { make in
            make.leading.trailing.equalToSuperview()
            make.height.equalTo(256)
            make.top.equalTo(titleLabel.snp.bottom).offset(20.scale375Height())
            make.bottom.equalToSuperview()
        }
    }
}

private extension String {
    static let giftTitle = internalLocalized("Gift")
    static let giftMutedText = internalLocalized("You have been muted in the current room and cannot send gifts")
}
