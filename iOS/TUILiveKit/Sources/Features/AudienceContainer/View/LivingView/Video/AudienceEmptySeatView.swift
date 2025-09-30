//
//  AudienceEmptySeatView.swift
//  TUILiveKit
//
//  Created by gg on 2025/7/22.
//

import RTCRoomEngine
import SnapKit
import AtomicXCore
import Combine

class AudienceEmptySeatView: UIView {
    
    private let manager: AudienceManager
    private let routerManager: AudienceRouterManager
    private let creator: AudienceRootMenuDataCreator
    private let seatInfo: TUISeatFullInfo
    private var cancellableSet = Set<AnyCancellable>()

    init(seatInfo: TUISeatFullInfo, manager: AudienceManager, routerManager: AudienceRouterManager, coreView: LiveCoreView) {
        self.seatInfo = seatInfo
        self.manager = manager
        self.routerManager = routerManager
        self.creator = AudienceRootMenuDataCreator(coreView: coreView, manager: manager, routerManager: routerManager)
        super.init(frame: .zero)
        
        backgroundColor = .bgOperateColor
        layer.borderWidth = 0.5
        layer.borderColor = UIColor.black.withAlphaComponent(0.25).cgColor
        
        let imageView = UIImageView(image: internalImage("add"))
        addSubview(imageView)
        imageView.snp.makeConstraints { make in
            make.bottom.equalTo(snp.centerY).offset(-2)
            make.centerX.equalToSuperview()
            make.size.equalTo(CGSize(width: 24, height: 24))
        }
        
        let titleLabel = UILabel(frame: .zero)
        titleLabel.font = .customFont(ofSize: 12, weight: .regular)
        titleLabel.textColor = .white.withAlphaComponent(0.9)
        titleLabel.text = .emptySeatText
        titleLabel.textAlignment = .center
        titleLabel.numberOfLines = 0
        addSubview(titleLabel)
        titleLabel.snp.makeConstraints { make in
            make.top.equalTo(snp.centerY).offset(2)
            make.leading.trailing.equalToSuperview()
        }
        
        let tap = UITapGestureRecognizer(target: self, action: #selector(onTap))
        addGestureRecognizer(tap)
        
        FloatWindow.shared.subscribeShowingState()
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] isShow in
                guard let self = self else { return }
                isHidden = isShow
            }
            .store(in: &cancellableSet)
    }
    
    @objc private func onTap(_ tap: UITapGestureRecognizer) {
        if manager.coGuestState.coGuestStatus != .none {
            return
        }
        let data = creator.generateLinkTypeMenuData(seatIndex: seatInfo.seatIndex)
        routerManager.router(action: .present(.linkType(data)))
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
}

fileprivate extension String {
    static let emptySeatText: String = internalLocalized("Conn Join")
}
