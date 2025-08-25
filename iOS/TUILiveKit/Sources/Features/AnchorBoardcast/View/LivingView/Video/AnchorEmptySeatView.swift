//
//  AnchorEmptySeatView.swift
//  TUILiveKit
//
//  Created by gg on 2025/7/22.
//

import RTCRoomEngine
import SnapKit
import Combine

class AnchorEmptySeatView: UIView {
    private var cancellableSet = Set<AnyCancellable>()

    init(seatInfo: TUISeatFullInfo) {
        super.init(frame: .zero)
        
        backgroundColor = .bgOperateColor
        layer.borderWidth = 0.5
        layer.borderColor = UIColor.black.withAlphaComponent(0.25).cgColor
        
        let indexLabel = UILabel(frame: .zero)
        indexLabel.font = .customFont(ofSize: 22, weight: .semibold)
        indexLabel.textColor = .white.withAlphaComponent(0.55)
        indexLabel.text = "\(seatInfo.seatIndex)"
        addSubview(indexLabel)
        indexLabel.snp.makeConstraints { make in
            make.bottom.equalTo(snp.centerY).offset(-4)
            make.centerX.equalToSuperview()
        }
        
        let titleLabel = UILabel(frame: .zero)
        titleLabel.font = .customFont(ofSize: 12, weight: .regular)
        titleLabel.textColor = .white.withAlphaComponent(0.9)
        titleLabel.text = .emptySeatText
        titleLabel.numberOfLines = 0
        titleLabel.textAlignment = .center
        addSubview(titleLabel)
        titleLabel.snp.makeConstraints { make in
            make.top.equalTo(snp.centerY).offset(4)
            make.leading.trailing.equalToSuperview()
        }
        
        FloatWindow.shared.subscribeShowingState()
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] isShow in
                guard let self = self else { return }
                isHidden = isShow
            }
            .store(in: &cancellableSet)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
}

fileprivate extension String {
    static let emptySeatText: String = internalLocalized("Conn Wait")
}
