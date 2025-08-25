//
//  AnchorBackgroundWidgetView.swift
//  TUILiveKit
//
//  Created by gg on 2025/7/17.
//

import SnapKit
import Kingfisher
import RTCCommon
import Combine

class AnchorBackgroundWidgetView: UIView {
    init(avatarUrl: String) {
        super.init(frame: .zero)
        
        backgroundColor = .bgOperateColor
        
        addSubview(avatarImageView)
        avatarImageView.snp.makeConstraints { make in
            make.center.equalToSuperview()
            make.width.equalTo(40.scale375())
            make.height.equalTo(40.scale375())
        }
        avatarImageView.kf.setImage(with: URL(string: avatarUrl), placeholder: UIImage.avatarPlaceholderImage)
        
        subscribeState()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var cancellableSet = Set<AnyCancellable>()
    
    private lazy var avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 40.scale375() * 0.5
        imageView.layer.masksToBounds = true
        return imageView
    }()
    
    func subscribeState() {
        FloatWindow.shared.subscribeShowingState()
            .receive(on: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] isShow in
                guard let self = self else { return }
                isHidden = isShow
            }
            .store(in: &cancellableSet)
    }
}
