//
//  SeatView.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/14.
//

import UIKit
import SnapKit
import RTCRoomEngine
import Combine

class SeatCollectionCell: UICollectionViewCell {
    static let identifier = "SeatCollectionCell"
    private var isViewReady: Bool = false
    
    let seatView: SeatView = {
        let view = SeatView()
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
    
    override func prepareForReuse() {
        super.prepareForReuse()
        seatView.prepareForReuse()
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }

}

class SeatView: UIView {
    
    @Published var seatInfo: SeatInfo = SeatInfo()
    @Published var isSpeaking: Bool = false
    @Published var isAudioMuted: Bool = true
    
    var userId: String {
        seatInfo.userId
    }
    
    private var isViewReady: Bool = false
    private var isGetBounds: Bool = false
    
    var cancellables = Set<AnyCancellable>()
    private var internalCancellables = Set<AnyCancellable>()
    
    private var seatInfoStoreValue: StoreValue<OperationState, SeatInfo>?
    private(set) var seatIndex: Int = -1
    
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
        mainImageView.layer.cornerRadius = mainImageView.frame.height*0.5
        
        speakView.layer.cornerRadius = speakView.frame.height*0.5
        speakView.layer.borderWidth = 4
        speakView.layer.borderColor = UIColor.orangeColor.cgColor
    }
    
    let speakView: UIView = {
        let view = UIView()
        view.backgroundColor = UIColor.clear
        view.isHidden = false
        return view
    }()
    
    let mainImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.contentMode = .scaleAspectFit
        imageView.image = UIImage(named: "live_seat_placeholder_avatar", in: .liveBundle, compatibleWith: nil)
        imageView.layer.masksToBounds = true
        return imageView
    }()
    
    let muteImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.contentMode = .scaleAspectFit
        imageView.image = UIImage(named: "live_seat_audio_locked", in: .liveBundle, compatibleWith: nil)
        imageView.isHidden = true
        return imageView
    }()
    
    let nameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.text = .handsupText
        label.font = UIFont.systemFont(ofSize: 14.0)
        label.textColor = .lightCyanColor
        label.textAlignment = .center
        label.numberOfLines = 1
        label.adjustsFontSizeToFitWidth = true
        label.minimumScaleFactor = 0.5
        return label
    }()
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        isViewReady = true
    }
    
    func setupViewConfig() {
        backgroundColor = .clear
    }
    
    func constructViewHierarchy() {
        addSubview(mainImageView)
        addSubview(muteImageView)
        addSubview(nameLabel)
        addSubview(speakView)
    }
    
    func activateConstraints() {
        mainImageView.snp.makeConstraints { (make) in
            make.top.leading.trailing.equalToSuperview()
            make.height.equalTo(mainImageView.snp.width)
        }
        muteImageView.snp.makeConstraints { (make) in
            make.trailing.bottom.equalTo(mainImageView)
        }
        nameLabel.snp.makeConstraints { (make) in
            make.leading.trailing.bottom.equalToSuperview()
            make.top.equalTo(mainImageView.snp.bottom).offset(8)
            make.width.lessThanOrEqualTo(120)
        }
        speakView.snp.makeConstraints { (make) in
            make.center.equalTo(mainImageView)
            make.width.equalTo(mainImageView).offset(1)
            make.height.equalTo(mainImageView).offset(1)
        }
    }
    
    func prepareForReuse() {
        mainImageView.kf.cancelDownloadTask()
        mainImageView.image = nil
        nameLabel.text = ""
        speakView.isHidden = true
        muteImageView.isHidden = true
        cancellables.removeAll()
    }
    
    func bindInteraction() {
        $seatInfo
            .receive(on: RunLoop.main)
            .sink { [weak self] seatInfo in
                guard let self = self else { return }
                self.update(seatInfo: seatInfo)
            }
            .store(in: &internalCancellables)
        $isSpeaking
            .receive(on: RunLoop.main)
            .map { !$0 }
            .assign(to: \UIView.isHidden, on: speakView)
            .store(in: &internalCancellables)
        $isAudioMuted
            .receive(on: RunLoop.main)
            .map { !$0 }
            .assign(to: \UIView.isHidden, on: muteImageView)
            .store(in: &internalCancellables)
    }
    
    private func update(seatInfo: SeatInfo) {
        if seatInfo.userId.isEmpty {
            toEmptySeatStyle()
            if seatInfo.isLocked {
                mainImageView.image = UIImage(named: "live_seat_locked", in: .liveBundle, compatibleWith: nil)
                nameLabel.text = ""
                return
            } else {
                mainImageView.image = UIImage(named: "live_seat_empty", in: .liveBundle, compatibleWith: nil)
                nameLabel.text = ""
            }
        } else {
            let placeholder = UIImage(named: "live_seat_placeholder_avatar", in: .liveBundle, compatibleWith: nil)
            if !seatInfo.avatarUrl.isEmpty, let avatarURL = URL(string: seatInfo.avatarUrl) {
                mainImageView.kf.setImage(with: avatarURL, placeholder: placeholder)
            } else {
                mainImageView.image = placeholder
            }
            nameLabel.text = seatInfo.userName
            toUserOnSeatStyle()
        }
    }
}

extension SeatView {
    private func toEmptySeatStyle() {
        nameLabel.font = UIFont.systemFont(ofSize: 14.0)
        nameLabel.textColor = .placeholderBackColor
        speakView.isHidden = true
        muteImageView.isHidden = true
    }
    
    private func toUserOnSeatStyle() {
        nameLabel.font = UIFont.systemFont(ofSize: 14.0)
        nameLabel.textColor = .lightCyanColor
    }
}

/// MARK: - internationalization string
fileprivate extension String {
    static var handsupText: String {
        return ""
    }
    static var lockedText: String {
        return ""
    }
    static var inviteHandsupText: String {
        return ""
    }
}
