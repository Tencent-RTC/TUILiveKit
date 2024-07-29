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
    
    var cancellableSet = Set<AnyCancellable>()
    private var internalCancellableSet = Set<AnyCancellable>()
    
    private var seatInfoStoreValue: StoreValue<OperationState, SeatInfo>?
    private(set) var seatIndex: Int = -1
    
    let soundWaveView: SeatSoundWaveView = {
        let view = SeatSoundWaveView()
        view.backgroundColor = .seatWaveColor.withAlphaComponent(0.5)
        view.isHidden = true
        return view
    }()
    
    let mainImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.contentMode = .scaleAspectFit
        imageView.layer.masksToBounds = true
        return imageView
    }()
    
    let seatContentView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .seatContentColor
        return view
    }()
    
    let seatImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.image = UIImage(named: "live_seat_empty_icon", in: .liveBundle, compatibleWith: nil)
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
        label.font = UIFont.customFont(ofSize: 14)
        label.textColor = .g9
        label.textAlignment = .center
        label.numberOfLines = 1
        label.adjustsFontSizeToFitWidth = true
        label.minimumScaleFactor = 0.5
        return label
    }()

    let ownerImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.image = UIImage(named: "live_seat_owner_icon", in: .liveBundle, compatibleWith: nil)
        imageView.isHidden = true
        return imageView
    }()
    
    let nameContentView: UIView = {
        let view = UIView()
        return view
    }()
    
    let giftContentView: UIView = {
        let view = UIView()
        view.backgroundColor = .giftContentColor
        return view
    }()
    
    let giftImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.image = UIImage(named: "live_seat_gift_icon", in: .liveBundle, compatibleWith: nil)
        return imageView
    }()
    
    let giftCountLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.text = .handsupText
        label.font = UIFont.customFont(ofSize: 12.0)
        label.textColor = .white
        label.textAlignment = .center
        label.numberOfLines = 1
        label.adjustsFontSizeToFitWidth = true
        label.minimumScaleFactor = 0.5
        return label
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
        mainImageView.layer.cornerRadius = mainImageView.frame.height * 0.5
        soundWaveView.layer.cornerRadius = soundWaveView.frame.height * 0.5
        seatContentView.layer.cornerRadius = seatContentView.frame.height * 0.5
        seatContentView.layer.borderWidth = 0.5
        seatContentView.layer.borderColor = UIColor.seatContentBorderColor.cgColor
        giftContentView.layer.cornerRadius = giftContentView.frame.height * 0.5
    }
    
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
        addSubview(soundWaveView)
        addSubview(mainImageView)
        addSubview(muteImageView)
        
        addSubview(seatContentView)
        seatContentView.addSubview(seatImageView)
        
        addSubview(nameContentView)
        nameContentView.addSubview(ownerImageView)
        nameContentView.addSubview(nameLabel)
        
        addSubview(giftContentView)
        giftContentView.addSubview(giftImageView)
        giftContentView.addSubview(giftCountLabel)
        
        
    }
    
    func activateConstraints() {
        soundWaveView.snp.makeConstraints { (make) in
            make.center.equalTo(mainImageView.snp.center)
            make.size.equalTo(CGSizeMake(50, 50))
        }
        mainImageView.snp.makeConstraints { (make) in
            make.top.equalToSuperview().offset(10.scale375())
            make.size.equalTo(CGSizeMake(50, 50))
            make.centerX.equalToSuperview()
        }
        muteImageView.snp.makeConstraints { (make) in
            make.trailing.bottom.equalTo(mainImageView)
        }
        
        activateConstraintsSeatContent()
        activateConstraintsNameContent()
        activateConstraintsGiftContent()
    }
    
    func activateConstraintsSeatContent() {
        seatContentView.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(10.scale375())
            make.size.equalTo(CGSizeMake(50, 50))
            make.centerX.equalToSuperview()
        }
        seatImageView.snp.makeConstraints { make in
            make.center.equalToSuperview()
        }
    }
    
    func activateConstraintsNameContent() {
        nameContentView.snp.makeConstraints { make in
            make.top.equalTo(mainImageView.snp.bottom).offset(4.scale375())
            make.leading.trailing.equalToSuperview()
            make.height.equalTo(18.scale375())
        }
        ownerImageView.snp.makeConstraints { make in
            make.leading.equalToSuperview()
            make.trailing.equalTo(nameLabel.snp.leading).offset(-3.scale375())
            make.centerY.equalTo(nameLabel.snp.centerY)
            make.size.equalTo(CGSize(width: 14.scale375(), height: 14.scale375()))
        }
        nameLabel.snp.makeConstraints { (make) in
            make.leading.equalTo(ownerImageView.snp.trailing).offset(3.scale375())
            make.trailing.equalToSuperview()
            make.top.equalToSuperview()
            make.bottom.equalToSuperview()
        }
    }
    
    func activateConstraintsGiftContent() {
        giftContentView.snp.makeConstraints { make in
            make.top.equalTo(nameContentView.snp.bottom)
            make.centerX.equalToSuperview()
            make.bottom.equalToSuperview()
        }
        giftImageView.snp.makeConstraints { make in
            make.leading.equalToSuperview().offset(4.scale375())
            make.centerY.equalToSuperview()
            make.trailing.equalTo(giftImageView.snp.leading).offset(-2.scale375())
        }
        giftCountLabel.snp.makeConstraints { make in
            make.trailing.equalToSuperview().offset(-4.scale375())
            make.centerY.equalToSuperview()
        }
    }
    
    func prepareForReuse() {
        mainImageView.kf.cancelDownloadTask()
        mainImageView.image = nil
        nameLabel.text = ""
        muteImageView.isHidden = true
        cancellableSet.removeAll()
    }
    
    func bindInteraction() {
        $seatInfo
            .receive(on: RunLoop.main)
            .sink { [weak self] seatInfo in
                guard let self = self else { return }
                self.update(seatInfo: seatInfo)
            }
            .store(in: &internalCancellableSet)
        $isSpeaking
            .receive(on: RunLoop.main)
            .sink(receiveValue: { [weak self] isSpeaking in
                guard let self = self else { return }
                isSpeaking ? self.soundWaveView.startWave() : self.soundWaveView.stopWave()
            })
            .store(in: &internalCancellableSet)
        $isAudioMuted
            .receive(on: RunLoop.main)
            .map { !$0 }
            .assign(to: \UIView.isHidden, on: muteImageView)
            .store(in: &internalCancellableSet)
    }
    
    private func update(seatInfo: SeatInfo) {
        seatContentView.isHidden = !seatInfo.userId.isEmpty
        if seatInfo.userId.isEmpty {
            if seatInfo.isLocked {
                seatImageView.image = UIImage(named: "live_seat_locked_icon", in: .liveBundle, compatibleWith: nil)
            } else {
                seatImageView.image = UIImage(named: "live_seat_empty_icon", in: .liveBundle, compatibleWith: nil)
            }
            nameLabel.text = "\(seatInfo.index + 1)"
            toEmptySeatStyle()
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
        muteImageView.isHidden = true
        ownerImageView.isHidden = true
        giftImageView.isHidden = true
        soundWaveView.isHidden = true
        mainImageView.image = nil
        nameLabel.snp.remakeConstraints { make in
            make.leading.trailing.top.bottom.equalToSuperview()
        }
    }
    
    private func toUserOnSeatStyle() {
        soundWaveView.isHidden = false
        ownerImageView.snp.remakeConstraints { make in
            make.leading.equalToSuperview()
            make.trailing.equalTo(nameLabel.snp.leading).offset(-3)
            make.centerY.equalTo(nameLabel.snp.centerY)
            make.size.equalTo(CGSize(width: 14, height: 14))
        }
        nameLabel.snp.remakeConstraints { (make) in
            make.leading.equalTo(ownerImageView.snp.trailing).offset(3)
            make.trailing.equalToSuperview()
            make.top.equalToSuperview()
            make.bottom.equalToSuperview()
        }
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
