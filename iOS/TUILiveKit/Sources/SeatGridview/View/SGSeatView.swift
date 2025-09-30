//
//  SGSeatView.swift
//  SeatGridView
//
//  Created by krabyu on 2024/10/16.
//

import Combine
import RTCRoomEngine

class SGSeatView: UIView {
    @Published var seatInfo: TUISeatInfo
    @Published var isSpeaking: Bool = false
    @Published var isAudioMuted: Bool = true
    
    private var userId: String {
        seatInfo.userId ?? ""
    }
    private let ownerId: String
    private var isViewReady: Bool = false
    private var cancellableSet = Set<AnyCancellable>()
    private(set) var seatIndex: Int = -1
    
    init(seatInfo: TUISeatInfo, ownerId: String) {
        self.seatInfo = seatInfo
        self.ownerId = ownerId
        super.init(frame: .zero)
        setupViewConfig()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    let soundWaveView: SGSeatSoundWaveView = {
        let view = SGSeatSoundWaveView()
        view.backgroundColor = UIColor.seatWaveColor.withAlphaComponent(0.5)
        view.isHidden = true
        return view
    }()
    
    let mainImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.contentMode = .scaleAspectFill
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
        imageView.image = internalImage("seat_empty_icon")
        return imageView
    }()
    
    let muteImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.contentMode = .scaleAspectFit
        imageView.image = internalImage("seat_audio_locked")
        imageView.isHidden = true
        return imageView
    }()
    
    let nameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 12)
        label.textColor = .g9
        label.textAlignment = .center
        label.numberOfLines = 1
        return label
    }()

    let ownerImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.image =  internalImage("seat_owner_icon")
        imageView.isHidden = true
        return imageView
    }()
    
    let nameContentView: UIView = {
        let view = UIView()
        return view
    }()
    
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
    }
    
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
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
        addSubview(seatContentView)
        addSubview(muteImageView)
        addSubview(nameContentView)
        seatContentView.addSubview(seatImageView)
        nameContentView.addSubview(ownerImageView)
        nameContentView.addSubview(nameLabel)
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
            make.size.equalTo(CGSize(width: 16.scale375(), height: 16.scale375()))
        }
        
        activateConstraintsSeatContent()
        activateConstraintsNameContent()
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
            make.width.lessThanOrEqualToSuperview()
            make.centerX.equalToSuperview()
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
    
    func bindInteraction() {
        $seatInfo
            .receive(on: RunLoop.main)
            .sink { [weak self] seatInfo in
                guard let self = self else { return }
                self.update(seatInfo: seatInfo)
            }
            .store(in: &cancellableSet)
        
        $isAudioMuted
            .combineLatest($seatInfo)
            .receive(on: RunLoop.main)
            .sink { [weak self] isMuted, seatInfo in
                guard let self = self, let userId = seatInfo.userId, !userId.isEmpty else { return }
                self.muteImageView.isHidden = !isMuted
            }
            .store(in: &cancellableSet)

        $isSpeaking
            .combineLatest($seatInfo)
            .receive(on: RunLoop.main)
            .sink { [weak self] isSpeaking, seatInfo in
                guard let self = self, let userId = seatInfo.userId, !userId.isEmpty else { return }
                isSpeaking ? self.soundWaveView.startWave() : self.soundWaveView.stopWave()
            }
            .store(in: &cancellableSet)
    }
    
    private func update(seatInfo: TUISeatInfo) {
        if let userId = seatInfo.userId, !userId.isEmpty {
            if let avatarUrl = seatInfo.avatarUrl, let url = URL(string: avatarUrl) {
                mainImageView.kf.setImage(with: url, placeholder: avatarPlaceholderImage)
            } else {
                mainImageView.image = .avatarPlaceholderImage
            }
            nameLabel.text = seatInfo.userName ?? ""
            toUserOnSeatStyle()
        } else {
            if seatInfo.isLocked {
                seatImageView.image = internalImage("seat_locked_icon")
            } else {
                seatImageView.image = internalImage("seat_empty_icon")
            }
            nameLabel.text = "\(seatInfo.index + 1)"
            toEmptySeatStyle()
        }
    }
}

extension SGSeatView {
    private func toEmptySeatStyle() {
        seatImageView.isHidden = false
        muteImageView.isHidden = true
        ownerImageView.isHidden = true
        soundWaveView.isHidden = true
        mainImageView.image = nil
        nameLabel.snp.remakeConstraints { make in
            make.leading.trailing.top.bottom.equalToSuperview()
        }
    }
    
    private func toUserOnSeatStyle() {
        soundWaveView.isHidden = false
        seatImageView.isHidden = true
        ownerImageView.isHidden = !isOwner()
        ownerImageView.snp.remakeConstraints { make in
            make.leading.equalToSuperview()
            make.trailing.equalTo(nameLabel.snp.leading).offset(-1)
            make.centerY.equalTo(nameLabel.snp.centerY)
            make.size.equalTo(CGSize(width: 14, height: 14))
        }
        nameLabel.snp.remakeConstraints { (make) in
            if isOwner() {
                make.leading.equalTo(ownerImageView.snp.trailing).offset(1)
            } else {
                make.leading.equalToSuperview()
            }
            make.trailing.equalToSuperview()
            make.top.equalToSuperview()
            make.bottom.equalToSuperview()
        }
    }
    
    private func isOwner() -> Bool {
        return ownerId == seatInfo.userId
    }
}
