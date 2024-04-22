//
//  VoiceAudienceListView.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/10/20.
//

import UIKit
import Combine

class VoiceAudienceCell: UITableViewCell {
    
    var userInfo: User? {
        didSet {
            guard let userInfo = userInfo else {
                return
            }
            if let url = URL(string: userInfo.avatarUrl) {
                avatarImageView.kf.setImage(with: url,placeholder: UIImage.placeholderImage)
            } else {
                avatarImageView.image = .placeholderImage
            }
            nameLabel.text = userInfo.name
        }
    }
    
    lazy var avatarImageView: UIImageView = {
        let imageView = UIImageView(frame: .zero)
        imageView.layer.cornerRadius = 40.scale375() * 0.5
        imageView.layer.masksToBounds = true
        contentView.addSubview(imageView)
        return imageView
    }()
    
    lazy var nameLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 16)
        label.textColor = .white
        return label
    }()
    
    let lineView: UIView = {
        let view = UIView()
        view.backgroundColor = .g3.withAlphaComponent(0.3)
        return view
    }()
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        selectionStyle = .none
        backgroundColor = .clear
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else {
            return
        }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
    }
    
    func constructViewHierarchy() {
        contentView.addSubview(avatarImageView)
        contentView.addSubview(nameLabel)
        contentView.addSubview(lineView)
    }
    
    func activateConstraints() {
        avatarImageView.snp.makeConstraints { (make) in
            make.centerY.equalToSuperview()
            make.leading.equalToSuperview().inset(24)
            make.width.equalTo(40.scale375())
            make.height.equalTo(40.scale375())
        }
        
        nameLabel.snp.makeConstraints { (make) in
            make.centerY.equalToSuperview()
            make.height.equalToSuperview()
            make.leading.equalTo(avatarImageView.snp.trailing).offset(14.scale375())
            make.trailing.trailing.equalToSuperview().inset(24)
        }
       
        lineView.snp.makeConstraints { (make) in
            make.bottom.equalToSuperview()
            make.leading.equalTo(nameLabel)
            make.trailing.equalToSuperview().inset(24)
            make.height.equalTo(1)
        }
        
    }
}

class VoiceAudienceListView: UIView {
    @Injected var store: VoiceRoomStoreProvider
    lazy var getAudienceList = self.store.select(UserSelectors.getAudienceList)
    private var internalCancellables = Set<AnyCancellable>()
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        backgroundColor = .clear
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
    }
    
    override func removeFromSuperview() {
        super.removeFromSuperview()
        internalCancellables.removeAll()
    }
    
    var audienceList: [User] = [] {
        didSet {
            titleLabel.text = .localized(.onlineAudience)
            userListTableView.reloadData()
        }
    }

    init() {
        super.init(frame: .zero)
        
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }

    private lazy var backButton: UIButton = {
        let view = UIButton(type: .system)
        view.setBackgroundImage(.liveBundleImage("live_back_icon"), for: .normal)
        view.addTarget(self, action: #selector(clickBack(sender:)), for: .touchUpInside)
        return view
    }()

    private lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.textAlignment = .center
        label.font = .customFont(ofSize: 16,weight: .medium)
        label.textColor = .g7
        return label
    }()
    
    private lazy var userListTableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .plain)
        tableView.backgroundColor = .clear
        tableView.separatorStyle = .none
        tableView.register(VoiceAudienceCell.self, forCellReuseIdentifier: VoiceAudienceCell.cellReuseIdentifier)
        return tableView
    }()

    private func bindInteraction() {
        getAudienceList
            .receive(on: RunLoop.main)
            .assign(to: \VoiceAudienceListView.audienceList, on: self)
            .store(in: &internalCancellables)
        userListTableView.dataSource = self
        userListTableView.delegate = self
    }
}

// MARK: Layout

extension VoiceAudienceListView {
    func constructViewHierarchy() {
        backgroundColor = .g2
        self.layer.cornerRadius = 16
        self.layer.masksToBounds = true
        addSubview(backButton)
        addSubview(titleLabel)
        addSubview(userListTableView)
    }

    func activateConstraints() {
        snp.remakeConstraints { make in
            if isPortrait {
                make.width.equalToSuperview()
                make.centerX.equalToSuperview()
            } else {
                make.width.equalTo(375)
                make.height.equalToSuperview()
                make.trailing.equalToSuperview()
            }
            make.bottom.equalToSuperview()
        }
        
        backButton.snp.remakeConstraints { make in
            make.leading.equalToSuperview().inset(20)
            make.top.equalToSuperview().inset(20)
            make.height.equalTo(24.scale375())
            make.width.equalTo(24.scale375())
        }

        titleLabel.snp.remakeConstraints { make in
            make.centerY.equalTo(backButton)
            make.centerX.equalToSuperview()
            make.height.equalTo(24.scale375())
        }
        
        userListTableView.snp.remakeConstraints { make in
            make.leading.trailing.bottom.equalToSuperview()
            make.top.equalTo(backButton.snp.bottom).offset(32)
            make.height.equalTo(UIScreen.main.bounds.height * 0.6)
        }
    }
}

// MARK: Action

extension VoiceAudienceListView {
    @objc
    func clickBack(sender: UIButton) {
        store.dispatch(action: NavigatorActions.navigatorTo(payload: .main))
    }
}


// MARK: - UITableViewDataSource

extension VoiceAudienceListView: UITableViewDataSource {
    internal func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return audienceList.count
    }
}

// MARK: - UITableViewDelegate

extension VoiceAudienceListView: UITableViewDelegate {
    internal func tableView(_ tableView: UITableView,
                            cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        
        let cell = tableView.dequeueReusableCell(withIdentifier: VoiceAudienceCell.cellReuseIdentifier, for: indexPath)
        if let cell = cell as? VoiceAudienceCell,
            indexPath.row < audienceList.count {
            cell.userInfo = audienceList[indexPath.row]
        }
        return cell
    }
    
    internal func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        
    }
    
    internal func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 50.scale375()
    }
}

fileprivate extension String {
    static let onlineAudience = localized("live.recent.online.audience")
}
