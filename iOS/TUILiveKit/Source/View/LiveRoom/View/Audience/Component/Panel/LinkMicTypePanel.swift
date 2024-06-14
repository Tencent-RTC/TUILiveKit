//
//  LinkMicTypePanel.swift
//  TUILiveKit
//
//  Created by krabyu on 2023/10/25.
//

import Foundation

class LinkMicTypePanel: UIView {
    @Injected var viewStore: LiveRoomViewStore
    
    private var isPortrait: Bool = {
        WindowUtils.isPortrait
    }()

    let data: [LinkMicTypeCellData]
    
    init(data: [LinkMicTypeCellData]) {
        self.data = data
        super.init(frame: .zero)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        constructViewHierarchy()
        activateConstraints()
        backgroundColor = .g2
        isViewReady = true
    }

    private let titleLabel: UILabel = {
        let view = UILabel()
        view.text = .linkTypeTitleText
        view.textColor = .g7
        view.font = .customFont(ofSize: 16)
        view.textAlignment = .center
        return view
    }()

    private let tipsLabel: UILabel = {
        let view = UILabel()
        view.text = .linkTypeTipsText
        view.textColor = .greyColor
        view.font = .customFont(ofSize: 12)
        view.textAlignment = .center
        view.numberOfLines = 0
        view.lineBreakMode = .byWordWrapping
        return view
    }()
    
    private lazy var videoSettingButton: UIButton = {
        let view = UIButton()
        view.setImage(.liveBundleImage("live_link_videoSetting"), for: .normal)
        view.addTarget(self, action: #selector(videoSettingImageViewAction), for: .touchUpInside)
        return view
        
    }()

    private lazy var linkMicTypeTableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .plain)
        tableView.backgroundColor = .clear
        tableView.separatorStyle = .none
        tableView.delegate = self
        tableView.dataSource = self
        tableView.register(LinkMicTypeCell.self, forCellReuseIdentifier: LinkMicTypeCell.cellReuseIdentifier)
        return tableView
    }()
}

// MARK: Layout
extension LinkMicTypePanel {
    func constructViewHierarchy() {
        backgroundColor = .b2d
        layer.cornerRadius = 20
        layer.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        addSubview(titleLabel)
        addSubview(tipsLabel)
        addSubview(videoSettingButton)
        addSubview(linkMicTypeTableView)
    }

    func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(20.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(24.scale375Height())
        }

        tipsLabel.snp.makeConstraints { make in
            make.top.equalTo(titleLabel.snp.bottom).offset(8.scale375Height())
            make.centerX.equalToSuperview()
            make.width.equalToSuperview()
            make.height.equalTo(17.scale375Height())
        }
        
        videoSettingButton.snp.makeConstraints { make in
            make.top.equalToSuperview().offset(38.scale375Height())
            make.width.height.equalTo(20.scale375())
            make.trailing.equalToSuperview().offset(-16.scale375())
        }

        linkMicTypeTableView.snp.makeConstraints { make in
            if isPortrait {
                make.height.equalTo(234.scale375Height())
            } else {
                make.width.equalTo(375)
            }
            make.top.equalTo(tipsLabel.snp.bottom).offset(20.scale375Height())
            make.width.equalToSuperview()
            make.bottom.equalToSuperview()
        }
    }
}

extension LinkMicTypePanel {
    @objc func videoSettingImageViewAction() {
        viewStore.navigate(action: .present(.linkSetting))
    }
}

extension LinkMicTypePanel: UITableViewDelegate {
    public func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 55.scale375Height()
    }
}

extension LinkMicTypePanel: UITableViewDataSource {
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return 2
    }

    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: LinkMicTypeCell.cellReuseIdentifier, for: indexPath)
        if let cell = cell as? LinkMicTypeCell {
            cell.data = data[indexPath.row]
        }
        cell.selectionStyle = .none
        return cell
    }
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        let data = data[indexPath.row]
        data.action?()
    }
}

private extension String {
    static var linkTypeTitleText: String {
        localized("live.audience.linkType.title")
    }

    static var linkTypeTipsText: String {
        localized("live.audience.linkType.tips")
    }

    static var videoLinkRequestText: String {
        localized("live.audience.linkType.videoLinkRequest")
    }

    static var audioLinkRequestText: String {
        localized("live.audience.linkType.audioLinkRequest")
    }
}
