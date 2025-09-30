//
//  LiveDashboardCell.swift
//
//
//  Created by jack on 2024/11/21.
//

import Foundation
import Combine
import RTCCommon

class StreamDashboardMediaCell: UICollectionViewCell {
    static let CellID: String = "StreamDashboardMediaCell"
    
    enum VideoDataType {
        case resolution
        case bitrate
        case fps
    }
    
    enum AudioDataType {
        case bitrate
        case sampleRate
    }
    
    private lazy var containerView: UIView = {
        let view = UIView(frame: .zero)
        view.backgroundColor = .bgEntrycardColor
        view.layer.cornerRadius = 12
        view.layer.masksToBounds = true
        return view
    }()
    
    private lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 14, weight: .medium)
        label.textColor = .textPrimaryColor
        label.textAlignment = .left
        return label
    }()
    
    private lazy var separatorLine: UIView = {
        let view = UIView()
        view.backgroundColor = .strokeModuleColor
        return view
    }()
    
    private lazy var videoTitleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.text = .videoText
        label.font = .customFont(ofSize: 12, weight: .medium)
        label.textColor = .textPrimaryColor
        return label
    }()
    
    private lazy var videoTableView: UITableView = {
        let view = UITableView(frame: .zero, style: .plain)
        view.isScrollEnabled = false
        view.isUserInteractionEnabled = false
        view.delegate = self
        view.dataSource = self
        view.register(StreamDashboardMediaItemCell.self, forCellReuseIdentifier: StreamDashboardMediaItemCell.CellID)
        view.backgroundColor = .clear
        view.layer.cornerRadius = 12
        view.layer.masksToBounds = true
        return view
    }()
    
    private lazy var audioTitleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.text = .audioText
        label.font = .customFont(ofSize: 12, weight: .medium)
        label.textColor = .textPrimaryColor
        return label
    }()
    
    private lazy var audioTableView: UITableView = {
        let view = UITableView(frame: .zero, style: .plain)
        view.isScrollEnabled = false
        view.isUserInteractionEnabled = false
        view.delegate = self
        view.dataSource = self
        view.register(StreamDashboardMediaItemCell.self, forCellReuseIdentifier: StreamDashboardMediaItemCell.CellID)
        view.backgroundColor = .clear
        view.layer.cornerRadius = 12
        view.layer.masksToBounds = true
        return view
    }()
    
    private var videoDataSource: [VideoDataType] = [.resolution, .bitrate, .fps]
    private var audioDataSource: [AudioDataType] = [.sampleRate, .bitrate]
    @Published private var isRemoteUserEmpty: Bool = false
    var cancellableSet = Set<AnyCancellable>()
    
    private var isViewReady: Bool = false
    override func didMoveToWindow() {
        super.didMoveToWindow()
        guard !isViewReady else { return }
        isViewReady = true
        constructViewHierarchy()
        activateConstraints()
        subscribeState()
        contentView.backgroundColor = .clear
    }
    
    override func preferredLayoutAttributesFitting(_ layoutAttributes: UICollectionViewLayoutAttributes) -> UICollectionViewLayoutAttributes {
        self.setNeedsLayout()
        self.layoutIfNeeded()
        
        let size = self.contentView.systemLayoutSizeFitting(layoutAttributes.size)
        var cellFrame = layoutAttributes.frame
        cellFrame.size.height = size.height
        layoutAttributes.frame = cellFrame
        return layoutAttributes
    }
    
    private var data: StreamDashboardUser = StreamDashboardUser()
    func updateData(_ data: StreamDashboardUser) {
        self.data = data
        if data.isLocal {
            titleLabel.text = .localText
        } else {
            titleLabel.text = .remoteText + ": \(data.userId)"
        }
        self.videoTableView.reloadData()
        self.audioTableView.reloadData()
    }
    
    func changeRemoteUserEmpty(isEmpty: Bool) {
        isRemoteUserEmpty = isEmpty
    }
    
    func subscribeState() {
        $isRemoteUserEmpty
            .removeDuplicates()
            .receive(on: RunLoop.main)
            .dropFirst()
            .sink { [weak self] isRemoteUserEmpty in
                guard let self = self else { return }
                if isRemoteUserEmpty {
                    titleLabel.safeRemoveFromSuperview()
                    separatorLine.safeRemoveFromSuperview()
                } else {
                    containerView.addSubview(titleLabel)
                    containerView.addSubview(separatorLine)
                }
                activateConstraints()
            }
            .store(in: &cancellableSet)
    }
}

extension StreamDashboardMediaCell {
    
    private func constructViewHierarchy() {
        contentView.addSubview(containerView)
        if !isRemoteUserEmpty {
            containerView.addSubview(titleLabel)
            containerView.addSubview(separatorLine)
        }
        containerView.addSubview(videoTitleLabel)
        containerView.addSubview(videoTableView)
        containerView.addSubview(audioTitleLabel)
        containerView.addSubview(audioTableView)
    }
    
    private func activateConstraints() {
        containerView.snp.remakeConstraints { make in
            make.top.bottom.equalToSuperview()
            make.leading.trailing.equalToSuperview().inset(20.scale375())
        }
        if !isRemoteUserEmpty {
            titleLabel.snp.remakeConstraints { make in
                make.leading.trailing.top.equalToSuperview().inset(16.scale375())
            }
            separatorLine.snp.remakeConstraints { make in
                make.leading.trailing.equalToSuperview().inset(16.scale375())
                make.top.equalTo(titleLabel.snp.bottom).offset(12.scale375())
                make.height.equalTo(1)
            }
        }
        videoTitleLabel.snp.remakeConstraints { make in
            make.leading.equalToSuperview().offset(16.scale375())
            if isRemoteUserEmpty {
                make.top.equalToSuperview().offset(16.scale375Height())
            } else {
                make.top.equalTo(separatorLine.snp.bottom).offset(12.scale375Height())
            }
        }
        videoTableView.snp.remakeConstraints { make in
            make.top.equalTo(videoTitleLabel.snp.bottom).offset(8.scale375Height())
            make.leading.equalToSuperview().inset(16.scale375())
            make.width.equalTo(bounds.width / 2 - 44.scale375())
            make.height.equalTo(StreamDashboardMediaItemCell.CellHeight * CGFloat(videoDataSource.count))
        }
        audioTitleLabel.snp.remakeConstraints { make in
            make.leading.equalTo(videoTableView.snp.trailing).offset(16.scale375())
            make.centerY.equalTo(videoTitleLabel)
        }
        audioTableView.snp.remakeConstraints { make in
            make.top.equalTo(audioTitleLabel.snp.bottom).offset(8.scale375Height())
            make.leading.equalTo(audioTitleLabel)
            make.trailing.equalToSuperview().inset(16.scale375())
            make.height.equalTo(StreamDashboardMediaItemCell.CellHeight * CGFloat(audioDataSource.count))
        }
    }
    
    private func updateVideoCellData(cell: StreamDashboardMediaItemCell, dataType: VideoDataType) {
        switch dataType {
        case .bitrate:
            cell.titleLabel.text = .bitrateText
            cell.valueLabel.text = "\(data.videoBitrate) kbps"
        case .fps:
            cell.titleLabel.text = .videoFrameRateText
            cell.valueLabel.text = "\(data.videoFrameRate) FPS"
        case .resolution:
            cell.titleLabel.text = .videoResolutionText
            cell.valueLabel.text = "\(data.videoResolution)"
        }
    }
    
    private func updateAudioCellData(cell: StreamDashboardMediaItemCell, dataType: AudioDataType) {
        switch dataType {
        case .bitrate:
            cell.titleLabel.text = .bitrateText
            cell.valueLabel.text = "\(data.audioBitrate) kbps"
        case .sampleRate:
            cell.titleLabel.text = .audioSampleRateText
            cell.valueLabel.text = "\(data.audioSampleRate) Hz"
        }
    }
}

extension StreamDashboardMediaCell: UITableViewDataSource {
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        if tableView == videoTableView {
            return videoDataSource.count
        }
        if tableView == audioTableView {
            return audioDataSource.count
        }
        return 0
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: StreamDashboardMediaItemCell.CellID, for: indexPath) as! StreamDashboardMediaItemCell
        if tableView == videoTableView {
            updateVideoCellData(cell: cell, dataType: videoDataSource[indexPath.row])
        }
        if tableView == audioTableView {
            updateAudioCellData(cell: cell, dataType: audioDataSource[indexPath.row])
        }
        return cell
    }
}

extension StreamDashboardMediaCell: UITableViewDelegate {
    
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return StreamDashboardMediaItemCell.CellHeight
    }
}

class StreamDashboardMediaItemCell: UITableViewCell {
    static let CellID: String = "StreamDashboardMediaItemCell"
    static let CellHeight: CGFloat = 20.scale375Height()
    
    lazy var titleLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 12)
        label.textColor = .textSecondaryColor
        label.adjustsFontSizeToFitWidth = true
        return label
    }()
    
    lazy var valueLabel: UILabel = {
        let label = UILabel(frame: .zero)
        label.font = .customFont(ofSize: 12)
        label.textColor = .textPrimaryColor
        return label
    }()
    
    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        constructViewHierarchy()
        activateConstraints()
        backgroundColor = .clear
        selectionStyle = .none
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private func constructViewHierarchy() {
        contentView.addSubview(titleLabel)
        contentView.addSubview(valueLabel)
    }
    
    private func activateConstraints() {
        titleLabel.snp.makeConstraints { make in
            make.centerY.leading.equalToSuperview()
            make.width.equalTo(LocalizedLanguage.isChinese ? 40.scale375() : 63.scale375())
            make.height.equalTo(20.scale375Height())
        }
        valueLabel.snp.makeConstraints { make in
            make.leading.equalTo(titleLabel.snp.trailing).offset(8.scale375())
            make.centerY.equalToSuperview()
        }
    }
}

fileprivate extension String {
    
    static let localText = internalLocalized("Local User")
    static let remoteText = internalLocalized("Remote User")
    
    static let videoText = internalLocalized("Video Information")
    static let videoResolutionText = internalLocalized("Video Resolution")
    static let bitrateText = internalLocalized("Video Bitrate")
    static let videoFrameRateText = internalLocalized("Video FPS")
    
    static let audioText = internalLocalized("Audio Information")
    static let audioSampleRateText = internalLocalized("Sample Rate")
}
