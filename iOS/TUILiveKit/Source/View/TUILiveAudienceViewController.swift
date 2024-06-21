//
//  TUILiveAudienceViewController.swift
//  TUILiveKit
//
//  Created by adamsfliu on 2024/6/18.
//

import UIKit
import Combine
import RTCRoomEngine

public class TUILiveAudienceViewController: UIViewController {
    @Injected var store: LiveStore
    @Injected var listStore: LiveListStore
    
    private lazy var routerCenter: RouterControlCenter = {
        let routerCenter = RouterControlCenter(rootViewController: self, rootRoute: .audience)
        return routerCenter
    }()
    
    private var displayLiveInfo: TUILiveInfo
    
    private lazy var liveInfoListPublisher = self.listStore.select(LiveListSelectors.getLiveInfoList)
    private var cancellableSet = Set<AnyCancellable>()
    private var liveInfoList: [TUILiveInfo] = []
    
    private let displayCollectionView: UICollectionView = {
        let flowLayout = UICollectionViewFlowLayout()
        flowLayout.scrollDirection = .vertical
        flowLayout.itemSize = UIScreen.main.bounds.size
        flowLayout.minimumLineSpacing = 0
        flowLayout.minimumLineSpacing = 0
        
        let collectionView = UICollectionView(frame: .zero, collectionViewLayout: flowLayout)
        collectionView.isPagingEnabled = true
        collectionView.showsVerticalScrollIndicator = false
        collectionView.contentInsetAdjustmentBehavior = .never
        collectionView.register(TUILiveDisplayCell.self, forCellWithReuseIdentifier: TUILiveDisplayCell.reuseIdentifier)
        return collectionView
    }()
    
    init(liveInfo: TUILiveInfo) {
        self.displayLiveInfo = liveInfo
        self.liveInfoList.append(liveInfo)
        super.init(nibName: nil, bundle: nil)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        constructViewHierarchy()
        activateConstraints()
        bindInteraction()
        subscribeLiveListState()
    }
    
    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(true, animated: false)
    }
    
    public override func viewDidDisappear(_ animated: Bool) {
        super.viewDidDisappear(animated)
        navigationController?.setNavigationBarHidden(false, animated: false)
    }
}

extension TUILiveAudienceViewController {
    
    private func constructViewHierarchy() {
        view.addSubview(displayCollectionView)
    }
    
    private func activateConstraints() {
        displayCollectionView.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
    
    private func bindInteraction() {
        displayCollectionView.delegate = self
        displayCollectionView.dataSource = self
        routerCenter.subscribeRouter()
        listStore.dispatch(action: LiveListActions.getLiveInfoList(payload: ""))
    }
    
    private func subscribeLiveListState() {
        liveInfoListPublisher
            .receive(on: RunLoop.main)
            .sink { [weak self] result in
                guard let self = self else { return }
                let filterLiveInfoList = result.liveInfoList.filter { [weak self] liveInfo in
                    guard let self = self else { return true }
                    return liveInfo.roomInfo.roomId != self.displayLiveInfo.roomInfo.roomId
                }
                self.liveInfoList.append(contentsOf: filterLiveInfoList)
                self.displayCollectionView.reloadData()
            }
            .store(in: &cancellableSet)
    }
}

extension TUILiveAudienceViewController: UICollectionViewDelegate {
    public func collectionView(_ collectionView: UICollectionView, willDisplay cell: UICollectionViewCell, forItemAt indexPath: IndexPath) {
        if liveInfoList.count - indexPath.item <= 3 {
            let cursor = listStore.selectCurrent(LiveListSelectors.getLiveInfoList).cursor
            if cursor != "" {
                listStore.dispatch(action: LiveListActions.getLiveInfoList(payload: cursor))
            }
        }
    }
    
    public func collectionView(_ collectionView: UICollectionView, didEndDisplaying cell: UICollectionViewCell, forItemAt indexPath: IndexPath) {
        
    }
}

extension TUILiveAudienceViewController: UICollectionViewDataSource {
    public func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        return liveInfoList.count
    }
    
    public func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        let cell = collectionView.dequeueReusableCell(withReuseIdentifier: TUILiveDisplayCell.reuseIdentifier, for: indexPath)
        let liveInfo = liveInfoList[indexPath.item]
        guard let roomType = LiveIdentityGenerator.shared.getIDType(liveInfo.roomInfo.roomId) else {
            return cell
        }
        if let displayViewCell = cell as? TUILiveDisplayCell {
            switch roomType {
            case .live:
                displayViewCell.displayLiveRoom(liveInfo: liveInfo)
                routerCenter.routerProvider = displayViewCell.liveRoomView
            case .voice:
                displayViewCell.displayVoiceRoom(liveInfo: liveInfo)
                routerCenter.routerProvider = displayViewCell.voiceRoomView
            }
            
        }
        return cell
    }
}

class TUILiveDisplayCell: UICollectionViewCell {
    static let reuseIdentifier = "TUILiveDisplayCell"
    
    var liveRoomView: AudienceView?
    var voiceRoomView: VoiceRoomRootView?
    
    override init(frame: CGRect) {
        super.init(frame: frame)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        print("deinit \(type(of: self))")
    }
    
    override func prepareForReuse() {
        if let view = voiceRoomView {
            view.removeFromSuperview()
        }
        
        if let view = liveRoomView {
            view.removeFromSuperview()
        }
    }
    
    func displayVoiceRoom(liveInfo: TUILiveInfo) {
        let view = VoiceRoomRootView(frame: .zero,
                                     roomId: liveInfo.roomInfo.roomId)
        contentView.addSubview(view)
        view.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        voiceRoomView = view
        debugPrint("adams: \(view)")
    }
    
    func displayLiveRoom(liveInfo: TUILiveInfo) {
        let view = AudienceView(roomId: liveInfo.roomInfo.roomId)
        contentView.addSubview(view)
        view.snp.makeConstraints { make in
            make.edges.equalToSuperview()
        }
        liveRoomView = view
        debugPrint("adams: \(view)")
    }
}
