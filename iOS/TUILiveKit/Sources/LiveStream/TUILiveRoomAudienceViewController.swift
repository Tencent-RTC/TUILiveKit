//
//  TUILiveRoomAudienceViewController.swift
//  TUILiveKit
//
//  Created by WesleyLei on 2023/12/11.
//
import UIKit
import Combine
import TUICore
import LiveStreamCore
import RTCRoomEngine
import RTCCommon

public class TUILiveRoomAudienceViewController: UIViewController {
    
    private lazy var sliderView: LiveListPagerView = {
        let view = LiveListPagerView()
        view.dataSource = self
        view.delegate = self
        return view
    }()
    
    private weak var coreView: LiveCoreView?
    
    // MARK: - private property.
    var roomId: String
    private let routerManager: LSRouterManager = LSRouterManager()
    private var cancellableSet = Set<AnyCancellable>()
    private lazy var routerCenter: LSRouterControlCenter = {
        let rootRoute: LSRoute = .audience
        let routerCenter = LSRouterControlCenter(rootViewController: self, rootRoute: rootRoute, routerManager: routerManager)
        return routerCenter
    }()
    private var ownerId = ""
    private var cursor = ""
    private let fetchCount = 20
    private var isFirstFetch = true
    private var isFirstRoom = true
    private var relayoutCoreViewClosure: () -> Void = {}
    
    public init(roomId: String) {
        self.roomId = roomId
        super.init(nibName: nil, bundle: nil)
    }
    
    public init(liveInfo: TUILiveInfo) {
        self.roomId = liveInfo.roomInfo.roomId
        super.init(nibName: nil, bundle: nil)
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    deinit {
        StateCache.shared.clear()
        print("deinit \(type(of: self))")
    }
    
    public func leaveLive(onSuccess: TUISuccessBlock?, onError: TUIErrorBlock?) {
        coreView?.leaveLiveStream(onSuccess: {
            onSuccess?()
        }, onError: { code, message in
            onError?(code, message)
        })
    }
    
    public override func viewDidLoad() {
        super.viewDidLoad()
        subscribeRouter()
        constructViewHierarchy()
        activateConstraints()
        view.backgroundColor = .black
    }
    
    public override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        navigationController?.setNavigationBarHidden(true, animated: true)
        UIApplication.shared.isIdleTimerDisabled = true
    }
    
    public override func viewDidDisappear(_ animated: Bool) {
        super.viewDidDisappear(animated)
        UIApplication.shared.isIdleTimerDisabled = false
        navigationController?.setNavigationBarHidden(false, animated: true)
    }
}

extension TUILiveRoomAudienceViewController {
    private func subscribeRouter() {
        routerCenter.subscribeRouter()
    }
    
    private func constructViewHierarchy() {
        view.backgroundColor = .g1
        view.addSubview(sliderView)
    }
    
    private func activateConstraints() {
        sliderView.snp.remakeConstraints { make in
            make.edges.equalToSuperview()
        }
    }
}

// MARK: - FloatWindowDataSource
extension TUILiveRoomAudienceViewController: FloatWindowDataSource {
    func getRoomId() -> String {
        roomId
    }
    
    func getOwnerId() -> String {
        ownerId
    }

    func getCoreView() -> LiveCoreView {
        return coreView ?? LiveCoreView()
    }
    
    func relayoutCoreView() {
        relayoutCoreViewClosure()
    }
    
    func getIsLinking() -> Bool {
        guard let coGuestState: CoGuestState = coreView?.getState(),
              let userState: UserState = coreView?.getState() else { return false }
        return !coGuestState.seatList.filter({ $0.userId == userState.selfInfo.userId }).isEmpty
    }
}

extension TUILiveRoomAudienceViewController: LiveListViewDataSource {
    public func fetchLiveList(completionHandler: @escaping LiveListCallback) {
        guard cursor != "" || isFirstFetch else { return }
        isFirstFetch = false
        let liveListManager = TUIRoomEngine.sharedInstance().getExtension(extensionType: .liveListManager) as? TUILiveListManager
        var resultList: [LiveInfo] = []
        liveListManager?.fetchLiveList(cursor: cursor, count: fetchCount) { [weak self] cursor, list in
            guard let self = self else { return }
            self.cursor = cursor
            if isFirstRoom {
                var liveInfo = LiveInfo()
                liveInfo.roomId = roomId
                resultList.append(liveInfo)
                isFirstRoom = false
                let filteredList = list.filter { tuiLiveInfo in
                    tuiLiveInfo.roomInfo.roomId != self.roomId
                }.map { tuiLiveInfo in
                    LiveInfo(tuiLiveInfo: tuiLiveInfo)
                }
                resultList.append(contentsOf: filteredList)
            } else {
                let liveInfoList = list.map { tuiLiveInfo in
                    LiveInfo(tuiLiveInfo: tuiLiveInfo)
                }
                resultList.append(contentsOf: liveInfoList)
            }
            completionHandler(resultList)
        } onError: { [weak self] code, message in
            guard let self = self else { return }
            LiveKitLog.error("\(#file)","\(#line)","fetchLiveList:[onError:[code:\(code),message:\(message)]]")
            var liveInfo = LiveInfo()
            liveInfo.roomId = self.roomId
            resultList.append(liveInfo)
            completionHandler(resultList)
        }
    }
}

extension TUILiveRoomAudienceViewController: LiveListViewDelegate {
    public func onCreateView(liveInfo: LiveInfo) -> UIView {
        let audienceCell = AudienceSliderCell(roomId: liveInfo.roomId, routerManager: routerManager, routerCenter: routerCenter, audienceVC: self)
        audienceCell.delegate = self
        return audienceCell
    }
    
    public func onViewWillSlideIn(view: UIView) {
        if let view = view as? AudienceSliderCell {
            view.onViewWillSlideIn()
        }
    }
    
    public func onViewDidSlideIn(view: UIView) {
        if let view = view as? AudienceSliderCell {
            view.onViewDidSlideIn()
        }
    }
    
    public func onViewSlideInCancelled(view: UIView) {
        if let view = view as? AudienceSliderCell {
            view.onViewSlideInCancelled()
        }
    }
    
    public func onViewWillSlideOut(view: UIView) {
        if let view = view as? AudienceSliderCell {
            view.onViewWillSlideOut()
        }
    }
    
    public func onViewDidSlideOut(view: UIView) {
        if let view = view as? AudienceSliderCell {
            view.onViewDidSlideOut()
        }
    }
    
    public func onViewSlideOutCancelled(view: UIView) {
        if let view = view as? AudienceSliderCell {
            view.onViewSlideOutCancelled()
        }
    }
}

extension TUILiveRoomAudienceViewController: AudienceListCellDelegate {
    func handleScrollToNewRoom(roomId: String, ownerId: String, manager: LiveStreamManager,
                               coreView: LiveCoreView, routerProvider: LSRouterViewProvider,
                               relayoutCoreViewClosure: @escaping () -> Void) {
        routerCenter.handleScrollToNewRoom(manager: manager, coreView: coreView, routerProvider: routerProvider)
        self.roomId = roomId
        self.ownerId = ownerId
        self.coreView = coreView
        self.relayoutCoreViewClosure = relayoutCoreViewClosure
    }
    
    func showFloatWindow() {
        FloatWindow.shared.showFloatWindow(controller: self)
    }
    
    func showToast(message: String) {
        view.makeToast(message)
    }
    
    func disableScrolling() {
        sliderView.disableScrolling()
    }
    
    func enableScrolling() {
        sliderView.enableScrolling()
    }
    
    func scrollToNextPage() {
        sliderView.scrollToNextPage()
    }
}
