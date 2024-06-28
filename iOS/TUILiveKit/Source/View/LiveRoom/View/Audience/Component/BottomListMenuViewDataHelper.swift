//
//  BottomPopupListViewDataHelper.swift
//  Alamofire
//
//  Created by aby on 2024/5/31.
//

import Foundation

class BottomPopupListViewDataHelper {
    deinit {
        print("deinit \(type(of: self))")
    }
    // audience take seat list menu.
    func generateLinkTypeMenuData(store: LiveStore, routerStore: RouterStore) -> [LinkMicTypeCellData] {
        var data = [LinkMicTypeCellData]()
        data.append(LinkMicTypeCellData(image: .liveBundleImage("live_link_video"),
                                        text: .videoLinkRequestText,
                                        action: {
            self.applyToTakeSeat(store: store, needOpenCamera: true)
            routerStore.router(action: .dismiss)
        }))
        
        data.append(LinkMicTypeCellData(image: .liveBundleImage("live_link_audio"),
                                        text: .audioLinkRequestText,
                                        action: {
            self.applyToTakeSeat(store: store, needOpenCamera: false)
            routerStore.router(action: .dismiss)
        }))
        return data
    }
    
    func generateAudienceBottomMenuData(store: LiveStore, routerStore: RouterStore) -> [ButtonMenuInfo] {
        var menus: [ButtonMenuInfo] = []
        var gift = ButtonMenuInfo(normalIcon: "live_gift_icon", normalTitle: "")
        gift.tapAction = { sender in
            routerStore.router(action: .present(.giftView))
        }
        menus.append(gift)
        var linkMic = ButtonMenuInfo(normalIcon: "live_link_icon", selectIcon: "live_linking_icon")
        linkMic.tapAction = { sender in
            if sender.isSelected {
                let requestId = store.selectCurrent(SeatSelectors.getMySeatApplicationId)
                store.dispatch(action: SeatActions.cancelApplication(payload: requestId))
            } else {
                if store.selectCurrent(UserSelectors.isOnSeat) {
                    store.dispatch(action: SeatActions.leaveSeat())
                } else {
                    routerStore.router(action: .present(.linkType))
                }
            }
        }
        linkMic.bindStateClosure = { button, cancellableSet in
            store
                .select(SeatSelectors.getMySeatApplicationId)
                .receive(on: RunLoop.main)
                .map { !$0.isEmpty }
                .assign(to: \MenuButton.isSelected, on: button)
                .store(in: &cancellableSet)
            store
                .select(UserSelectors.isOnSeat)
                .receive(on: RunLoop.main)
                .sink { isOnSeat in
                    let imageName = isOnSeat ? "live_linked_icon" : "live_link_icon"
                    button.setImage(.liveBundleImage(imageName), for: .normal)
                }
                .store(in: &cancellableSet)
        }
        menus.append(linkMic)
        var like = ButtonMenuInfo(normalIcon: "live_like_icon")
        like.tapAction = { sender in
            store.dispatch(action: UserResponseActions.like())
        }
        menus.append(like)
        return menus
    }
}

extension BottomPopupListViewDataHelper {
    func applyToTakeSeat(store:LiveStore, needOpenCamera: Bool) {
        store.dispatch(action: ViewActions.updateAutoOpenCameraOnSeated(payload: needOpenCamera))
        store.dispatch(action: SeatActions.takeSeat(payload: nil))
    }
}

private extension String {
    static let videoLinkRequestText = localized("live.audience.linkType.videoLinkRequest")
    static var audioLinkRequestText: String =
        localized("live.audience.linkType.audioLinkRequest")
}
