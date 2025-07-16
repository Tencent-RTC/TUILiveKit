//
//  VRSeatService.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/15.
//

import RTCRoomEngine
import Combine

class VRSeatService: BaseServiceProtocol {
    var roomEngine: TUIRoomEngine
    required init(roomEngine: TUIRoomEngine) {
        self.roomEngine = roomEngine
    }
    
    deinit {
        debugPrint("deinit \(type(of: self))")
    }
    
    func getSeatList() async throws -> [TUISeatInfo] {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "getSeatList")
            roomEngine.getSeatList { seatList in
                LiveKitLog.info("\(#file)", "\(#line)", "getSeatList[onSuccess]")
                continuation.resume(returning: seatList)
            } onError: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)", "getSeatList[onError:[error:\(err) message:\(message)]]")
                let error = InternalError(code: err.rawValue, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
    
    func fetchSeatApplicationList() async throws -> [VRSeatApplication] {
        return try await withCheckedThrowingContinuation { [weak self] continuation in
            guard let self = self else { return }
            LiveKitLog.info("\(#file)", "\(#line)", "getSeatApplicationList")
            roomEngine.getSeatApplicationList { requests in
                LiveKitLog.info("\(#file)", "\(#line)", "getSeatApplicationList:[onSuccess]")
                let result = requests.map { request in
                    let seatApplication = VRSeatApplication(request: request)
                    return seatApplication
                }
                continuation.resume(returning: result)
            } onError: { err, message in
                LiveKitLog.error("\(#file)", "\(#line)", "getSeatApplicationList[onError:[error:\(err) message:\(message)]]")
                let error = InternalError(code: err.rawValue, message: message)
                continuation.resume(throwing: error)
            }
        }
    }
}
