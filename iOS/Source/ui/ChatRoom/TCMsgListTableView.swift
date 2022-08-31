//
//  TCMsgListTableView.swift
//  TUILiveRoom
//
//  Created by origin 李 on 2021/6/22.
//  Copyright © 2022 Tencent. All rights reserved.

import Foundation
///  TCMsgListTableView
class TCMsgListTableView: UITableView{
    var msgArray: [TCMsgModel] = []
    var beginScroll: Bool
    var canScrollToBottom: Bool
    var canReload: Bool
    
    override init(frame: CGRect, style: UITableView.Style) {
        msgArray = [TCMsgModel]()
        beginScroll = false
        canScrollToBottom = true
        canReload = true
        super.init(frame: frame, style: style)
        initTableView()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    func initTableView() {
        delegate = self
        dataSource = self
        backgroundView = nil
        backgroundColor = UIColor.clear
        separatorStyle = .none
        showsVerticalScrollIndicator = false
        isHidden = true
        register(TCMsgListCell.classForCoder(), forCellReuseIdentifier: "MsgListCell")
        
        
        
    }
    
    func calCellHeight(_ attribText: NSAttributedString?) -> CGFloat {
        let rect = attribText?.boundingRect(with: CGSize(width: width - 20, height: CGFloat.greatestFiniteMagnitude), options:
         .usesLineFragmentOrigin, context: nil)
        let cellHeight = (rect?.size.height ?? 0.0) + 10
        return cellHeight
    }
    
    func bulletNewMsg(_ msgModel: TCMsgModel?) {
        if var msgModel = msgModel {
            if msgArray.count > 1000 {
                if let subRange = Range(NSRange(location: 0, length: 100)) {
                    msgArray.removeSubrange(subRange)
                }
            }
            msgModel.msgAttribText = TCMsgListCell.getAttributedString(from: msgModel)
            msgModel.msgHeight = calCellHeight(msgModel.msgAttribText)
            msgArray.append(msgModel)
            isHidden = false
            if canReload {
                canReload = false
                DispatchQueue.main.asyncAfter(deadline: DispatchTime.now() + 0.5, execute: { [weak self] in
                    guard let `self` = self else { return }
                    self.canReload = true
                    self.reloadData()
                    if !self.beginScroll {
                        if self.calculateTotalCellHeight() >= self.height {
                            self.scrollToBottom()
                            self.beginScroll = true
                        }
                    } else {
                        self.scrollToBottom()
                    }
                })
            }
        }
    }
    
    func scrollToBottom() {
        if canScrollToBottom {
            let n = Int(min(msgArray.count, numberOfRows(inSection: 0)))
            scrollToRow(at: IndexPath(row: n - 1, section: 0), at: .none, animated: false)
        }
    }
    
    func calculateCellHeight(_ indexPath: IndexPath) -> CGFloat {
        let msgModel = msgArray[indexPath.row]
        let msg = TCMsgListCell.getAttributedString(from: msgModel)
        let rect = msg?.boundingRect(with: CGSize(width: width - 20, height: CGFloat.greatestFiniteMagnitude), options:
         .usesLineFragmentOrigin, context: nil)
        let cellHeight = (rect?.size.height ?? 0.0) + 10
        return cellHeight
    }
    
    func calculateTotalCellHeight() -> CGFloat {
        var totalCellHeight: Int = 0
        for model in msgArray {
            totalCellHeight += Int(model.msgHeight)
        }
        return CGFloat(totalCellHeight)
    }
}

//MARK: - UITableViewDelegate
extension TCMsgListTableView: UITableViewDelegate{
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat{
        if msgArray.count > indexPath.row{
            let msgModel = msgArray[indexPath.row]
            return msgModel.msgHeight
        }
        return 20
    }
    
    func tableView(_ tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat{
        return 0.01
    }
    
    func tableView(_ tableView: UITableView, heightForFooterInSection section: Int) -> CGFloat{
        return 0.01
    }
    
    func tableView(_ tableView: UITableView, estimatedHeightForRowAt indexPath: IndexPath) -> CGFloat{
        return 20
    }
}

//MARK: - UITableViewDataSource
extension TCMsgListTableView: UITableViewDataSource{
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return msgArray.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        
        let cellID = "MsgListCell"
        let cell = tableView.dequeueReusableCell(withIdentifier: cellID, for: indexPath)
        if let msgCell = cell as? TCMsgListCell {
            msgCell.accessoryType = UITableViewCell.AccessoryType.none
            msgCell.backgroundColor = UIColor.clear
            msgCell.selectionStyle = UITableViewCell.SelectionStyle.none
            if msgArray.count > indexPath.row {
                let msgModel = msgArray[indexPath.row]
                msgCell.refresh(with: msgModel)
            }
        }
        return cell
    }
}

extension TCMsgListTableView: UIScrollViewDelegate{
    func scrollViewWillBeginDragging(_ scrollView: UIScrollView) {
        canScrollToBottom = false
    }
    
    func scrollViewDidEndDecelerating(_ scrollView: UIScrollView) {
        let tableViewOffset = contentOffset.y + frame.size.height
        if tableViewOffset + 10 >= calculateTotalCellHeight() {
            canScrollToBottom = true
        }
    }
}

protocol TCAudienceListDelegate: NSObjectProtocol {
    
    func onFetchGroupMemberList(_ errCode: Int, memberCount: Int)
}

//MARK: - Audience List
class TCAudienceListTableView: UITableView{
    weak var audienceListDelegate: TCAudienceListDelegate?
    private var liveInfo: TRTCLiveRoomInfo?
    private var dataArray: [TRTCLiveUserInfo]
    init(frame: CGRect, style: UITableView.Style, live _liveInfo: TRTCLiveRoomInfo) {
        liveInfo = _liveInfo
        dataArray = [TRTCLiveUserInfo]()
        super.init(frame: frame, style: style)
        separatorStyle = .none
        showsVerticalScrollIndicator = false
        backgroundColor = UIColor.clear
        dataSource = self
        delegate = self
        register(TCAudienceListCell.classForCoder(), forCellReuseIdentifier: "AudienceListCell")
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    func isAlready(inAudienceList model: TCMsgModel) -> Bool {
        guard model.userId != nil else { return false }
        for data in dataArray {
            if data.userId == model.userId {
                return true
            }
        }
        return false
    }
    
    func refreshAudienceList(_ model: TCMsgModel?) {
        guard let model = model  else {
            return
        }
        if model.userId == nil {
            return
        }
        for data in dataArray {
            if data.userId == model.userId {
                dataArray.remove(at: (dataArray.firstIndex(of: data)!))
                break
            }
        }
        if model.msgType == .memberEnterRoom {
            let infoData = TRTCLiveUserInfo()
            if let userId = model.userId {
                infoData.userId = userId
            }
            infoData.avatarURL = model.userHeadImageUrl ?? ""
            dataArray.insert(infoData, at: 0)
        }
        reloadData()
    }
}

//MARK: - UITableViewDelegate
extension TCAudienceListTableView: UITableViewDelegate{
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat{
        return CGFloat(IMAGE_SIZE + IMAGE_SPACE)
    }
    
    func tableView(_ tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat{
        return 0.01
    }
    
    func tableView(_ tableView: UITableView, heightForFooterInSection section: Int) -> CGFloat{
        return 0.01
    }
}

//MARK: - UITableViewDataSource
extension TCAudienceListTableView: UITableViewDataSource{
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return dataArray.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cellID = "AudienceListCell"
        let cell = dequeueReusableCell(withIdentifier: cellID, for: indexPath)
        if let audienceListCell = cell as? TCAudienceListCell{
            audienceListCell.accessoryType = UITableViewCell.AccessoryType.none
            audienceListCell.backgroundColor = UIColor.clear
            audienceListCell.selectionStyle = UITableViewCell.SelectionStyle.none
            if dataArray.count > indexPath.row {
                let msgModel = dataArray[indexPath.row]
                audienceListCell.refresh(withModel:msgModel)
            }
        }
        return cell
    }
}
