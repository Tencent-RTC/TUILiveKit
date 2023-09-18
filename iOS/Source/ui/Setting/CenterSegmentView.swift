//
//  CenterSegmentView.swift
//  TRTCScenesDemo
//
//  Created by J J on 2020/5/15.
//  Copyright © 2022 Tencent. All rights reserved.
//

import TUIKitCommon
import UIKit

class CenterSegmentView: UIView {
    typealias PageBlock = (_ selectedIndex: Int) -> Void
    var pageBlock: PageBlock?

    var selectedIndex: Int = 0

    var nameArray: [String] = []

    var segmentScrollVHeight: CGFloat = 41

    var titleNormalColor: UIColor = .gray

    var titleSelectColor: UIColor = UIColor(hex: "006EFF") ?? .blue

    var selectFont = UIFont.systemFont(ofSize: 18)

    var normalFont = UIFont.systemFont(ofSize: 17)

    var lineSelectedColor = UIColor(hex: "006EFF") ?? .blue

    var downColor = UIColor(hex: "EEEEEE")

    var lineHeight: CGFloat = 1

    lazy var segmentView: UIScrollView = {
        let view = UIScrollView(frame: CGRect.zero)
        view.showsVerticalScrollIndicator = false
        view.showsHorizontalScrollIndicator = false
        return view
    }()

    lazy var segmentScrollView: UIScrollView = {
        let view = UIScrollView()
        view.isScrollEnabled = false
        return view
    }()

    lazy var line: UILabel = {
        let label = UILabel(frame: CGRect.zero)
        return label
    }()

    lazy var seleBtn: UIButton = {
        let btn = UIButton(frame: CGRect.zero)
        return btn
    }()

    lazy var down: UILabel = {
        let label = UILabel(frame: CGRect.zero)
        label.backgroundColor = UIColor.gray
        return label
    }()

    var controllers: [UIViewController] = []

    convenience init(frame: CGRect, controllers: [UIViewController], titleArray: [String], selectIndex: Int, lineHeight: CGFloat) {
        self.init(frame: frame)
        self.controllers = controllers
        nameArray = titleArray
        self.lineHeight = lineHeight
        initData()
    }

    let widthTop: CGFloat = (ScreenWidth - 50 * 2 - 30 * 2) / 3

    func initData() {
        if nameArray.count == 0 && controllers.count == 0 {
            return
        }

        let avgWidth: CGFloat = 16
        segmentView.frame = CGRect(x: 0, y: 0, width: frame.size.width, height: segmentScrollVHeight)
        segmentView.tag = 50
        addSubview(segmentView)

        segmentScrollView.frame = CGRect(x: 0, y: segmentScrollVHeight, width: frame.size.width, height: frame.size.height - segmentScrollVHeight)
        segmentScrollView.contentSize = CGSize(width: frame.size.width * CGFloat(controllers.count), height: 0)
        segmentScrollView.delegate = self
        segmentScrollView.showsHorizontalScrollIndicator = false

        segmentScrollView.isPagingEnabled = true
        segmentScrollView.bounces = false

        addSubview(segmentScrollView)

        for (index, controller) in controllers.enumerated() {
            segmentScrollView.addSubview(controller.view)
            controller.view.frame = CGRect(x: CGFloat(index) * frame.size.width, y: 0, width: frame.size.width, height: frame.size.height)
        }

        for (index, _) in controllers.enumerated() {
            let btn = UIButton(type: UIButton.ButtonType.custom)
            btn.frame = CGRect(x: 50 + CGFloat(index) * (widthTop + 30), y: 0, width: widthTop, height: segmentScrollVHeight)
            btn.backgroundColor = .clear
            btn.tag = index
            btn.setTitle(nameArray[index], for: .normal)
            btn.setTitleColor(titleNormalColor, for: .normal)
            btn.setTitleColor(titleSelectColor, for: .selected)
            btn.addTarget(self, action: #selector(Click(sender:)), for: .touchUpInside)

            if selectedIndex == index {
                btn.isSelected = true
                seleBtn = btn
                btn.titleLabel?.font = selectFont
                segmentScrollView.setContentOffset(CGPoint(x: CGFloat(btn.tag) * frame.size.width, y: 0), animated: true)
                NotificationCenter.default.post(name: NSNotification.Name(rawValue: "SelectVC"), object: btn, userInfo: nil)
            } else {
                btn.isSelected = false
                btn.titleLabel?.font = normalFont
            }
            segmentView.addSubview(btn)
            btn.snp.makeConstraints { maker in
                maker.centerX.equalToSuperview()
            }
        }

        let downFrame = CGRect(x: 0, y: segmentScrollVHeight - 0.5, width: frame.size.width, height: 0.5)
        down = UILabel(frame: downFrame)
        down.backgroundColor = downColor
        segmentView.addSubview(down)

        let lineFrame = CGRect(x: avgWidth * CGFloat(selectedIndex), y: segmentScrollVHeight - lineHeight, width: avgWidth, height: lineHeight)
        line = UILabel(frame: lineFrame)
        line.backgroundColor = lineSelectedColor
        line.tag = 100
        line.clipsToBounds = true
        line.layer.cornerRadius = lineHeight * 0.5

        var lineCeter = line.center
        lineCeter.x = segmentView.width / 2
        line.center = lineCeter
        segmentView.addSubview(line)
    }

    override init(frame: CGRect) {
        super.init(frame: frame)
    }

    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    @objc func Click(sender: UIButton) {
        seleBtn.titleLabel?.font = normalFont
        seleBtn.isSelected = false
        seleBtn = sender
        pageBlock?(sender.tag)
        seleBtn.titleLabel?.font = selectFont
        seleBtn.isSelected = true
        if sender.tag != 0 {
            controllers.first?.view.endEditing(true)
        }
        segmentScrollView.setContentOffset(CGPoint(x: CGFloat(sender.tag) * frame.size.width, y: 0), animated: true)
        NotificationCenter.default.post(name: NSNotification.Name(rawValue: "SelectVC"), object: sender, userInfo: nil)
    }
}

extension CenterSegmentView: UIScrollViewDelegate {
    func scrollViewDidEndDecelerating(_ scrollView: UIScrollView) {
        let tag = Int(segmentScrollView.contentOffset.x / frame.size.width)
        if tag != 0 {
            controllers.first?.view.endEditing(true)
        }
        let btn = segmentView.viewWithTag(tag)
        seleBtn.isSelected = false
        seleBtn.titleLabel?.font = normalFont
        if let button = btn {
            seleBtn = button as! UIButton
            seleBtn.isSelected = true
            seleBtn.titleLabel?.font = selectFont
            pageBlock?(button.tag)
        }
    }

    func scrollViewDidScroll(_ scrollView: UIScrollView) {
        let itemWidth = widthTop + 30
        let offsetX = (itemWidth / scrollView.bounds.width) * scrollView.contentOffset.x
        let xoffset = offsetX - (CGFloat(selectedIndex) * itemWidth)
        line.transform = CGAffineTransform(translationX: xoffset, y: 0)
    }
}
