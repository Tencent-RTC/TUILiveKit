//
//  KaraokePitchViewConfig.swift
//  TUIKaraoke
//
//  Created by adams on 2023/6/5.
//

import UIKit

class KaraokePitchViewConfig: NSObject {
    // MARK: - 音高相关
    // 音高等级数 默认值: 20
    var pitchNum: Int = 20
    // 最大音高值 默认值: 90
    var maxPitch: Int = 90
    // 最小音高值 默认值: 5
    var minPitch: Int = 5
    // 控件开始至竖线这一段表示的时间, 单位 ms 默认值: 1175
    var timeElapsedOnScreen: Int = 1_175
    // 竖线至控件末尾这一段表示的时间, 单位 ms 默认值: 2750
    var timeToPlayOnScreen: Int = 2_750
    // 调用TUIPitchView.setCurrentSongProgress:pitch: 方法的大致时间间隔, 单位 ms
    var estimatedCallInterval: CGFloat = 20
    
    // MARK: - 颜色相关
    // 竖线颜色
    var verticalLineColor: UIColor = UIColor.white.withAlphaComponent(0.45)
    // 标准音调颜色
    var standardRectColor: UIColor = UIColor.white.withAlphaComponent(0.7)
    // 击中音调颜色
    var hitRectColor: UIColor = UIColor("FF6A4C")
    // 音调指示器颜色
    var pitchIndicatorColor: UIColor = .white
    // 分数文本颜色
    var scoreTextColor: UIColor = UIColor("FF6A4C")
}
