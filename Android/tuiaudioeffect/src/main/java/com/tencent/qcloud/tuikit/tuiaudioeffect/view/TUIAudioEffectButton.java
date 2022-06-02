package com.tencent.qcloud.tuikit.tuiaudioeffect.view;

import android.content.Context;
import android.view.View;

import com.tencent.liteav.audio.TXAudioEffectManager;
import com.tencent.qcloud.tuikit.tuiaudioeffect.R;
import com.tencent.qcloud.tuikit.tuiaudioeffect.model.AudioEffectModel;
import com.tencent.qcloud.tuikit.tuiaudioeffect.model.IAudioEffectModel;
import com.tencent.qcloud.tuikit.tuiaudioeffect.presenter.AudioEffectPresenter;

/**
 * TUIAudioEffect 插件视图
 *
 * 小图标按钮，点击后可以弹出音效面板 {@link TUIAudioEffectView}
 */
public final class TUIAudioEffectButton extends androidx.appcompat.widget.AppCompatImageView {

    private final TUIAudioEffectView mAudioEffectView; // 音效面板Dialog

    public TUIAudioEffectButton(Context context, TXAudioEffectManager manager) {
        super(context);
        IAudioEffectModel model = new AudioEffectModel(context);
        AudioEffectPresenter presenter = new AudioEffectPresenter(model);
        presenter.setAudioEffectManager(manager);
        mAudioEffectView = new TUIAudioEffectView(context, presenter);
        init();
    }

    private void init() {
        setImageResource(R.drawable.tuiaudioeffect_icon);
        setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View view) {
                mAudioEffectView.show();
            }
        });
    }
}
