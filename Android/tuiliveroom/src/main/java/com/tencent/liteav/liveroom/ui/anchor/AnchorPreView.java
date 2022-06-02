package com.tencent.liteav.liveroom.ui.anchor;

import android.content.Context;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.EditText;
import android.widget.FrameLayout;
import android.widget.RadioButton;
import android.widget.RadioGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.tencent.liteav.basic.UserModel;
import com.tencent.liteav.basic.UserModelManager;
import com.tencent.liteav.debug.BuildConfig;
import com.tencent.liteav.liveroom.R;
import com.tencent.liteav.liveroom.model.TRTCLiveRoom;
import com.tencent.liteav.liveroom.ui.common.utils.TCUtils;
import com.tencent.trtc.TRTCCloudDef;

import de.hdodenhof.circleimageview.CircleImageView;


public class AnchorPreView extends FrameLayout {

    private View            mViewRoot;
    private TextView        mTextRoomName;
    private CircleImageView mImageCover;
    private EditText        mEditRoomName;
    private RadioGroup      mRadioGroup;
    private RadioButton     mRadioBtnMusic;

    protected String mAnchorAvatar;
    protected String mAnchorCoverUrl;
    protected String mAnchorName;
    protected String mAnchorId;

    public AnchorPreView(@NonNull Context context) {
        this(context, null);
    }

    public AnchorPreView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        mViewRoot = LayoutInflater.from(getContext()).inflate(R.layout.trtcliveroom_anchor_preview, this, true);
        initView();
    }

    private void initView() {
        mTextRoomName = mViewRoot.findViewById(R.id.tv_live_room_name);
        mImageCover = mViewRoot.findViewById(R.id.img_live_room_cover);
        mEditRoomName = mViewRoot.findViewById(R.id.et_live_room_name);
        mRadioBtnMusic = mViewRoot.findViewById(R.id.rb_live_room_quality_music);
        mRadioGroup = mViewRoot.findViewById(R.id.radio_group_audio_quality);

        UserModel userModel = UserModelManager.getInstance().getUserModel();
        mAnchorId = userModel.userId;
        mAnchorName = userModel.userName;
        mAnchorAvatar = userModel.userAvatar;
        mAnchorCoverUrl = userModel.userAvatar;

        mEditRoomName.setFocusableInTouchMode(!BuildConfig.RTCube_APPSTORE);
        if (!TextUtils.isEmpty(mAnchorName)) {
            mEditRoomName.setText(getContext().getString(R.string.trtcliveroom_create_room_default, mAnchorName));
        }

        TCUtils.showPicWithUrl(getContext(), mImageCover, mAnchorAvatar, R.drawable.trtcliveroom_bg_cover);
        mTextRoomName.setText(mAnchorName);

        final TRTCLiveRoom liveRoom = TRTCLiveRoom.sharedInstance(getContext());
        if (mRadioBtnMusic.isChecked()) {
            liveRoom.setAudioQuality(TRTCCloudDef.TRTC_AUDIO_QUALITY_MUSIC);
        } else {
            liveRoom.setAudioQuality(TRTCCloudDef.TRTC_AUDIO_QUALITY_DEFAULT);
        }
        mRadioGroup.setOnCheckedChangeListener(new RadioGroup.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(RadioGroup group, int checkedId) {
                if (checkedId == R.id.rb_live_room_quality_music) {
                    liveRoom.setAudioQuality(TRTCCloudDef.TRTC_AUDIO_QUALITY_MUSIC);
                } else if (checkedId == R.id.rb_live_room_quality_normal) {
                    liveRoom.setAudioQuality(TRTCCloudDef.TRTC_AUDIO_QUALITY_DEFAULT);
                }
            }
        });
    }
}
