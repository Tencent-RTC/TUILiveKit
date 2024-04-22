package com.trtc.uikit.livekit.example.main;


import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.widget.EditText;
import android.widget.TextView;

import com.trtc.uikit.livekit.common.core.LiveDefine;
import com.trtc.uikit.livekit.example.BaseActivity;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.settings.SettingsConfig;

public class MainActivity extends BaseActivity {
    private EditText mEditStreamId;
    private TextView mTextJoinStream;
    private TextView mTextJoinVoiceRoom;

    private final TextWatcher mEditTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            if (!TextUtils.isEmpty(mEditStreamId.getText().toString())) {
                mTextJoinStream.setEnabled(true);
                mTextJoinVoiceRoom.setEnabled(true);
            } else {
                mTextJoinStream.setEnabled(false);
                mTextJoinVoiceRoom.setEnabled(false);
            }
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_main);
        initView();
    }

    private void initView() {
        mEditStreamId = findViewById(R.id.et_stream_id);
        mTextJoinStream = findViewById(R.id.tv_join_stream);
        mTextJoinVoiceRoom = findViewById(R.id.tv_join_voice);
        mEditStreamId.addTextChangedListener(mEditTextWatcher);

        findViewById(R.id.tv_join_stream).setOnClickListener(v -> {
            final String roomId = mEditStreamId.getText().toString().trim();
            Intent intent = new Intent(MainActivity.this, AudienceActivity.class);
            intent.putExtra("roomId", roomId);
            startActivity(intent);
        });

        findViewById(R.id.tv_join_voice).setOnClickListener(v -> {
            final String roomId = mEditStreamId.getText().toString().trim();
            Bundle bundle = new Bundle();
            bundle.putString("roomId", roomId);
            bundle.putSerializable("roomBehavior", LiveDefine.RoomBehavior.JOIN);
            Intent intent = new Intent(MainActivity.this, LiveActivity.class);
            intent.putExtras(bundle);
            startActivity(intent);
        });

        findViewById(R.id.btn_start_stream).setOnClickListener(v -> {
            Intent intent = new Intent(MainActivity.this, AnchorActivity.class);
            startActivity(intent);
        });

        findViewById(R.id.btn_trtclivekit_link).setOnClickListener(v -> {
            Intent intent = new Intent(Intent.ACTION_VIEW);
            intent.setData(Uri.parse(SettingsConfig.TRTC_LIVE_ROOM_DOCUMENT_URL));
            startActivity(intent);
        });
    }

}