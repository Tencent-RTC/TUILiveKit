package com.trtc.uikit.livekit.example.main;


import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.widget.EditText;
import android.widget.TextView;

import com.trtc.tuikit.common.util.ActivityLauncher;
import com.trtc.uikit.livekit.example.BaseActivity;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.settings.SettingsConfig;

public class MainActivity extends BaseActivity {
    private EditText mEditStreamId;
    private TextView mTextJoinStream;
    private TextView mTextEnterRoomList;

    private final TextWatcher mEditTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            if (!TextUtils.isEmpty(mEditStreamId.getText().toString())) {
                mTextJoinStream.setEnabled(true);
            } else {
                mTextJoinStream.setEnabled(false);
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
        mTextEnterRoomList = findViewById(R.id.tv_room_list);
        mEditStreamId.addTextChangedListener(mEditTextWatcher);

        findViewById(R.id.tv_join_stream).setOnClickListener(v -> {
            final String roomId = mEditStreamId.getText().toString().trim();
            Intent intent = new Intent(MainActivity.this, AudienceActivity.class);
            intent.putExtra("roomId", roomId);
            startActivity(intent);
        });

        mTextEnterRoomList.setOnClickListener(v -> {
            Intent intent = new Intent(MainActivity.this, RoomListActivity.class);
            startActivity(intent);
        });

        findViewById(R.id.btn_start_stream).setOnClickListener(v -> {
            Intent intent = new Intent(MainActivity.this, AnchorActivity.class);
            startActivity(intent);
        });

        findViewById(R.id.btn_trtclivekit_link).setOnClickListener(v -> {
            Intent intent = new Intent(Intent.ACTION_VIEW);
            intent.setData(Uri.parse(SettingsConfig.TRTC_LIVE_ROOM_DOCUMENT_URL));
            ActivityLauncher.startActivity(MainActivity.this, intent);
        });
    }
}