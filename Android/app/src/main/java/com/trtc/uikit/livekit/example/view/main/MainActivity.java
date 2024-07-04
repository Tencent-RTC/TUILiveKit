package com.trtc.uikit.livekit.example.view.main;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.tencent.qcloud.tuicore.TUILogin;
import com.tencent.qcloud.tuicore.interfaces.TUICallback;
import com.trtc.tuikit.common.FullScreenActivity;
import com.trtc.tuikit.common.ui.ConfirmWithCheckboxDialog;
import com.trtc.tuikit.common.util.ActivityLauncher;
import com.trtc.uikit.livekit.example.R;
import com.trtc.uikit.livekit.example.store.AppStore;
import com.trtc.uikit.livekit.example.view.login.LoginActivity;
import com.trtc.uikit.livekit.view.TUILiveListFragment;

public class MainActivity extends FullScreenActivity implements View.OnClickListener {
    private static final String      TAG                   = "MainActivity";
    private static final int         FRAGMENT_INDEX_HALL   = 0;
    private static final int         FRAGMENT_INDEX_ME     = 1;
    private              ImageView   mImageLive;
    private              TextView    mTextLive;
    private              ImageView   mImageMe;
    private              TextView    mTextMe;
    private              TextView    mTextTitle;
    private              ImageButton mButtonMultiFunction;
    private              int         mCurrentFragmentIndex = FRAGMENT_INDEX_HALL;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.app_activity_main);
        bindViewId();

        initLiveListFragment();
        initStartLiveView();
        initUserInfoView();
        initMultiFunctionButton();
    }

    private void bindViewId() {
        mImageLive = findViewById(R.id.iv_live);
        mTextLive = findViewById(R.id.tv_live);
        mImageMe = findViewById(R.id.iv_me);
        mTextMe = findViewById(R.id.tv_me);
        mTextTitle = findViewById(R.id.tv_title);
        mButtonMultiFunction = findViewById(R.id.btn_multi_function);
    }

    private void initUserInfoView() {
        LinearLayout mLayoutMe = findViewById(R.id.ll_me);
        mLayoutMe.setOnClickListener(this);
    }

    private void initStartLiveView() {
        findViewById(R.id.iv_start).setOnClickListener(view -> {
            Intent intent = new Intent(MainActivity.this, StartLiveActivity.class);
            startActivity(intent);
        });
    }

    private void initLiveListFragment() {
        LinearLayout mLayoutLiveHall = findViewById(R.id.ll_live_hall);
        mLayoutLiveHall.setOnClickListener(this);
        replaceFragment(new TUILiveListFragment());
    }

    private void replaceFragment(Fragment fragment) {
        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction transaction = fragmentManager.beginTransaction();
        transaction.replace(R.id.fl_live_list, fragment);
        transaction.commit();
    }

    private void initMultiFunctionButton() {
        mButtonMultiFunction.setOnClickListener(v -> {
            if (mCurrentFragmentIndex == FRAGMENT_INDEX_HALL) {
                Intent intent = new Intent(Intent.ACTION_VIEW);
                intent.setData(Uri.parse(AppStore.TRTC_LIVE_ROOM_DOCUMENT_URL));
                ActivityLauncher.startActivity(MainActivity.this, intent);
            } else if (mCurrentFragmentIndex == FRAGMENT_INDEX_ME) {
                showLogoutDialog();
            }
        });
    }

    private void showLogoutDialog() {
        ConfirmWithCheckboxDialog dialog = new ConfirmWithCheckboxDialog(MainActivity.this);
        dialog.setTitle(getString(R.string.app_tips_title));
        dialog.setContent(getString(R.string.app_logout_tips));
        dialog.setNegativeText(getString(R.string.app_cancel), negativeView -> dialog.dismiss());
        dialog.setPositiveText(getString(R.string.app_logout), positiveView -> {
            TUILogin.logout(new TUICallback() {
                @Override
                public void onSuccess() {
                    Intent intent = new Intent(MainActivity.this, LoginActivity.class);
                    startActivity(intent);
                    finish();
                }

                @Override
                public void onError(int errorCode, String errorMessage) {
                    Log.e(TAG, "logout fail，error code: " + errorCode + " message:" + errorMessage);
                }
            });
        });
        dialog.show();
    }

    private void clickLiveBottomBarChange() {
        mTextLive.setTextColor(ContextCompat.getColor(
                this,
                com.trtc.uikit.livekit.R.color.livekit_design_standard_flowkit_blue));
        mImageLive.setImageDrawable(ContextCompat.getDrawable(this, R.drawable.app_live_hall_blue));
        mTextMe.setTextColor(ContextCompat.getColor(this, R.color.app_color_black));
        mImageMe.setImageDrawable(ContextCompat.getDrawable(this, R.drawable.app_me_black));
    }

    private void clickLiveTopBarChange() {
        mButtonMultiFunction.setBackgroundResource(R.drawable.app_question_link);
        mCurrentFragmentIndex = FRAGMENT_INDEX_HALL;
        mTextTitle.setText(com.trtc.uikit.livekit.R.string.livekit_live_room_list);
    }

    private void clickMeBottomBarChange() {
        mTextLive.setTextColor(ContextCompat.getColor(this, R.color.app_color_black));
        mImageLive.setImageDrawable(ContextCompat.getDrawable(this, R.drawable.app_live_hall_black));
        mTextMe.setTextColor(ContextCompat.getColor(
                this,
                com.trtc.uikit.livekit.R.color.livekit_design_standard_flowkit_blue));
        mImageMe.setImageDrawable(ContextCompat.getDrawable(this, R.drawable.app_me_blue));
    }

    private void clickMeTopBarChange() {
        mButtonMultiFunction.setBackgroundResource(R.drawable.app_logout);
        mCurrentFragmentIndex = FRAGMENT_INDEX_ME;
        mTextTitle.setText(R.string.app_title_me);
    }

    @Override
    public void onClick(View v) {
        if (v.getId() == R.id.ll_live_hall) {
            clickLiveBottomBarChange();
            clickLiveTopBarChange();
            replaceFragment(new TUILiveListFragment());
        } else if (v.getId() == R.id.ll_me) {
            clickMeBottomBarChange();
            clickMeTopBarChange();
            replaceFragment(new MeFragment());
        }
    }
}