package com.trtc.uikit.livekit.component.liveListviewpager;

import static android.view.ViewGroup.LayoutParams.MATCH_PARENT;
import static androidx.viewpager2.widget.ViewPager2.SCROLL_STATE_DRAGGING;
import static androidx.viewpager2.widget.ViewPager2.SCROLL_STATE_IDLE;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.FrameLayout;
import android.widget.RelativeLayout;

import androidx.viewpager2.widget.ViewPager2;

public class LiveListViewPager extends FrameLayout {
    private final Context             mContext;
    private       ViewPager2          mViewPager;
    private       LiveListViewAdapter mLiveListViewAdapter;

    private int   mCurrentPosition      = -1;
    private float mPositionOffset       = -1;
    private int   mWillSlideInPosition  = -1;
    private int   mWillSlideOutPosition = -1;

    public LiveListViewPager(Context context) {
        this(context, null);
    }

    public LiveListViewPager(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public LiveListViewPager(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mContext = context;
        init();
    }

    public void setAdapter(LiveListViewAdapter adapter) {
        mLiveListViewAdapter = adapter;
        mViewPager.setAdapter(adapter);
    }

    private int getCurrentItem() {
        return mViewPager.getCurrentItem();
    }

    public void enableSliding(boolean enabled) {
        mViewPager.setUserInputEnabled(enabled);
    }

    private void init() {
        mViewPager = new ViewPager2(mContext);
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT);
        addView(mViewPager, layoutParams);
        mViewPager.setOffscreenPageLimit(1);
        mViewPager.setOrientation(ViewPager2.ORIENTATION_VERTICAL);
        mViewPager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageScrollStateChanged(int state) {
                switch (state) {
                    case SCROLL_STATE_DRAGGING:
                        mWillSlideInPosition = -1;
                        mWillSlideOutPosition = -1;
                        mPositionOffset = -1;
                        break;
                    case SCROLL_STATE_IDLE:
                        onScrollEnd();
                        break;
                    default:
                        break;
                }
            }

            @Override
            public void onPageSelected(int position) {
                if (mCurrentPosition == -1) {
                    onFragmentDidSlideIn(position);
                    mCurrentPosition = position;
                    return;
                }
                if (isSliding()) {
                    onFragmentDidSlideOut(mCurrentPosition);
                    onFragmentDidSlideIn(position);
                    mCurrentPosition = position;
                }
            }

            @Override
            public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
                if (positionOffset <= 0) {
                    return;
                }
                if (isSliding()) {
                    return;
                }
                if (isSlideToPrevious(position, positionOffset)) {
                    onSlideToPrevious();
                } else {
                    onSlideToNext();
                }
                mPositionOffset = positionOffset;
            }
        });
    }

    private boolean isSlideToPrevious(int position, float positionOffset) {
        if (position < mCurrentPosition) {
            if (mPositionOffset == -1) {
                return true;
            }
            return positionOffset < mPositionOffset;
        }
        return false;
    }

    private boolean isSliding() {
        return mWillSlideInPosition != -1 || mWillSlideOutPosition != -1;
    }

    private void onSlideToNext() {
        if (mCurrentPosition >= mLiveListViewAdapter.getDataList().size() - 1) {
            return;
        }
        mWillSlideOutPosition = mCurrentPosition;
        mWillSlideInPosition = mCurrentPosition + 1;
        onFragmentWillSlideOut(mWillSlideOutPosition);
        onFragmentWillSlideIn(mWillSlideInPosition);
    }

    private void onSlideToPrevious() {
        if (mCurrentPosition <= 0) {
            return;
        }
        mWillSlideOutPosition = mCurrentPosition;
        mWillSlideInPosition = mCurrentPosition - 1;
        onFragmentWillSlideOut(mWillSlideOutPosition);
        onFragmentWillSlideIn(mWillSlideInPosition);
    }

    private void onScrollEnd() {
        if (!isSliding()) {
            return;
        }
        if (mWillSlideInPosition < 0 || mWillSlideInPosition >= mLiveListViewAdapter.getDataList().size()) {
            return;
        }
        if (mWillSlideOutPosition < 0 || mWillSlideOutPosition >= mLiveListViewAdapter.getDataList().size()) {
            return;
        }
        if (mWillSlideInPosition != getCurrentItem()) {
            onFragmentSlideInCancelled(mWillSlideInPosition);
            onFragmentSlideOutCancelled(mWillSlideOutPosition);
        }
        mWillSlideInPosition = -1;
        mWillSlideOutPosition = -1;
    }

    private void onFragmentWillSlideIn(int position) {
        LiveListFragment fragment = (LiveListFragment) mLiveListViewAdapter.getFragment(position);
        if (fragment == null) {
            return;
        }
        fragment.onFragmentWillSlideIn();
    }

    private void onFragmentDidSlideIn(int position) {
        LiveListFragment fragment = (LiveListFragment) mLiveListViewAdapter.getFragment(position);
        if (fragment == null) {
            return;
        }
        fragment.onFragmentDidSlideIn();
        if (position >= mLiveListViewAdapter.getDataList().size() - 2) {
            mLiveListViewAdapter.fetchData();
        }
    }

    private void onFragmentSlideInCancelled(int position) {
        LiveListFragment fragment = (LiveListFragment) mLiveListViewAdapter.getFragment(position);
        if (fragment == null) {
            return;
        }
        fragment.onFragmentSlideInCancelled();
    }

    private void onFragmentWillSlideOut(int position) {
        LiveListFragment fragment = (LiveListFragment) mLiveListViewAdapter.getFragment(position);
        if (fragment == null) {
            return;
        }
        fragment.onFragmentWillSlideOut();
    }

    private void onFragmentDidSlideOut(int position) {
        LiveListFragment fragment = (LiveListFragment) mLiveListViewAdapter.getFragment(position);
        if (fragment == null) {
            return;
        }
        fragment.onFragmentDidSlideOut();
    }

    private void onFragmentSlideOutCancelled(int position) {
        LiveListFragment fragment = (LiveListFragment) mLiveListViewAdapter.getFragment(position);
        if (fragment == null) {
            return;
        }
        fragment.onFragmentSlideOutCancelled();
    }
}
