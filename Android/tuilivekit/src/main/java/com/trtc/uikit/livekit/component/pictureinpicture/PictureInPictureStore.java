package com.trtc.uikit.livekit.component.pictureinpicture;


import static com.trtc.uikit.livekit.common.MutableLiveDataUtils.setValue;

public class PictureInPictureStore {
    private volatile static PictureInPictureStore sInstance;

    private final PictureInPictureState mState;

    private PictureInPictureStore() {
        mState = new PictureInPictureState();
    }

    public static PictureInPictureStore sharedInstance() {
        if (sInstance == null) {
            synchronized (PictureInPictureStore.class) {
                if (sInstance == null) {
                    sInstance = new PictureInPictureStore();
                }
            }
        }
        return sInstance;
    }

    public void setPictureInPictureModeRoomId(String roomId) {
        setValue(mState.roomId, roomId);
    }

    public PictureInPictureState getState() {
        return mState;
    }

    public void reset() {
        mState.roomId.setValue("");
        mState.anchorIsPictureInPictureMode = false;
        mState.audienceIsPictureInPictureMode = false;
        mState.isAnchorStreaming = false;
    }
}
