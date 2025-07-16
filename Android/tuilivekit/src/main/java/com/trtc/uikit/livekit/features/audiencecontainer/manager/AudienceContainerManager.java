package com.trtc.uikit.livekit.features.audiencecontainer.manager;

import static com.trtc.uikit.livekit.common.utils.MutableLiveDataUtils.setValue;

import com.trtc.uikit.livekit.features.audiencecontainer.AudienceContainerViewDefine;
import com.trtc.uikit.livekit.features.audiencecontainer.manager.observer.AudienceContainerViewListenerList;
import com.trtc.uikit.livekit.features.audiencecontainer.state.AudienceContainerConfig;

public class AudienceContainerManager {
    private final AudienceContainerViewListenerList mViewListenerList;

    public AudienceContainerManager() {
        mViewListenerList = new AudienceContainerViewListenerList();
    }

    public void addListener(AudienceContainerViewDefine.AudienceContainerViewListener listener) {
        mViewListenerList.addListener(listener);
    }

    public void removeListener(AudienceContainerViewDefine.AudienceContainerViewListener listener) {
        mViewListenerList.removeListener(listener);
    }

    public AudienceContainerViewListenerList getAudienceContainerViewListenerList() {
        return mViewListenerList;
    }

    public static void disableSliding(boolean disable) {
        setValue(AudienceContainerConfig.disableSliding, disable);
    }

    public static void disableHeaderFloatWin(boolean disable) {
        setValue(AudienceContainerConfig.disableHeaderFloatWin, disable);
    }

    public static void disableHeaderLiveData(boolean disable) {
        AudienceContainerConfig.disableHeaderLiveData.setValue(disable);
    }

    public static void disableHeaderVisitorCnt(boolean disable) {
        AudienceContainerConfig.disableHeaderVisitorCnt.setValue(disable);
    }

    public static void disableFooterCoGuest(boolean disable) {
        AudienceContainerConfig.disableFooterCoGuest.setValue(disable);
    }
}
