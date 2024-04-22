package com.trtc.uikit.livekit.common.core.store.state;

import com.trtc.uikit.livekit.common.core.store.state.view.ViewState;
import com.trtc.uikit.livekit.common.core.store.state.operation.OperationState;

public class LiveState {
    public OperationState operationState = new OperationState();
    public ViewState      viewState      = new ViewState();
}
