package com.trtc.uikit.livekit.state;

import com.trtc.uikit.livekit.state.view.ViewState;
import com.trtc.uikit.livekit.state.operation.OperationState;

public class LiveState {
    public OperationState operationState = new OperationState();
    public ViewState      viewState      = new ViewState();

    public void reset() {
        viewState.reset();
        operationState.reset();
    }
}
