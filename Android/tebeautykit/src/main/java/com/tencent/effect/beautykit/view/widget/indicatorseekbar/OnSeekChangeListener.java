package com.tencent.effect.beautykit.view.widget.indicatorseekbar;


public interface OnSeekChangeListener {

    /**
     * Notification of progress change.
     * <p>
     * The client can use the fromUser parameter to distinguish changes initiated by the user
     * from those that occur programmatically. If the search bar type is discrete series,
     * the client can use the thumbPosition parameter to check the position of the thumb on the scale.
     * The checkedText parameter can be used to obtain the checked text below the current thumb.
     *
     * @param seekParams Information about the search bar
     */
    void onSeeking(SeekParams seekParams);

    /**
     * Notification that the user has started touching the gesture. The client may want to use this
     * to disable the forward search bar.
     *
     * @param seekBar The SeekBar where the touch gesture started
     */
    void onStartTrackingTouch(IndicatorSeekBar seekBar);

    /**
     * Notification that the user has completed the touch gesture. The client may want to use this
     * to re-enable the forward search bar.
     *
     * @param seekBar The SeekBar where the touch gesture started
     */
    void onStopTrackingTouch(IndicatorSeekBar seekBar);
}