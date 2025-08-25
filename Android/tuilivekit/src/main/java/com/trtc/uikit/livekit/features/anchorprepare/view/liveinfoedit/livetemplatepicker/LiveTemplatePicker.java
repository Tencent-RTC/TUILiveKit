package com.trtc.uikit.livekit.features.anchorprepare.view.liveinfoedit.livetemplatepicker;

import android.content.Context;
import android.view.View;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.trtc.tuikit.common.ui.PopupDialog;
import com.trtc.uikit.livekit.R;
import com.trtc.uikit.livekit.features.anchorprepare.manager.AnchorPrepareManager;

import java.util.Arrays;

public class LiveTemplatePicker extends PopupDialog {

    private final AnchorPrepareManager mManager;

    public LiveTemplatePicker(Context context, AnchorPrepareManager manager) {
        super(context);
        mManager = manager;
        initView();
    }

    private void initView() {
        View view = View.inflate(getContext(), R.layout.anchor_prepare_layout_select_template, null);
        initCoGuestTemplateView(view);
        initCoHostTemplateView(view);
        setView(view);
    }

    private void initCoHostTemplateView(View view) {
        RecyclerView mRecyclerCoHost = view.findViewById(R.id.rv_template_co_host);
        mRecyclerCoHost.setLayoutManager(new GridLayoutManager(getContext(), 2));

        mRecyclerCoHost.setBackgroundResource(R.drawable.anchor_prepare_dialog_background);
        CoHostTemplatePickAdapter adapter = new CoHostTemplatePickAdapter(getContext(), mManager,
                Arrays.asList(TemplateType.VERTICAL_DYNAMIC_GRID_CO_HOST, TemplateType.VERTICAL_DYNAMIC_FLOAT_CO_HOST),
                type -> {
                    mManager.setCoHostTemplate(type.id);
                    dismiss();
                });
        mRecyclerCoHost.setAdapter(adapter);
    }

    private void initCoGuestTemplateView(View view) {
        RecyclerView mRecyclerCoGuest = view.findViewById(R.id.rv_template);
        mRecyclerCoGuest.setLayoutManager(new GridLayoutManager(getContext(), 2));

        mRecyclerCoGuest.setBackgroundResource(R.drawable.anchor_prepare_dialog_background);
        CoGuestTemplatePickAdapter adapter = new CoGuestTemplatePickAdapter(getContext(), mManager,
                Arrays.asList(TemplateType.VERTICAL_DYNAMIC_GRID, TemplateType.VERTICAL_DYNAMIC_FLOAT,
                        TemplateType.VERTICAL_STATIC_GRID, TemplateType.VERTICAL_STATIC_FLOAT), type -> {
            mManager.setCoGuestTemplate(type.id);
            dismiss();
        });
        mRecyclerCoGuest.setAdapter(adapter);
    }

    public enum TemplateType {
        VERTICAL_DYNAMIC_GRID(600, R.string.common_template_dynamic_grid, R.drawable.anchor_prepare_dynamic_grid),
        VERTICAL_DYNAMIC_FLOAT(601, R.string.common_template_dynamic_float, R.drawable.anchor_prepare_dynamic_float),
        VERTICAL_STATIC_GRID(800, R.string.common_template_static_grid, R.drawable.anchor_prepare_static_grid),
        VERTICAL_STATIC_FLOAT(801, R.string.common_template_static_float, R.drawable.anchor_prepare_static_float),
        VERTICAL_DYNAMIC_GRID_CO_HOST(600, R.string.common_template_dynamic_grid,
                R.drawable.anchor_prepare_dynamic_grid_co_host),
        VERTICAL_DYNAMIC_FLOAT_CO_HOST(601, R.string.common_template_dynamic_float,
                R.drawable.anchor_prepare_dynamic_float_co_host);

        final int id;
        final int name;
        final int icon;

        TemplateType(int id, int name, int icon) {
            this.id = id;
            this.name = name;
            this.icon = icon;
        }

        public static String getNameById(Context context, Integer integer) {
            if (integer == null) {
                return context.getResources().getString(R.string.common_template_dynamic_grid);
            }
            for (TemplateType type : TemplateType.values()) {
                if (type.id == integer) {
                    return context.getResources().getString(type.name);
                }
            }
            return context.getResources().getString(R.string.common_template_dynamic_grid);
        }
    }
}
