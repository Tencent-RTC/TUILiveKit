package com.trtc.uikit.livekit.common.utils;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

import java.io.IOException;

public class GsonNullStringAdapter extends TypeAdapter<String> {

    @Override
    public void write(JsonWriter out, String value) throws IOException {
        if (value == null) {
            out.value("");
        } else {
            out.value(value);
        }
    }

    @Override
    public String read(JsonReader in) throws IOException {
        if (in.peek() == null) {
            in.nextNull();
            return "";
        } else {
            return in.nextString();
        }
    }
}
