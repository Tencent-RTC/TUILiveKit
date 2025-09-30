# Add project specific ProGuard rules here.
# You can control the set of applied configuration files using the
# proguardFiles setting in build.gradle.
#
# For more details, see
#   http://developer.android.com/guide/developing/tools/proguard.html

# If your project uses WebView with JS, uncomment the following
# and specify the fully qualified class name to the JavaScript interface
# class:
#-keepclassmembers class fqcn.of.javascript.interface.for.webview {
#   public *;
#}

# Uncomment this to preserve the line number information for
# debugging stack traces.
#-keepattributes SourceFile,LineNumberTable

# If you keep the line number information, uncomment this to
# hide the original source file name.
#-renamesourcefileattribute SourceFile

-dontwarn javax.annotation.Nullable
-printmapping build/proguard/mapping.txt

-keep class com.tencent.** { *; }
-keep class com.trtc.uikit.livekit.livestreamcore.** { *; }
-keep class com.trtc.uikit.livekit.component.gift.store.model.** { *; }
-keep class com.squareup.wire.** { *; }
-keep class com.opensource.svgaplayer.proto.** { *; }

-keep class com.tcmediax.** { *; }
-keep class com.tencent.** { *; }
-keep class com.tencent.xmagic.** { *; }
-keep class androidx.exifinterface.** {*;}
-keep class com.google.gson.** { *;}
# Tencent Effect SDK - beauty
-keep class com.tencent.xmagic.** { *;}
-keep class org.light.** { *;}
-keep class org.libpag.** { *;}
-keep class org.extra.** { *;}
-keep class com.gyailib.**{ *;}
-keep class com.tencent.cloud.iai.lib.** { *;}
-keep class com.tencent.beacon.** { *;}
-keep class com.tencent.qimei.** { *;}
-keep class androidx.exifinterface.** { *;}

-keep class okhttp3.** { *; }
-keep interface okhttp3.** { *; }
-keep class okio.** { *; }
-keep interface okio.** { *; }
-dontwarn okhttp3.**
-dontwarn okio.**

-keep class io.trtc.tuikit.atomicx.karaoke.** { *; }

# TRTC SDK Viesion >= 12.5.0
-dontwarn com.tencent.rtmp.video.BaseBridge$BaseBridgeCallback
-dontwarn com.tencent.rtmp.video.BaseBridge
-dontwarn com.tencent.rtmp.video.VirtualDisplayListener
-dontwarn com.tencent.rtmp.video.VirtualDisplayManagerProxy
-dontwarn com.tencent.rtmp.video.VirtualDisplayWrapper
-dontwarn kotlinx.parcelize.Parcelize
