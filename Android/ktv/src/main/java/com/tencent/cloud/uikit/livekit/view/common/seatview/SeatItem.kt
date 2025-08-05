import androidx.compose.animation.core.*
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.Image
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.lerp
import coil.compose.AsyncImage
import com.tencent.cloud.uikit.ktv.R

private const val VOLUME_THRESHOLD = 25

@Composable
fun SeatItem(
    modifier: Modifier = Modifier,
    userName: String? = null,
    avatarUrl: String? = null,
    volume: Int = 0,
    seatImage: Int = R.drawable.ktv_seat_placeholder,
    waveColor: Color = Color.White,
    waveDuration: Int = 900,
    onClick: (() -> Unit)? = null
) {
    val dimensions = remember { SeatDimensions() }

    Column(
        modifier = modifier.width(dimensions.containerWidth),
        horizontalAlignment = Alignment.CenterHorizontally
    ) {
        InteractiveAvatarContainer(
            dimensions = dimensions,
            avatarUrl = avatarUrl,
            seatImage = seatImage,
            volume = volume,
            waveColor = waveColor,
            waveDuration = waveDuration,
            onClick = onClick
        )

        userName?.let {
            UserNameText(name = it, width = dimensions.avatarSize)
        }
    }
}

private class SeatDimensions(
    val avatarSize: Dp = 48.dp,
    val minWave: Dp = 2.dp,
    val maxWave: Dp = 5.dp,
    val minStroke: Dp = 1.dp,
    val maxStroke: Dp = 4.dp
) {
    val waveStart = avatarSize / 2 + minWave
    val waveEnd = avatarSize / 2 + maxWave
    val containerWidth = (avatarSize / 2 + maxWave) * 2
}

@Composable
private fun InteractiveAvatarContainer(
    dimensions: SeatDimensions,
    avatarUrl: String?,
    seatImage: Int,
    volume: Int,
    waveColor: Color,
    waveDuration: Int,
    onClick: (() -> Unit)?
) {
    val waveProgress by animateWaveProgress(
        shouldAnimate = volume > VOLUME_THRESHOLD,
        duration = waveDuration
    )

    Box(
        modifier = Modifier
            .size(dimensions.containerWidth)
            .then(if (onClick != null) Modifier.clickable { onClick() } else Modifier),
        contentAlignment = Alignment.Center
    ) {
        if (volume > VOLUME_THRESHOLD) {
            WaveEffect(
                progress = waveProgress,
                dimensions = dimensions,
                waveColor = waveColor
            )
        }

        AvatarImage(
            avatarUrl = avatarUrl,
            placeholder = seatImage,
            size = dimensions.avatarSize
        )
    }
}

@Composable
private fun animateWaveProgress(shouldAnimate: Boolean, duration: Int): State<Float> {
    return if (shouldAnimate) {
        val infiniteTransition = rememberInfiniteTransition(label = "wave")
        infiniteTransition.animateFloat(
            initialValue = 0f,
            targetValue = 1f,
            animationSpec = infiniteRepeatable(
                animation = tween(duration, easing = LinearEasing),
                repeatMode = RepeatMode.Restart
            ),
            label = "wave"
        )
    } else {
        remember { mutableStateOf(0f) }
    }
}

@Composable
private fun WaveEffect(
    progress: Float,
    dimensions: SeatDimensions,
    waveColor: Color
) {
    Canvas(modifier = Modifier.fillMaxSize()) {
        val center = Offset(size.width / 2, size.height / 2)
        val radius = lerp(dimensions.waveStart, dimensions.waveEnd, progress).toPx()
        val strokeWidth = lerp(
            dimensions.minStroke,
            dimensions.maxStroke,
            if (progress < 0.5f) progress * 2 else (1 - progress) * 2
        ).toPx()

        drawCircle(
            color = waveColor.copy(alpha = 1 - progress),
            radius = radius,
            center = center,
            style = Stroke(strokeWidth)
        )
    }
}

@Composable
private fun AvatarImage(
    avatarUrl: String?,
    placeholder: Int,
    size: Dp
) {
    if (!avatarUrl.isNullOrEmpty()) {
        AsyncImage(
            model = avatarUrl,
            contentDescription = "User Avatar",
            modifier = Modifier
                .size(size)
                .clip(CircleShape),
            placeholder = painterResource(placeholder),
            error = painterResource(placeholder)
        )
    } else {
        Image(
            painter = painterResource(placeholder),
            contentDescription = "Seat Placeholder",
            modifier = Modifier.size(size)
        )
    }
}

@Composable
private fun UserNameText(name: String, width: Dp) {
    Text(
        text = name,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        color = Color.White,
        style = MaterialTheme.typography.bodySmall,
        textAlign = TextAlign.Center,
        modifier = Modifier
            .padding(top = 3.dp)
            .width(width)
    )
}