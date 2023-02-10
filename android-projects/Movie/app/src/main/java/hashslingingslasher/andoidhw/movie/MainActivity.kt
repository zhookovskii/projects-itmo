package hashslingingslasher.andoidhw.movie

import android.animation.AnimatorSet
import android.animation.ObjectAnimator
import android.animation.ValueAnimator
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.widget.ImageView
import androidx.core.animation.doOnEnd

class MainActivity : AppCompatActivity() {

    private lateinit var heartImage: ImageView

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        supportActionBar?.hide()
        setContentView(R.layout.activity_main)

        heartImage = findViewById(R.id.heart)

        val heartStretchHorizontalAnim = ObjectAnimator.ofFloat(
            heartImage,
            "scaleX",
            1.5f
        ).apply{
            duration = 500
            repeatCount = 1
            repeatMode = ValueAnimator.REVERSE
        }

        val heartStretchVerticalAnim = ObjectAnimator.ofFloat(
            heartImage,
            "scaleY",
            1.5f
        ).apply {
            duration = 500
            repeatCount = 1
            repeatMode = ValueAnimator.REVERSE
        }

        val someAnim = AnimatorSet().apply {
            play(heartStretchHorizontalAnim).with(heartStretchVerticalAnim)
            doOnEnd { start() }
        }

        someAnim.apply {
            startDelay = 1000
            start()
        }
    }
}