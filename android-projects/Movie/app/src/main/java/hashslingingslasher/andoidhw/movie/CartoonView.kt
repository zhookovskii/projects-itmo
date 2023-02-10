package hashslingingslasher.andoidhw.movie

import android.content.Context
import android.content.res.TypedArray
import android.graphics.Canvas
import android.graphics.Color.*
import android.graphics.Paint
import android.os.Parcel
import android.os.Parcelable
import android.os.Parcelable.Creator
import android.util.AttributeSet
import android.view.View
import kotlin.math.cos
import kotlin.math.min
import kotlin.math.sin

class CartoonView(context : Context, attrs: AttributeSet) : View(context, attrs) {

    private var circleColor: Int
    private var dotColor: Int
    private var valueRadius: Int
    private var deltaPhi: Float

    private val paint = Paint(Paint.ANTI_ALIAS_FLAG)
    private var size: Int = 0
    private var currentPhi = 0F

    init {
        val a: TypedArray = context.obtainStyledAttributes(
            attrs, R.styleable.CartoonView, 0, 0)
        try {
            circleColor = a.getColor(R.styleable.CartoonView_valueCircleColor, BLACK)
            dotColor = a.getColor(R.styleable.CartoonView_valueDotColor, RED)
            valueRadius = a.getDimensionPixelSize(
                R.styleable.CartoonView_valueRadius, 100)
            deltaPhi = a.getFloat(R.styleable.CartoonView_valueDeltaPhi, 0.01F)
        } finally {
            a.recycle()
        }
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec)
        size = min(measuredWidth, measuredHeight)
        setMeasuredDimension(size, size)
    }

    override fun onDraw(canvas: Canvas?) {
        super.onDraw(canvas)

        paint.apply {
            color = circleColor
            style = Paint.Style.STROKE
        }

        val radius = valueRadius.toFloat()
        val dotRadius = radius / 8
        paint.strokeWidth = dotRadius / 2

        canvas?.drawCircle(
            (size / 2).toFloat(),
            (size / 2).toFloat(),
            radius,
            paint
        )

        canvas?.drawCircle(
            (size / 2).toFloat() + radius * cos(currentPhi),
            (size / 2).toFloat() + radius * sin(currentPhi),
            dotRadius,
            paint.apply {
                color = dotColor
                style = Paint.Style.FILL
            }
        )

        currentPhi = (currentPhi + deltaPhi) % 360F
        invalidate()
    }

    override fun onSaveInstanceState(): Parcelable {
        val savedState = DotSavedState(super.onSaveInstanceState())
        savedState.phi = currentPhi
        return savedState
    }

    override fun onRestoreInstanceState(state: Parcelable?) {
        super.onRestoreInstanceState(state)
        val savedState = state as DotSavedState
        currentPhi = savedState.phi
        invalidate()
    }

    override fun isSaveEnabled(): Boolean {
        return true
    }

    private class DotSavedState : BaseSavedState {
        var phi: Float = 0F

        constructor(superState: Parcelable?) : super(superState)
        private constructor(parcel: Parcel) : super(parcel) {
            phi = parcel.readFloat()
        }

        override fun writeToParcel(out: Parcel, flags: Int) {
            super.writeToParcel(out, flags)
            out.writeFloat(phi)
        }

        companion object CREATOR : Creator<DotSavedState> {
            override fun createFromParcel(parcel: Parcel): DotSavedState {
                return DotSavedState(parcel)
            }

            override fun newArray(size: Int): Array<DotSavedState?> {
                return arrayOfNulls(size)
            }
        }
    }

}