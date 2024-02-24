package airline.utils

import kotlin.time.Duration
import kotlinx.datetime.Clock
import kotlinx.datetime.Instant

fun now() = Clock.System.now()

fun afterInstant(time: Instant, delta: Duration): Boolean =
    time <= now() && now() <= time + delta

fun beforeInstant(time: Instant, delta: Duration): Boolean =
    time - delta <= now() && now() <= time
