package airline.api

import kotlinx.datetime.Instant

/**
 * Classes representing various actor events in the application
 */
sealed interface AppEvent {
    val flightId: String
    val departureTime: Instant
}

sealed interface FlightEvent : AppEvent

data class ScheduleFlight(
    override val flightId: String,
    override val departureTime: Instant,
    val plane: Plane,
) : FlightEvent

data class DelayFlight(
    override val flightId: String,
    override val departureTime: Instant,
    val actualDepartureTime: Instant,
) : FlightEvent

data class CancelFlight(
    override val flightId: String,
    override val departureTime: Instant,
) : FlightEvent

data class SetCheckInNumber(
    override val flightId: String,
    override val departureTime: Instant,
    val checkInNumber: String,
) : FlightEvent

data class SetGateNumber(
    override val flightId: String,
    override val departureTime: Instant,
    val gateNumber: String,
) : FlightEvent

data class BuyTicket(
    override val flightId: String,
    override val departureTime: Instant,
    val seatNo: String,
    val passengerId: String,
    val passengerName: String,
    val passengerEmail: String,
) : AppEvent

data class SendEmail(
    val flightId: String,
    val departureTime: Instant,
    val emailBuilder: (String) -> String,
)
