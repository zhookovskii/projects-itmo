package airline

import airline.api.*
import airline.service.AirlineManagementService
import airline.service.BookingService
import airline.service.EmailService
import airline.service.PassengerNotificationService
import airline.utils.*
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.flow.*
import kotlinx.datetime.Instant

class AirlineApplication(
    private val config: AirlineConfig,
    private val emailService: EmailService,
) {

    private fun before(time: Instant): Boolean =
        beforeInstant(time, config.audioAlertsDelta)

    private fun after(time: Instant): Boolean =
        afterInstant(time, config.audioAlertsDelta)

    val bookingService: BookingService = object : BookingService {
        override val flightSchedule: List<FlightInfo>
            get() = appStateFlow.value
                .filter {
                    now() < it.actualDepartureTime - config.ticketSaleEndTime &&
                        !it.isCancelled
                }
                .map { it.asFlightInfo() }

        override fun freeSeats(flightId: String, departureTime: Instant): Set<String> {
            val flight = appStateFlow.value.find {
                it.flightId == flightId && it.departureTime == departureTime
            } ?: return emptySet()
            return flight.plane.seats.minus(flight.tickets.keys)
        }

        override suspend fun buyTicket(
            flightId: String,
            departureTime: Instant,
            seatNo: String,
            passengerId: String,
            passengerName: String,
            passengerEmail: String,
        ) {
            eventsFlow.emit(
                BuyTicket(flightId, departureTime, seatNo, passengerId, passengerName, passengerEmail),
            )
        }
    }

    val managementService: AirlineManagementService = object : AirlineManagementService {

        override suspend fun scheduleFlight(flightId: String, departureTime: Instant, plane: Plane) {
            eventsFlow.emit(
                ScheduleFlight(flightId, departureTime, plane),
            )
        }

        override suspend fun delayFlight(flightId: String, departureTime: Instant, actualDepartureTime: Instant) {
            eventsFlow.emit(
                DelayFlight(flightId, departureTime, actualDepartureTime),
            )
        }

        override suspend fun cancelFlight(flightId: String, departureTime: Instant) {
            eventsFlow.emit(
                CancelFlight(flightId, departureTime),
            )
        }

        override suspend fun setCheckInNumber(flightId: String, departureTime: Instant, checkInNumber: String) {
            eventsFlow.emit(
                SetCheckInNumber(flightId, departureTime, checkInNumber),
            )
        }

        override suspend fun setGateNumber(flightId: String, departureTime: Instant, gateNumber: String) {
            eventsFlow.emit(
                SetGateNumber(flightId, departureTime, gateNumber),
            )
        }
    }

    @OptIn(FlowPreview::class)
    suspend fun airportInformationDisplay(coroutineScope: CoroutineScope): StateFlow<InformationDisplay> =
        appStateFlow
            .map { flights -> InformationDisplay(flights.map { it.asFlightInfo() }) }
            .sample(config.displayUpdateInterval)
            .stateIn(coroutineScope)

    val airportAudioAlerts: Flow<AudioAlerts> = flow {
        while (true) {
            appStateFlow.value.filter { it.gateNumber != null }.forEach {
                if (before(it.actualDepartureTime - config.boardingClosingTime)) {
                    emit(AudioAlerts.BoardingClosing(it.flightId, it.gateNumber!!))
                }
                if (after(it.actualDepartureTime - config.boardingOpeningTime)) {
                    emit(AudioAlerts.BoardingOpened(it.flightId, it.gateNumber!!))
                }
            }
            appStateFlow.value.filter { it.checkInNumber != null }.forEach {
                if (before(it.actualDepartureTime - config.registrationClosingTime)) {
                    emit(AudioAlerts.RegistrationClosing(it.flightId, it.checkInNumber!!))
                }
                if (after(it.actualDepartureTime - config.registrationOpeningTime)) {
                    emit(AudioAlerts.RegistrationOpen(it.flightId, it.checkInNumber!!))
                }
            }
            delay(config.audioAlertsInterval)
        }
    }

    private val eventsFlow = MutableSharedFlow<AppEvent>()

    private val appStateFlow = MutableStateFlow<List<Flight>>(emptyList())

    private val bufferedEmailService = object : EmailService {
        val pending = Channel<Pair<String, String>>(capacity = 10)

        override suspend fun send(to: String, text: String) {
            pending.send(to to text)
        }
    }

    private val passengerNotificationService = object : PassengerNotificationService {

        override val emailFlow: MutableSharedFlow<SendEmail> = MutableSharedFlow()

        override suspend fun send(
            flightId: String,
            departureTime: Instant,
            emailBuilder: (String) -> String,
        ) = emailFlow.emit(
            SendEmail(flightId, departureTime, emailBuilder),
        )

        override suspend fun run() {
            emailFlow.collect { email ->
                val passengerEmails = appStateFlow.value.find {
                    it.flightId == email.flightId && it.departureTime == email.departureTime
                }?.tickets?.values ?: emptySet()

                passengerEmails.forEach { ticket ->
                    bufferedEmailService.send(
                        ticket.passengerEmail,
                        email.emailBuilder(ticket.passengerName),
                    )
                }
            }
        }
    }

    private fun matchFlight(flight: Flight, event: AppEvent): Boolean =
        flight.flightId == event.flightId && flight.departureTime == event.departureTime

    fun Flight.asFlightInfo(): FlightInfo =
        FlightInfo(
            flightId,
            departureTime,
            isCancelled,
            actualDepartureTime,
            checkInNumber,
            gateNumber,
            plane,
        )

    private suspend fun handleFlightEvent(event: FlightEvent) {
        when (event) {
            is ScheduleFlight -> {
                appStateFlow.value.find {
                    matchFlight(it, event)
                }
                    ?: appStateFlow.emit(
                        appStateFlow.value +
                            Flight(event.flightId, event.departureTime, plane = event.plane),
                    )
            }
            is DelayFlight -> {
                appStateFlow.emitCopy(event) {
                    it.copy(actualDepartureTime = event.actualDepartureTime)
                }
                sendEmail(event) { passengerName ->
                    "Dear $passengerName, unfortunately, your flight ${event.flightId} " +
                        "has been delayed from ${event.departureTime} to ${event.actualDepartureTime}"
                }
            }
            is CancelFlight -> {
                appStateFlow.emitCopy(event) {
                    it.copy(isCancelled = true)
                }
                sendEmail(event) { passengerName ->
                    "Dear $passengerName, unfortunately, your flight ${event.flightId} " +
                        "departing ${event.departureTime} has been cancelled"
                }
            }
            is SetCheckInNumber -> {
                appStateFlow.emitCopy(event) {
                    it.copy(checkInNumber = event.checkInNumber)
                }
                sendEmail(event) { passengerName ->
                    "Dear $passengerName, registration for your flight ${event.flightId} " +
                        "departing ${event.departureTime} has opened at check-in ${event.checkInNumber}"
                }
            }
            is SetGateNumber -> {
                appStateFlow.emitCopy(event) {
                    it.copy(gateNumber = event.gateNumber)
                }
                sendEmail(event) { passengerName ->
                    "Dear $passengerName, boarding for your flight ${event.flightId} " +
                        "departing ${event.departureTime} has started at gate ${event.gateNumber}"
                }
            }
        }
    }

    private suspend fun MutableStateFlow<List<Flight>>.emitCopy(event: AppEvent, copy: (Flight) -> Flight) =
        emit(
            value.map {
                if (matchFlight(it, event)) {
                    copy(it)
                } else {
                    it
                }
            },
        )

    private suspend fun sendEmail(event: FlightEvent, emailBuilder: (String) -> String) =
        passengerNotificationService.send(
            event.flightId,
            event.departureTime,
            emailBuilder,
        )

    sealed interface BookingResult
    data object Success : BookingResult
    data object NoSuchFlightError : BookingResult
    data object NoSuchSeatError : BookingResult
    data object SeatOccupiedError : BookingResult
    data object TicketSalesClosedError : BookingResult
    data object FlightCancelledError : BookingResult

    private suspend fun handleBookingEvent(event: BuyTicket) {
        var result: BookingResult = NoSuchFlightError
        appStateFlow.emitCopy(event) {
            if (it.isCancelled) {
                result = FlightCancelledError
            } else if (event.seatNo !in it.plane.seats) {
                result = NoSuchSeatError
            } else if (event.seatNo in it.tickets) {
                result = SeatOccupiedError
            } else if (now() >= (it.actualDepartureTime - config.ticketSaleEndTime)) {
                result = TicketSalesClosedError
            } else {
                result = Success
                it.tickets[event.seatNo] = Ticket(
                    event.flightId,
                    event.departureTime,
                    event.seatNo,
                    event.passengerId,
                    event.passengerName,
                    event.passengerEmail,
                )
            }
            it
        }
        val emailText = when (result) {
            is Success ->
                "Dear ${event.passengerName}, your ticket for flight " +
                    "${event.flightId} departing at ${event.departureTime} (seat ${event.seatNo}) " +
                    "was successfully purchased"

            is NoSuchFlightError ->
                "Dear ${event.passengerName}, unfortunately, " +
                    "your ticket (seat ${event.seatNo}) could not be purchased: " +
                    "flight ${event.flightId} departing at ${event.departureTime} " +
                    "can not be identified"

            is NoSuchSeatError ->
                "Dear ${event.passengerName}, unfortunately, " +
                    "your ticket (seat ${event.seatNo}) could not be purchased: " +
                    "seat ${event.seatNo} is not present at flight ${event.flightId}" +
                    "departing at ${event.departureTime}"

            is SeatOccupiedError ->
                "Dear ${event.passengerName}, unfortunately, " +
                    "your ticket could not be purchased: seat ${event.seatNo} on flight " +
                    "${event.flightId} departing at ${event.departureTime} is already " +
                    "occupied"

            is TicketSalesClosedError ->
                "Dear ${event.passengerName}, unfortunately, " +
                    "your ticket (seat ${event.seatNo}) could not be purchased: " +
                    "ticket sales for flight ${event.flightId} departing ${event.departureTime} " +
                    "are already closed"

            is FlightCancelledError ->
                "Dear ${event.passengerName}, unfortunately, " +
                    "your ticket (seat ${event.seatNo}) could not be purchased: " +
                    "flight ${event.flightId} departing at ${event.departureTime} was cancelled"
        }
        bufferedEmailService.send(
            to = event.passengerEmail,
            text = emailText,
        )
    }

    suspend fun run() {
        CoroutineScope(Dispatchers.Default).launch {
            launch { passengerNotificationService.run() }

            launch {
                eventsFlow.collect { event ->
                    when (event) {
                        is FlightEvent -> handleFlightEvent(event)
                        is BuyTicket -> handleBookingEvent(event)
                    }
                }
            }

            launch {
                while (true) {
                    val (to, text) = bufferedEmailService.pending.receive()
                    emailService.send(to, text)
                }
            }
        }
    }
}
