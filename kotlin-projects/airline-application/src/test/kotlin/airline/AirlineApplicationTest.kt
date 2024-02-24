package airline

import airline.api.*
import airline.service.EmailService
import airline.utils.*
import kotlin.time.Duration.Companion.hours
import kotlin.time.Duration.Companion.milliseconds
import kotlin.time.Duration.Companion.seconds
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.flow.take
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class AirlineApplicationTest {
    @Test
    fun simpleTest() {
        val emailService = InChannelEmailService()
        val config = AirlineConfig(
            displayUpdateInterval = 25.milliseconds,
        )
        val airlineApplication = AirlineApplication(config, emailService)
        val plane = Plane("A312", setOf("1A", "1B", "2A", "2B"))
        val flightId = "111"
        val flightTime = now() + 3.hours

        testAndCancel {
            launch { airlineApplication.run() }

            sleep()

            val booking = airlineApplication.bookingService
            val management = airlineApplication.managementService
            val display = airlineApplication.airportInformationDisplay(this)

            management.scheduleFlight(flightId, flightTime, plane)
            sleep()

            Assertions.assertEquals(1, display.value.departing.size)
            Assertions.assertEquals("111", display.value.departing[0].flightId)

            Assertions.assertEquals(1, booking.flightSchedule.size)
            Assertions.assertEquals("111", booking.flightSchedule[0].flightId)

            sleep()

            booking.buyTicket(
                "111",
                flightTime,
                "1A",
                "1",
                "Konstantin Bats",
                "kbats@itmo.ru",
            )
            withTimeout(100.milliseconds) {
                val (email, text) = emailService.messages.receive()
                Assertions.assertEquals("kbats@itmo.ru", email)
                Assertions.assertTrue("Konstantin Bats" in text)
                Assertions.assertTrue("success" in text)
                Assertions.assertTrue("111" in text)
                Assertions.assertTrue("1A" in text)
            }

            Assertions.assertEquals(1, display.value.departing.size)
            management.setCheckInNumber(flightId, flightTime, "checkin1")
            sleep()

            Assertions.assertEquals(1, display.value.departing.size)
            Assertions.assertEquals("111", display.value.departing[0].flightId)
            Assertions.assertEquals("checkin1", display.value.departing[0].checkInNumber)

            val (checkinEmail, checkinText) = emailService.messages.receive()
            Assertions.assertEquals("kbats@itmo.ru", checkinEmail)
            Assertions.assertTrue("check-in" in checkinText)

            management.delayFlight(flightId, flightTime, flightTime + 1.hours)
            sleep()

            val (email, text) = emailService.messages.receive()
            Assertions.assertEquals("kbats@itmo.ru", email)
            Assertions.assertTrue("delayed" in text)

            Assertions.assertEquals(1, display.value.departing.size)
            Assertions.assertEquals("111", display.value.departing[0].flightId)
            Assertions.assertEquals("checkin1", display.value.departing[0].checkInNumber)
            Assertions.assertEquals(flightTime, display.value.departing[0].departureTime)
            Assertions.assertEquals(flightTime + 1.hours, display.value.departing[0].actualDepartureTime)
        }
    }

    @Test
    fun informationDisplayTest() {
        val emailService = InChannelEmailService()
        val config = AirlineConfig(
            displayUpdateInterval = 100.milliseconds,
        )
        val airlineApplication = AirlineApplication(config, emailService)

        testAndCancel {
            launch { airlineApplication.run() }

            sleep()

            val management = airlineApplication.managementService
            val display = airlineApplication.airportInformationDisplay(this)

            management.scheduleFlight("111", now(), Plane("A11", setOf()))
            sleep()
            // shouldn't update yet
            Assertions.assertEquals(0, display.value.departing.size)
            sleep()
            Assertions.assertEquals(1, display.value.departing.size)

            management.scheduleFlight("222", now(), Plane("A11", setOf()))
            sleep()
            // shouldn't update yet
            Assertions.assertEquals(1, display.value.departing.size)
            sleep()
            Assertions.assertEquals(2, display.value.departing.size)
        }
    }

    @Test
    fun failedBookingTest() {
        val emailService = InChannelEmailService()
        val config = AirlineConfig(
            displayUpdateInterval = 25.milliseconds,
        )
        val airlineApplication = AirlineApplication(config, emailService)
        val flight1 = Flight(
            "111",
            now() + 1.hours + 3.seconds,
            plane = Plane("A312", setOf("1A", "1B", "2A", "2B")),
        )
        val flight2 = Flight(
            "222",
            now() + 3.hours,
            plane = Plane("B555", setOf("1A", "1B", "2A", "2B")),
        )

        testAndCancel {
            launch { airlineApplication.run() }

            sleep()

            val booking = airlineApplication.bookingService
            val management = airlineApplication.managementService
            val display = airlineApplication.airportInformationDisplay(this)

            management.scheduleFlight(flight1.flightId, flight1.departureTime, flight1.plane)

            // should be ok
            booking.buyTicket(
                flight1.flightId,
                flight1.departureTime,
                "1A",
                "1",
                "Jose Bebos",
                "bebos@gmail.com",
            )
            withTimeout(100.milliseconds) {
                val (email, text) = emailService.messages.receive()
                Assertions.assertEquals("bebos@gmail.com", email)
                Assertions.assertTrue("Jose Bebos" in text)
                Assertions.assertTrue("success" in text)
                Assertions.assertTrue(flight1.flightId in text)
                Assertions.assertTrue("1A" in text)
            }

            // seat occupied
            booking.buyTicket(
                flight1.flightId,
                flight1.departureTime,
                "1A",
                "2",
                "Gonzalo Biba",
                "biba@gmail.com",
            )
            withTimeout(100.milliseconds) {
                val (email, text) = emailService.messages.receive()
                Assertions.assertEquals("biba@gmail.com", email)
                Assertions.assertTrue("Gonzalo Biba" in text)
                Assertions.assertTrue("occupied" in text)
                Assertions.assertTrue(flight1.flightId in text)
                Assertions.assertTrue("1A" in text)
            }

            delay(3.seconds)

            // ticket sales closed
            booking.buyTicket(
                flight1.flightId,
                flight1.departureTime,
                "1B",
                "3",
                "Alice Tratata",
                "tratata@gmail.com",
            )
            withTimeout(100.milliseconds) {
                val (email, text) = emailService.messages.receive()
                Assertions.assertEquals("tratata@gmail.com", email)
                Assertions.assertTrue("Alice Tratata" in text)
                Assertions.assertTrue("ticket sales" in text && "closed" in text)
                Assertions.assertTrue(flight1.flightId in text)
                Assertions.assertTrue("1B" in text)
            }

            management.scheduleFlight(flight2.flightId, flight2.departureTime, flight2.plane)

            sleep()

            Assertions.assertEquals(2, display.value.departing.size)

            management.cancelFlight(flight2.flightId, flight2.departureTime)

            sleep()

            // flight is cancelled
            booking.buyTicket(
                flight2.flightId,
                flight2.departureTime,
                "1A",
                "1",
                "Hugo Gretze",
                "gretze@gmail.com",
            )
            withTimeout(100.milliseconds) {
                val (email, text) = emailService.messages.receive()
                Assertions.assertEquals("gretze@gmail.com", email)
                Assertions.assertTrue("Hugo Gretze" in text)
                Assertions.assertTrue("cancelled" in text)
                Assertions.assertTrue(flight2.flightId in text)
                Assertions.assertTrue("1A" in text)
            }
        }
    }

    @Test
    fun passengerNotificationTest() {
        val emailService = SnailEmailService()
        val config = AirlineConfig(
            displayUpdateInterval = 100.milliseconds,
        )
        val airlineApplication = AirlineApplication(config, emailService)
        val plane = Plane("A312", setOf("1A", "1B", "2A", "2B"))
        val flightId = "111"
        val flightTime = now() + 2.hours

        val tickets = listOf(
            Ticket(
                flightId,
                flightTime,
                "1A",
                "1",
                "Big Boy",
                "big@gmail.com",
            ),
            Ticket(
                flightId,
                flightTime,
                "1B",
                "2",
                "Mid Boy",
                "mid@gmail.com",
            ),
            Ticket(
                flightId,
                flightTime,
                "2A",
                "3",
                "Small Boy",
                "small@gmail.com",
            ),
        )

        testAndCancel {
            launch { airlineApplication.run() }

            sleep()

            val booking = airlineApplication.bookingService
            val management = airlineApplication.managementService

            management.scheduleFlight(flightId, flightTime, plane)

            sleep()

            // sending should be fast
            withTimeout(50.milliseconds) {
                tickets.forEach {
                    booking.buyTicket(
                        it.flightId,
                        it.departureTime,
                        it.seatNo,
                        it.passengerId,
                        it.passengerName,
                        it.passengerEmail,
                    )
                }
            }

            sleep()

            // skip through ticket purchase emails
            repeat(3) {
                emailService.messages.receive()
            }

            // sending should be fast
            withTimeout(50.milliseconds) {
                management.delayFlight(flightId, flightTime, flightTime + 1.hours)
            }

            sleep()

            val emails = (0 until 3).map {
                emailService.messages.receive()
            }
            emails.forEach { (email, text) ->
                val ticket = tickets.find { it.passengerEmail == email }
                Assertions.assertNotNull(ticket)
                Assertions.assertTrue(ticket!!.passengerName in text)
                Assertions.assertTrue("delayed" in text)
                Assertions.assertTrue("${flightTime + 1.hours}" in text)
            }
        }
    }

    @Test
    fun audioAlertsTest() {
        val emailService = InChannelEmailService()
        // configure audio alerts delta which is 3 minutes by default
        // this is done to test consecutive events for a single flight
        // without having to wait for 3 or more minutes
        val config = AirlineConfig(
            audioAlertsInterval = 1.seconds,
            audioAlertsDelta = 1.seconds,
            registrationOpeningTime = 9.seconds,
            registrationClosingTime = 7.seconds,
            boardingOpeningTime = 5.seconds,
            boardingClosingTime = 3.seconds,
        )
        val airlineApplication = AirlineApplication(config, emailService)
        val plane = Plane("A22", setOf())
        val flightId = "111"
        val baseTime = now()
        val flightTime = baseTime + 10.seconds
        val checkin = "checkin1"
        val gate = "gate1"

        testAndCancel {
            launch { airlineApplication.run() }

            sleep()

            val management = airlineApplication.managementService
            val audioAlerts = airlineApplication.airportAudioAlerts

            management.scheduleFlight(flightId, flightTime, plane)
            sleep()
            management.setCheckInNumber(flightId, flightTime, checkin)
            sleep()
            management.setGateNumber(flightId, flightTime, gate)
            sleep()

            val consecutiveAlerts: MutableList<AudioAlerts> = mutableListOf()
            var lastReading = now() - config.audioAlertsInterval

            audioAlerts.take(4).collect { alert ->
                consecutiveAlerts.add(alert)

                // check flow emission frequency
                val now = now()
                Assertions.assertTrue(now - lastReading >= config.audioAlertsInterval)
                lastReading = now

                // alert should be emitted in its respected time interval
                when (alert) {
                    is AudioAlerts.RegistrationOpen -> {
                        Assertions.assertTrue(
                            afterInstant(
                                flightTime - config.registrationOpeningTime,
                                config.audioAlertsDelta,
                            ),
                        )
                    }

                    is AudioAlerts.RegistrationClosing -> {
                        Assertions.assertTrue(
                            beforeInstant(
                                flightTime - config.registrationClosingTime,
                                config.audioAlertsDelta,
                            ),
                        )
                    }

                    is AudioAlerts.BoardingOpened -> {
                        Assertions.assertTrue(
                            afterInstant(
                                flightTime - config.boardingOpeningTime,
                                config.audioAlertsDelta,
                            ),
                        )
                    }

                    is AudioAlerts.BoardingClosing -> {
                        Assertions.assertTrue(
                            beforeInstant(
                                flightTime - config.boardingClosingTime,
                                config.audioAlertsDelta,
                            ),
                        )
                    }
                }
            }

            // alerts should be ordered logically and have expected content
            Assertions.assertEquals(
                listOf(
                    AudioAlerts.RegistrationOpen(flightId, checkin),
                    AudioAlerts.RegistrationClosing(flightId, checkin),
                    AudioAlerts.BoardingOpened(flightId, gate),
                    AudioAlerts.BoardingClosing(flightId, gate),
                ),
                consecutiveAlerts,
            )
        }
    }

    @Test
    fun massMailingOverkill() {
        val emailService = InChannelEmailService()
        val config = AirlineConfig()
        val passengers = 0 until 100
        val airlineApplication = AirlineApplication(config, emailService)
        val plane = Plane("A312", passengers.map { "$it seat" }.toSet())
        val flightId = "111"
        val flightTime = now() + 2.hours

        testAndCancel {
            launch { airlineApplication.run() }

            sleep()

            val booking = airlineApplication.bookingService
            val management = airlineApplication.managementService

            management.scheduleFlight(flightId, flightTime, plane)

            launch {
                passengers.map {
                    booking.buyTicket(
                        flightId,
                        flightTime,
                        "$it seat",
                        it.toString(),
                        "$it name",
                        "$it@gmail.com",
                    )
                }
            }

            sleep()

            val emails = passengers.map {
                emailService.messages.receive()
            }
            emails.forEach { (email, text) ->
                val name = email.split("@").first()
                Assertions.assertTrue(name.toInt() in passengers)
                Assertions.assertTrue(name in text)
                Assertions.assertTrue("success" in text)
            }
        }
    }

    private fun testAndCancel(block: suspend CoroutineScope.() -> Unit) {
        try {
            runBlocking {
                block()
                cancel()
            }
        } catch (timeout: TimeoutCancellationException) {
            throw timeout
        } catch (ignore: CancellationException) {
        }
    }

    private suspend fun sleep() {
        delay(50.milliseconds)
    }

    private class InChannelEmailService : EmailService {
        val messages = Channel<Pair<String, String>>()

        override suspend fun send(to: String, text: String) {
            messages.send(to to text)
        }
    }

    // very slow email service
    private class SnailEmailService : EmailService {
        val messages = Channel<Pair<String, String>>()

        override suspend fun send(to: String, text: String) {
            delay(1.seconds)
            messages.send(to to text)
        }
    }
}
