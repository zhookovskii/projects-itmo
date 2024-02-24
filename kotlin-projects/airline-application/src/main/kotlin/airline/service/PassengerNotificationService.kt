package airline.service

import airline.api.SendEmail
import kotlinx.coroutines.flow.Flow
import kotlinx.datetime.Instant

interface PassengerNotificationService {

    /**
     * Flow of emails to send
     */
    val emailFlow: Flow<SendEmail>

    /**
     * Emit new email to the email flow
     */
    suspend fun send(flightId: String, departureTime: Instant, emailBuilder: (String) -> String)

    /**
     * Launch email flow processor
     */
    suspend fun run()
}
