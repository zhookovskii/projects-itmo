package chatbot.dsl

import chatbot.api.*

private val initialPredicatePrefix: MessageProcessorPredicate = { true }
private val initialContextChecker: (ChatContext?) -> Boolean = { true }

@DslMarker
annotation class ChatBotDSL

data class PredicateHandler<C : ChatContext?>(
    val contextChecker: (ChatContext?) -> Boolean,
    val predicate: ChatBot.(Message) -> Boolean,
    val onTrue: MessageProcessor<C>,
)

data class BehaviourConfig<C : ChatContext?>(
    internal val predicateHandlers: List<PredicateHandler<C>>,
)

data class ChatConfig(
    internal val behaviour: BehaviourConfig<ChatContext?>,
    internal val logLevel: LogLevel,
)

@ChatBotDSL
class BehaviourBuilder<C : ChatContext?>(
    val contextChecker: (ChatContext?) -> Boolean,
    val predicatePrefix: MessageProcessorPredicate,
) {

    val predicateHandlers: MutableList<PredicateHandler<C>> =
        mutableListOf()

    private fun checkCommandMatch(message: Message, command: String): Boolean {
        val text = message.text
        if (text.isNotBlank() && text[0] == '/') {
            return command == message.text.split(' ', limit = 2)[0].substring(1)
        }
        return false
    }

    fun onCommand(command: String, call: MessageProcessor<C>) {
        return onMessage(
            predicate = { msg -> checkCommandMatch(msg, command) && predicatePrefix(msg) },
            call = call,
        )
    }

    fun onMessage(messageTextExactly: String, call: MessageProcessor<C>) {
        return onMessage(
            predicate = { msg -> messageTextExactly == msg.text && predicatePrefix(msg) },
            call = call,
        )
    }

    fun onMessagePrefix(prefix: String, call: MessageProcessor<C>) {
        return onMessage(
            predicate = { msg -> msg.text.startsWith(prefix) && predicatePrefix(msg) },
            call = call,
        )
    }

    fun onMessageContains(text: String, call: MessageProcessor<C>) {
        return onMessage(
            predicate = { msg -> msg.text.contains(text) && predicatePrefix(msg) },
            call = call,
        )
    }

    fun onMessage(call: MessageProcessor<C>) {
        return onMessage(
            predicate = { msg -> predicatePrefix(msg) },
            call = call,
        )
    }

    fun onMessage(predicate: ChatBot.(Message) -> Boolean, call: MessageProcessor<C>) {
        predicateHandlers.add(
            PredicateHandler(
                contextChecker,
                { msg -> predicate(msg) && predicatePrefix(msg) },
                onTrue = call,
            ),
        )
    }

    fun build(): BehaviourConfig<C> {
        return BehaviourConfig(predicateHandlers)
    }

    inline infix fun <reified T : ChatContext?> into(configure: BehaviourBuilder<T>.() -> Unit) {
        val builder = BehaviourBuilder<T>(
            contextChecker = { chatContext -> chatContext is T },
            predicatePrefix = predicatePrefix,
        )
        configure(builder)
        predicateHandlers.addAll(
            builder.predicateHandlers.map {
                PredicateHandler(it.contextChecker, it.predicate) {
                    it.onTrue(MessageProcessorContext(this.message, this.client, this.context as T, this.setContext))
                }
            },
        )
    }

    inline infix fun <reified T : ChatContext?> T.into(configure: BehaviourBuilder<T>.() -> Unit) {
        val builder = BehaviourBuilder<T>(
            contextChecker = { chatContext -> chatContext == this@into && chatContext is T },
            predicatePrefix = predicatePrefix,
        )
        configure(builder)
        predicateHandlers.addAll(
            builder.predicateHandlers.map {
                PredicateHandler(it.contextChecker, it.predicate) {
                    it.onTrue(MessageProcessorContext(this.message, this.client, this.context as T, this.setContext))
                }
            },
        )
    }

    /*
     * BONUS
     * add all `predicateHandlers` from child behaviours to `this.predicateHandlers`
     * child behaviours will accumulate all processor predicates via `predicatePrefix`
     * which is passed to them by their parent behaviour
     */
    fun MessageProcessorPredicate.into(configure: BehaviourBuilder<C>.() -> Unit) {
        val builder = BehaviourBuilder<C>(
            contextChecker = contextChecker,
            predicatePrefix = { msg -> this@into(msg) && predicatePrefix(msg) },
        )
        configure(builder)
        val config = builder.build()
        predicateHandlers.addAll(config.predicateHandlers)
    }

    operator fun MessageProcessorPredicate.times(
        other: MessageProcessorPredicate,
    ): MessageProcessorPredicate {
        return { msg -> this@times(msg) && other(msg) }
    }
}

@ChatBotDSL
class ChatBuilder {
    private var behaviourConfig: BehaviourConfig<ChatContext?> = BehaviourConfig(
        predicateHandlers = emptyList(),
    )

    private var logLevel: LogLevel = LogLevel.ERROR

    var contextManager: ChatContextsManager? = null

    fun use(logLevel: LogLevel) {
        this.logLevel = logLevel
    }

    fun use(contextsManager: ChatContextsManager) {
        this.contextManager = contextsManager
    }

    operator fun LogLevel.unaryPlus() {
        this@ChatBuilder.use(this)
    }

    fun behaviour(configure: BehaviourBuilder<ChatContext?>.() -> Unit) {
        val behaviourBuilder = BehaviourBuilder<ChatContext?>(
            contextChecker = initialContextChecker,
            predicatePrefix = initialPredicatePrefix,
        )
        configure(behaviourBuilder)
        behaviourConfig = behaviourBuilder.build()
    }

    fun build(): ChatConfig {
        return ChatConfig(
            behaviour = behaviourConfig,
            logLevel = logLevel,
        )
    }
}

fun chatBot(client: Client, configure: ChatBuilder.() -> Unit): ChatBot {
    val chatBuilder = ChatBuilder()
    configure(chatBuilder)
    val chatConfig = chatBuilder.build()
    return object : ChatBot {
        override fun processMessages(message: Message) {
            val chatContext = chatBuilder.contextManager?.getContext(message.chatId)
            val processorContext = MessageProcessorContext(
                message,
                client,
                chatContext,
            ) { context -> chatBuilder.contextManager?.setContext(message.chatId, context) }
            val behaviour = chatConfig.behaviour
            produceResponse(message, behaviour, processorContext)
        }

        override val logLevel: LogLevel = chatConfig.logLevel
    }
}

private fun ChatBot.produceResponse(
    message: Message,
    behaviour: BehaviourConfig<ChatContext?>,
    processorContext: MessageProcessorContext<ChatContext?>,
): Boolean {
    behaviour.predicateHandlers.forEach { handler ->
        if (handler.contextChecker(processorContext.context) && handler.predicate(this, message)) {
            handler.onTrue(processorContext)
            return true
        }
    }
    return false
}
