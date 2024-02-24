package chatbot.dsl

import chatbot.api.*

@ChatBotDSL
class MessageProcessorContext<C : ChatContext?>(
    val message: Message,
    val client: Client,
    val context: C,
    val setContext: (c: ChatContext?) -> Unit,
) {
    private fun checkKeyboardAbsentOrEmpty(keyboard: Keyboard?): Boolean {
        return when (keyboard) {
            null -> true
            is Keyboard.Remove -> false
            is Keyboard.Markup -> keyboard.keyboard.all { it.isEmpty() }
        }
    }

    fun sendMessage(chatId: ChatId, configure: MessageBuilder.() -> Unit) {
        val builder = MessageBuilder(message)
        configure(builder)
        val config = builder.build()
        if (config.text.isBlank()) {
            if (checkKeyboardAbsentOrEmpty(config.keyboard)) return
        }
        client.sendMessage(
            chatId,
            config.text,
            config.keyboard,
            config.replyTo,
        )
    }
}

typealias MessageProcessor<C> = MessageProcessorContext<C>.() -> Unit

typealias MessageProcessorPredicate = ChatBot.(Message) -> Boolean

@ChatBotDSL
class MessageBuilder(val message: Message) {
    var text: String = ""
    var replyTo: MessageId? = null

    private var removeKeyboard = false
    fun removeKeyboard() {
        removeKeyboard = true
    }

    private var keyboardConfig: KeyboardConfig? = null

    private fun buildKeyboard(config: KeyboardConfig?): Keyboard? {
        if (removeKeyboard) return Keyboard.Remove
        if (config == null) return null
        val keyMatrix = config.keyboard.map { it.toList() }
        return Keyboard.Markup(
            oneTime = config.oneTime,
            keyboard = keyMatrix,
        )
    }

    fun withKeyboard(configure: KeyboardBuilder.() -> Unit) {
        removeKeyboard = false
        val builder = KeyboardBuilder()
        configure(builder)
        keyboardConfig = builder.build()
    }

    fun build(): MessageConfig {
        return MessageConfig(
            text = text,
            keyboard = buildKeyboard(keyboardConfig),
            replyTo = replyTo,
        )
    }
}

@ChatBotDSL
class KeyboardBuilder {
    var oneTime: Boolean = false

    var keyboard: MutableList<MutableList<Keyboard.Button>> = mutableListOf()

    fun row(configure: RowBuilder.() -> Unit) {
        val builder = RowBuilder()
        configure(builder)
        keyboard += builder.row
    }

    fun build(): KeyboardConfig {
        return KeyboardConfig(
            oneTime,
            keyboard,
        )
    }
}

@ChatBotDSL
class RowBuilder {
    var row: MutableList<Keyboard.Button> = mutableListOf()
    fun button(text: String) {
        row += Keyboard.Button(text)
    }

    operator fun String.unaryMinus() {
        row += Keyboard.Button(this)
    }
}

data class MessageConfig(
    internal val text: String,
    internal val keyboard: Keyboard?,
    internal val replyTo: MessageId?,
)

data class KeyboardConfig(
    internal val oneTime: Boolean,
    internal val keyboard: MutableList<MutableList<Keyboard.Button>>,
)
